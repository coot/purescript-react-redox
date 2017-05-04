module React.Redox
  ( withStore
  , connect'
  , connect
  , dispatch
  , RedoxContext
  , DispatchFn
  , unsafeShallowEqual
  , unsafeStrictEqual
  ) where

import Prelude
import React as R
import Redox as Redox
import Control.Monad.Aff (Canceler)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Free (Free)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Lens (Getter', view)
import Data.Maybe (Maybe(..))
import React (ReactClass, ReactSpec, ReactThis, getProps, readState)
import ReactHocs (CONTEXT, withContext, accessContext, readContext, getDisplayName)
import Redox (ReadRedox, SubscribeRedox, ReadWriteSubscribeRedox, Store, SubscriptionId)
import Type.Proxy (Proxy(..))

type DispatchFn state dsl eff = Free dsl (state -> state) -> Eff (ReadWriteSubscribeRedox eff) (Canceler (ReadWriteSubscribeRedox eff))

foreign import unsafeShallowEqual :: forall a. Fn2 a a Boolean

foreign import unsafeStrictEqual :: forall a. Fn2 a a Boolean

-- | You need to wrap your most top-level component with `withStore`.  It makes
-- | the store and the bound dispatch function avaialble through React context.
-- | Then you can connect a component with `connect` (or `connect'`) and get
-- | access to the store and the dispatch function.
withStore
  :: forall state props dsl eff
  -- redox store
   . Store state
  -- bound redox dispatch function
  -> (Store state -> DispatchFn state dsl eff)
  -> ReactClass props
  -> ReactClass props
withStore store dispatch_ cls =
  withContext cls { redox: { store, dispatch: dispatch_ store } }

type RedoxContext state dsl eff =
  { store :: Store state
  , dispatch :: DispatchFn state dsl eff
  }

type ConnectState state = { state :: state, sid :: Maybe SubscriptionId }

_connect
  :: forall state state' dsl props props' eff
   . (ReactThis props' (ConnectState state')
      -> Eff (context :: CONTEXT, readRedox :: ReadRedox, subscribeRedox :: SubscribeRedox | eff) (RedoxContext state dsl eff))
  -> Getter' state state'
  -> ((DispatchFn state dsl eff) -> state' -> props' -> props)
  -> ReactClass props
  -> ReactSpec props' (ConnectState state') ( context :: CONTEXT, readRedox :: ReadRedox, subscribeRedox :: SubscribeRedox | eff )
_connect ctxEff _lns _iso cls = (R.spec' getInitialState renderFn)
    { displayName = getDisplayName cls <> "Connect"
    , componentWillMount = componentWillMount
    , componentWillUnmount = componentWillUnmount
    , shouldComponentUpdate = shouldComponentUpdate
    }
  where

    -- Connect the child component to the context, this is required for the
    -- `dispatch` function.
    cls_ :: ReactClass props
    cls_ = accessContext cls

    update this state = R.transformState this (_ { state = view _lns state })

    getInitialState this = do
      ctx <- addReactDisallowedEff $ ctxEff this
      state <- Redox.getState ctx.store
      pure { state: view _lns state, sid: Nothing }

    componentWillMount this = do
      ctx <- addReactEff $ ctxEff this
      sid <- Redox.subscribe ctx.store $ update this
      R.transformState this (_ { sid = Just sid })

    componentWillUnmount this = do
      ctx <- addReactReadOnlyEff $ ctxEff this
      msid <- (R.readState this) >>= pure <<< _.sid
      case msid of
        Nothing -> pure unit
        Just sid -> Redox.unsubscribe ctx.store sid

    shouldComponentUpdate this nPr nSt = do
      pr <- getProps this
      st <- readState this
      -- Take care only of `st.state` changes, `st.sid` is not used for
      -- rendering.
      pure $ not
         $ (runFn2 unsafeStrictEqual st.state nSt.state)
        && (runFn2 unsafeShallowEqual pr nPr)

    renderFn this = do
      props' <- R.getProps this
      ctx <- unsafeCoerceEff $ ctxEff this
      state <- unsafeCoerceEff $ R.readState this >>= pure <<< _.state
      pure $ R.createElement cls_ (_iso ctx.dispatch state props') []

    addReactEff :: forall a e. Eff e a -> Eff ( props :: R.ReactProps, state :: R.ReactState R.ReadWrite, refs :: R.ReactRefs (() :: # Effect) | e ) a
    addReactEff = unsafeCoerceEff

    addReactDisallowedEff :: forall a e. Eff e a -> Eff ( props :: R.ReactProps, state :: R.ReactState R.Disallowed, refs :: R.ReactRefs R.Disallowed | e ) a
    addReactDisallowedEff = unsafeCoerceEff

    addReactReadOnlyEff :: forall a e. Eff e a -> Eff ( props :: R.ReactProps, state :: R.ReactState R.ReadOnly, refs :: R.ReactRefs R.ReadOnly | e) a
    addReactReadOnlyEff = unsafeCoerceEff

-- | You must wrap the resulting component with `ReactHocs.accessContext` from
-- | `purescript-react-hocs`.  Checkout `connect` bellow.  This function makes
-- | the redox store and dispatch function available through the context.
-- | The first argument is a `Lens` that identifies the part of the store state
-- | that you want to subscribe for.
-- | The second argument let you combine state and additional properties
-- | `props'` to get props of the class that you are connecting to the store.
-- | You can read the context with:
-- | ```purescript
-- | ReactHocs.readContext this >>= pure <<< _.redox :: Eff eff (RedoxContext state (Free dsl (state -> state))  eff)
-- | ```
connect'
  :: forall state state' dsl props props' eff
   . Getter' state state'
  -> (DispatchFn state dsl eff -> state' -> props' -> props)
  -> ReactClass props
  -> ReactSpec props' (ConnectState state') ( context :: CONTEXT, readRedox :: ReadRedox, subscribeRedox :: SubscribeRedox | eff )
connect' _lns _iso cls = _connect ctxEff _lns _iso cls
  where
    ctxEff this = _.redox <$> ctx
      where
        ctx = readContext (Proxy :: Proxy ({ redox :: RedoxContext state dsl eff })) this

connect
  :: forall state state' dsl props props' eff'
   . Getter' state state'
  -> (DispatchFn state dsl eff' -> state' -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
connect _lns _iso cls = accessContext $ R.createClass $ connect' _lns _iso cls

-- | Light wieght version of `connect`.  It does not require wrapping a parent
-- | with `withStore` since it is not using react context to access the store.
-- | You just need to provide the store with your first argument.
-- |
-- | Note: it is not safe to do server side rendering and keep the store as
-- | a global reference.  However, you can safely use this method if you are
-- | passing the store explicitely through props.
connectStore
  :: forall state state' dsl props props' eff
   . Store state
  -> DispatchFn state dsl eff
  -> Getter' state state'
  -> (DispatchFn state dsl eff -> state' -> props' -> props)
  -> ReactClass props
  -> ReactSpec props' (ConnectState state') ( context :: CONTEXT, readRedox :: ReadRedox, subscribeRedox :: SubscribeRedox | eff )
connectStore store dispatch_ _lns _iso cls = _connect (const $ pure {store, dispatch: dispatch_}) _lns _iso cls

dispatch
  :: forall dsl rProps rState state eff
   . ReactThis rProps rState
  -> Free dsl (state -> state)
  -> Eff (ReadWriteSubscribeRedox (context :: CONTEXT | eff)) (Canceler (ReadWriteSubscribeRedox (context :: CONTEXT | eff)))
dispatch this dsl = do
  _dispatch <- _.redox.dispatch <$> readContext (Proxy :: Proxy ({ redox :: RedoxContext state dsl (context :: CONTEXT | eff) }) ) this
  _dispatch dsl
