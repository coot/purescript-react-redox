module React.Redox
  ( ConnectState
  , withStore
  , connect'
  , connect
  , withDispatch
  , dispatch
  , RedoxContext
  , DispatchFn
  , unsafeShallowEqual
  , unsafeStrictEqual
  ) where

import Prelude

import Control.Monad.Aff (Canceler)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Free (Free)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn2, runFn3)
import Data.Lens (Getter', view)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, over)
import React (ReactClass, ReactElement, ReactSpec, ReactThis, createElement, getProps, readState, writeState)
import React as R
import ReactHocs (CONTEXT, withContext, accessContext, readContext, getDisplayName)
import Redox as Redox
import Redox.Store (ReadRedox, RedoxStore, Store, SubscribeRedox, SubscriptionId)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type DispatchFn state dsl reff eff = Free dsl (state -> state) -> Eff (redox :: RedoxStore reff | eff) (Canceler (redox :: RedoxStore reff | eff))

-- | Shallowly compare to objects.  If the third argument is true it skips
-- | comapring the `key` property which should not be accessed on a property
-- | object.
foreign import unsafeShallowEqual :: forall a. Fn3 Boolean a a Boolean

-- | Compare two objects using strict equality `===`
foreign import unsafeStrictEqual :: forall a. Fn2 a a Boolean

foreign import writeIsMountedImpl
  :: forall props state e
   . EffFn2 e (ReactThis props state) Boolean Unit

writeIsMounted :: forall props state e. ReactThis props state -> Boolean -> Eff e Unit
writeIsMounted this isMounted = runEffFn2 writeIsMountedImpl this isMounted

foreign import readIsMountedImpl
  :: forall props state e
   . EffFn1 e (ReactThis props state) Boolean

readIsMounted :: forall props state e. ReactThis props state -> Eff e Boolean
readIsMounted this = runEffFn1 readIsMountedImpl this

foreign import forceUpdateImpl :: forall eff props state.
  EffFn1 eff (ReactThis props state) Unit

-- | Force render of a react component.
forceUpdate :: forall eff props state.
  ReactThis props state -> Eff eff Unit
forceUpdate this = runEffFn1 forceUpdateImpl this

createClassStatelessWithContext
  :: forall props ctx
   . (props -> ctx -> ReactElement)
  -> ReactClass props
createClassStatelessWithContext fn = unsafeCoerce f
  where
    f :: Fn2 props { ctx :: ctx } ReactElement
    f = mkFn2 (\a { ctx } -> fn a ctx)

newtype RedoxContext state dsl reff eff = RedoxContext
  { store :: Store state
  , dispatch :: DispatchFn state dsl reff eff
  }

-- | You need to wrap your most top-level component with `withStore`.  It makes
-- | the store and the bound dispatch function avaialble through React context.
-- | Then you can connect a component with `connect` (or `connect'`) and get
-- | access to the store and the dispatch function.
withStore
  :: forall state props dsl reff eff
  -- redox store
   . Store state
  -- bound redox dispatch function
  -> (Store state -> DispatchFn state dsl reff eff)
  -> ReactClass props
  -> ReactClass props
withStore store dispatch_ cls =
  withContext cls { redox: RedoxContext { store, dispatch: dispatch_ store } }

newtype ConnectState state = ConnectState { state :: state, sid :: Maybe SubscriptionId }

derive instance newtypeConnectState :: Newtype (ConnectState state) _

_connect
  :: forall state state' dsl props props' reff eff
   . (ReactThis props' (ConnectState state')
      -> Eff
        (context :: CONTEXT, redox :: RedoxStore (read :: ReadRedox, subscribe :: SubscribeRedox | reff) | eff)
        (RedoxContext state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff)
     )
  -> Getter' state state'
  -> ((DispatchFn state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff) -> state' -> props' -> props)
  -> ReactClass props
  -> ReactSpec props' (ConnectState state')
      ( context :: CONTEXT
      , redox :: RedoxStore (read :: ReadRedox, subscribe :: SubscribeRedox | reff)
      | eff
      )
_connect ctxEff _lns _iso cls = (R.spec' getInitialState renderFn)
    { displayName = getDisplayName cls <> "Connect"
    , componentDidMount = componentDidMount
    , componentWillUnmount = componentWillUnmount
    , shouldComponentUpdate = shouldComponentUpdate
    }
  where

    -- Connect the child component to the context, this is required for the
    -- `dispatch` function.
    cls_ :: ReactClass props
    cls_ = accessContext cls

    update this state = do
      st <- readState this
      isMounted <- readIsMounted this
      when isMounted do
        void $ writeState this $ over ConnectState (_ { state = view _lns state }) st

    getInitialState this = do
      -- unsafeCoerceEff is used to add react effects to ctxEff
      RedoxContext ctx <- unsafeCoerceEff $ ctxEff this
      state <- Redox.getState ctx.store
      pure (ConnectState { state: view _lns state, sid: Nothing })

    -- | Subscription to redox store happens in `componentDidMount`, rather than
    -- | on `componentWillMount`.  This is because `componentWillUnmount` and
    -- | `componentDidMount` do not fire in SSR. Otherwise we'd have a memory
    -- | leak.
    componentDidMount this = do
      writeIsMounted this true
      RedoxContext { store } <- unsafeCoerceEff $ ctxEff this
      st@ConnectState { state } <- readState this
      sid <- Redox.subscribe store $ update this
      _ <- writeState this (over ConnectState (_ { sid = Just sid }) st)
      state' <- view _lns <$> Redox.getState store
      when
        (runFn2 unsafeStrictEqual state state')
        (forceUpdate this)

    componentWillUnmount this = do
      writeIsMounted this false
      RedoxContext ctx <- unsafeCoerceEff $ ctxEff this
      ConnectState { sid: msid } <- R.readState this
      case msid of
        Nothing -> pure unit
        Just sid -> Redox.unsubscribe ctx.store sid

    shouldComponentUpdate this nPr (ConnectState nSt) = do
      pr <- getProps this
      ConnectState st <- readState this
      -- Take care only of `st.state` changes, `st.sid` is not used for
      -- rendering.
      pure $ not
         $ (runFn2 unsafeStrictEqual st.state nSt.state)
        && (runFn3 unsafeShallowEqual false pr nPr)

    renderFn this = do
      props' <- R.getProps this
      children <- R.getChildren this
      RedoxContext ctx <- unsafeCoerceEff $ ctxEff this
      ConnectState { state } <- R.readState this
      pure $ R.createElement cls_ (_iso ctx.dispatch state props') children

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
  :: forall state state' dsl props props' reff eff
   . Proxy state
  -> Getter' state state'
  -> (DispatchFn state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff -> state' -> props' -> props)
  -> ReactClass props
  -> ReactSpec props' (ConnectState state') ( context :: CONTEXT, redox :: RedoxStore (read :: ReadRedox, subscribe :: SubscribeRedox | reff) | eff )
connect' _ _lns _iso cls = _connect ctxEff _lns _iso cls
  where
    proxy :: Proxy ({ redox :: RedoxContext state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff })
    proxy = Proxy

    ctxEff this = _.redox <$> readContext proxy this

connect
  :: forall state state' dsl props props' reff eff'
   . Proxy state
  -> Getter' state state'
  -> (DispatchFn state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff' -> state' -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
connect p _lns _iso cls = accessContext $ R.createClass $ connect' p _lns _iso cls

withDispatch
  :: forall state props props' dsl reff eff'
   . (DispatchFn state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff' -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
withDispatch fn cls = accessContext $ createClassStatelessWithContext
  \props' { redox: RedoxContext { dispatch: disp }}
  -- todo: childrenToArray
  -> createElement cls (fn disp props') (unsafeCoerce props').children

-- | Light wieght version of `connect`.  It does not require wrapping a parent
-- | with `withStore` since it is not using react context to access the store.
-- | You just need to provide the store with your first argument.
-- |
-- | Note: it is not safe to do server side rendering and keep the store as
-- | a global reference.  However, you can safely use this method if you are
-- | passing the store explicitely through props.
connectStore
  :: forall state state' dsl props props' reff eff
   . Store state
  -> DispatchFn state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff
  -> Getter' state state'
  -> (DispatchFn state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff -> state' -> props' -> props)
  -> ReactClass props
  -> ReactSpec props' (ConnectState state') ( context :: CONTEXT, redox :: RedoxStore (read :: ReadRedox, subscribe :: SubscribeRedox | reff) | eff )
connectStore store dispatch_ _lns _iso cls = _connect (const $ pure (RedoxContext {store, dispatch: dispatch_})) _lns _iso cls

dispatch
  :: forall dsl rProps rState state reff eff
   . ReactThis rProps rState
  -> Free dsl (state -> state)
  -> Eff (context :: CONTEXT, redox :: RedoxStore reff | eff) (Canceler (context :: CONTEXT, redox :: RedoxStore reff | eff))
dispatch this dsl =
  let proxy :: Proxy ({ redox :: RedoxContext state dsl reff (context :: CONTEXT | eff) }) 
      proxy = Proxy
  in do
    { redox: RedoxContext { dispatch: disp } } <- readContext proxy this
    disp dsl
