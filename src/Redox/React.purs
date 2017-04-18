module React.Redox
  ( withStore
  , connect'
  , connect
  , dispatch
  , RedoxContext
  ) where

import Prelude
import React as R
import Redox as Redox
import Control.Monad.Aff (Canceler)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Lens (Lens', view)
import Data.Maybe (Maybe(..))
import React (ReactClass, ReactSpec, ReactThis)
import ReactHocs (CONTEXT, withContext, accessContext, readContext, getDisplayName)
import Redox (ReadRedox, SubscribeRedox, ReadWriteSubscribeRedox, Store, SubscriptionId)
import Type.Proxy (Proxy(..))

-- | You need to wrap your most top-level component with `withStore`.  It makes
-- | the store and the bound dispatch function avaialble through React context.
-- | Then you can connect a component with `connect` (or `connect'`) and get
-- | access to the store and the dispatch function.
withStore
  :: forall state props dsl eff
  -- redox store
   . Store state
  -- bound redox dispatch function
  -> (Store state -> dsl -> Eff (ReadWriteSubscribeRedox eff) (Canceler (ReadWriteSubscribeRedox eff)))
  -> ReactClass props
  -> ReactClass props
withStore store dispatch_ cls =
  withContext cls { redox: { store, dispatch: dispatch_ store } }

type RedoxContext state dsl eff =
  { redox ::
    { store :: Store state
    , dispatch :: dsl -> Eff (ReadWriteSubscribeRedox eff) (Canceler (ReadWriteSubscribeRedox eff))
    }
  }

type RedoxStoreContext state = { redox :: { store :: Store state } }

type ConnectState state = { state :: state, sid :: Maybe SubscriptionId }

_connect
  :: forall state state' props props' eff
   . (ReactThis props' (ConnectState state')
      -> Eff (context :: CONTEXT, readRedox :: ReadRedox, subscribeRedox :: SubscribeRedox | eff) (Store state))
  -> Lens' state state'
  -> (state' -> props' -> props)
  -> ReactClass props
  -> ReactSpec props' (ConnectState state') ( context :: CONTEXT, readRedox :: ReadRedox, subscribeRedox :: SubscribeRedox | eff )
_connect storeEff _lns _iso cls = (R.spec' getInitialState renderFn)
    { displayName = getDisplayName cls <> "Connect"
    , componentWillMount = componentWillMount
    , componentWillUnmount = componentWillUnmount
    }
  where

    update this state = R.transformState this (_ { state = view _lns state })

    getInitialState this = do
      store <- addReactDisallowedEff $ storeEff this
      state <- Redox.getState store
      pure { state: view _lns state, sid: Nothing }

    componentWillMount this = do
      store <- addReactEff $ storeEff this
      sid <- Redox.subscribe store $ update this
      R.transformState this (_ { sid = Just sid })

    componentWillUnmount this = do
      store <- addReactReadOnlyEff $ storeEff this
      msid <- (R.readState this) >>= pure <<< _.sid
      case msid of
        Nothing -> pure unit
        Just sid -> Redox.unsubscribe store sid

    renderFn this = do
      props' <- R.getProps this
      state <- R.readState this >>= pure <<< _.state
      pure $ R.createElement cls (_iso state props') []

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
-- | ReactHocs.readContext this >>= pure <<< _.redox :: Eff eff (RedoxContext state dsl eff)
-- | ```
connect'
  :: forall state state' props props' eff
   . Lens' state state'
  -> (state' -> props' -> props)
  -> ReactClass props
  -> ReactSpec props' (ConnectState state') ( context :: CONTEXT, readRedox :: ReadRedox, subscribeRedox :: SubscribeRedox | eff )
connect' _lns _iso cls = _connect storeEff _lns _iso cls
  where
    storeEff this = readContext (Proxy :: Proxy (RedoxStoreContext state)) this >>= pure <<< _.redox.store

connect
  :: forall state state' props props'
   . Lens' state state'
  -> (state' -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
connect _lns _iso cls = accessContext $ R.createClass $ connect' _lns _iso cls

dispatch
  :: forall dsl props state eff
   . ReactThis props state
  -> dsl
  -> Eff (ReadWriteSubscribeRedox (context :: CONTEXT | eff)) (Canceler (ReadWriteSubscribeRedox (context :: CONTEXT | eff)))
dispatch this dsl = do
  _dispatch <- readContext (Proxy :: Proxy (RedoxContext state dsl (context :: CONTEXT | eff)) ) this >>= pure <<< _.redox.dispatch
  _dispatch dsl
