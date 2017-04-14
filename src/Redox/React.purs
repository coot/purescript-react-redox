module React.Redox
  ( withStore
  , connect'
  , connect
  , dispatch
  , RedoxContext
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Canceler)
import Data.Maybe (Maybe(..))
import Data.Lens (Lens', view)
import ReactHocs (CONTEXT, withContext, accessContext, readContext, getDisplayName) 
import React as R
import React (ReactClass, ReactSpec, ReactThis)
import Redox as Redox
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
  -> (dsl -> Eff (ReadWriteSubscribeRedox eff) (Canceler (ReadWriteSubscribeRedox eff)))
  -> ReactClass props
  -> ReactClass props
withStore store dispatch cls =
  withContext cls { redox: {store, dispatch} }


type RedoxContext state dsl eff =
  { redox ::
    { store :: Store state
    , dispatch :: dsl -> Eff (ReadWriteSubscribeRedox eff) (Canceler (ReadWriteSubscribeRedox eff))
    }
  }

type RedoxStoreContext state = { redox :: { store :: Store state } }

type ConnectState state = { state :: state, sid :: Maybe SubscriptionId }

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
connect' lns cnstrProps cls = (R.spec' getInitialState renderFn)
    { displayName = getDisplayName cls <> "Connect"
    , componentWillMount = componentWillMount
    , componentWillUnmount = componentWillUnmount
    }
  where

    update this state = R.transformState this (_ { state = view lns state })

    getInitialState this = do
      store <- readContext (Proxy :: Proxy (RedoxStoreContext state)) this >>= pure <<< _.redox.store
      state <- Redox.getState store
      pure { state: view lns state, sid: Nothing }

    componentWillMount this = do
      store <- readContext (Proxy :: Proxy (RedoxStoreContext state)) this >>= pure <<< _.redox.store
      sid <- Redox.subscribe store $ update this
      R.transformState this (_ { sid = Just sid })

    componentWillUnmount this = do
      store <- readContext (Proxy :: Proxy (RedoxStoreContext state)) this >>= pure <<< _.redox.store
      msid <- R.readState this >>= pure <<< _.sid
      case msid of
        Nothing -> pure unit
        Just sid -> Redox.unsubscribe store sid

    renderFn this = do
      props' <- R.getProps this
      state <- R.readState this >>= pure <<< _.state
      pure $ R.createElement cls (cnstrProps state props') []

connect
  :: forall state state' props props'
   . Lens' state state'
  -> (state' -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
connect lns cnstProps cls = accessContext $ R.createClass $ connect' lns cnstProps cls

dispatch
  :: forall dsl props state eff
   . ReactThis props state
  -> dsl
  -> Eff (ReadWriteSubscribeRedox (context :: CONTEXT | eff)) (Canceler (ReadWriteSubscribeRedox (context :: CONTEXT | eff)))
dispatch this dsl = do
  _dispatch <- readContext (Proxy :: Proxy (RedoxContext state dsl (context :: CONTEXT | eff)) ) this >>= pure <<< _.redox.dispatch
  _dispatch dsl
