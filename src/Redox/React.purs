module React.Redox
  ( withStore
  , connect'
  , connect
  , dispatch
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Canceler)
import ReactHocs (CONTEXT, withContext, accessContext, readContext) 
import React as R
import React (ReactClass, ReactSpec, ReactThis)
import Redox as Redox
import Redox (ReadRedox, ReadWriteSubscribeRedox, Store)
import Type.Proxy (Proxy(..))

withStore
  :: forall state props dsl eff
  -- redox store
   . Store state
  -- bound redox dispatch function
  -> (dsl -> Eff (ReadWriteSubscribeRedox eff) (Canceler (ReadWriteSubscribeRedox eff)))
  -> ReactClass props
  -> ReactClass props
withStore store dispatch cls =
  withContext cls { store, dispatch }

-- | You must wrap the resulting component with `ReactHocs.accessContext` from
-- | `purescript-react-hocs`.  Checkout `connect` bellow.
connect'
  :: forall state props props' eff
   . (state -> props' -> props)
  -> ReactClass props
  -> ReactSpec props' Unit ( context :: CONTEXT, readRedox :: ReadRedox | eff )
connect' cnstrProps cls = R.spec unit renderFn
  where
    renderFn this = do
      props' <- R.getProps this
      state <- readContext (Proxy :: Proxy { store :: Store state }) this >>= Redox.getState <<< _.store
      pure $ R.createElement cls (cnstrProps state props') []

connect
  :: forall state props props'
   . (state -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
connect cnstProps cls = accessContext $ R.createClass $ connect' cnstProps cls

type Dispatcher dsl eff = dsl -> Eff (ReadWriteSubscribeRedox eff) (Canceler (ReadWriteSubscribeRedox eff))

dispatch
  :: forall dsl props state eff
   . ReactThis props state
  -> dsl
  -> Eff (ReadWriteSubscribeRedox (context :: CONTEXT | eff)) (Canceler (ReadWriteSubscribeRedox (context :: CONTEXT | eff)))
dispatch this dsl = do
  _dispatch <- readContext (Proxy :: Proxy { dispatch :: Dispatcher dsl (context :: CONTEXT | eff)}) this >>= pure <<< _.dispatch
  _dispatch dsl
