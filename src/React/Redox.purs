module React.Redox
  ( ConnectState(..)
  , DispatchFn
  , DispatchFnFiber
  , RedoxContext
  , RedoxSpec
  , withStore
  , StoreProvider(..)
  , storeProvider
  , connect'
  , connectEq'
  , connect
  , connectEq
  , connectV'
  , connectEqV'
  , connectV
  , connectEqV
  , withDispatch
  , withDispatchV
  , dispatch
  , dispatchV
  , asReactClass
  , overRedoxSpec
  , unsafeShallowEqual
  , unsafeShallowEqual'
  ) where

import Prelude

import Control.Monad.Aff (Fiber)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Free (Free)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn3)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, over)
import Data.Traversable (traverse_)
import React (ReactClass, ReactElement, ReactSpec, ReactThis, childrenToArray, createClass, createClassStateless', createElement, getProps, readState, writeState)
import React as R
import React.DOM (div')
import ReactHocs (CONTEXT, accessContext, getDisplayName, readContext, withContext, withContext')
import ReactHocs.DisplayName (setDisplayName)
import Redox as Redox
import Redox.Store (ReadRedox, RedoxStore, Store, SubscribeRedox, SubscriptionId)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type DispatchFn a state dsl reff eff = Free dsl (state -> state) -> Eff (redox :: RedoxStore reff | eff) a
type DispatchFnFiber state dsl reff eff = DispatchFn (Fiber (redox :: RedoxStore reff | eff) Unit) state dsl reff eff

-- | Shallowly compare two objects.  If the first argument is true it skips
-- | comparing the `key` property which should not be accessed on a property
-- | object.
-- |
-- | If you want to use strict value equality (`===` in JavaScript), check out
-- | `Unsafe.Reference.unsafeRefEq`.
foreign import unsafeShallowEqual' :: forall a. Fn3 Boolean a a Boolean

unsafeShallowEqual :: forall a. Boolean -> a -> a -> Boolean
unsafeShallowEqual = runFn3 unsafeShallowEqual'

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
  , dispatch :: DispatchFnFiber state dsl reff eff
  }

-- | You need to wrap your most top-level component with `withStore`.  It makes
-- | the store and the bound dispatch function available through React context.
-- | Then you can connect a component with `connect` (or `connect'`) and get
-- | access to the store and the dispatch function.  It serves the same purpose
-- | as
-- | [Provider](https://github.com/reactjs/react-redux/blob/master/docs/api.md#provider-store)
-- | class in `react-redux`.
withStore
  :: forall state props dsl reff eff
  -- redox store
   . Store state
  -- bound redox dispatch function
  -> (Store state -> DispatchFnFiber state dsl reff eff)
  -> ReactClass props
  -> Eff eff (ReactClass props)
withStore store dispatch_ cls =
  pure $ withContext { redox: RedoxContext { store, dispatch: dispatch_ store } } cls

newtype StoreProvider state dsl reff eff  = StoreProvider
  { store :: Store state
  , dispatch :: Store state -> DispatchFnFiber state dsl reff eff
  }

-- | `StoreProvider` component.  This function provides store and the dispatch
-- | function through the React context.  This is an alternative to `withStore`
-- | function.
storeProvider :: forall state dsl reff eff. ReactClass (StoreProvider state dsl reff eff)
storeProvider = withContext'
    (case _ of StoreProvider { store, dispatch: dsp } -> { redox: RedoxContext { store, dispatch: dsp store } })
  $ setDisplayName "StoreProvider"
  $ createClassStateless' \_ children -> div' children

newtype ConnectState state = ConnectState { state :: state, sid :: Maybe SubscriptionId }

derive instance newtypeConnectState :: Newtype (ConnectState state) _

-- Read redox contenxt.
readRC
  :: forall state state' props dsl reff eff
   . ReactThis props (ConnectState state')
  -> Eff (context :: CONTEXT | eff) (RedoxContext state dsl reff eff)
readRC this = _.redox <$> readContext Proxy this

newtype ConnectCfg state state' props props' dsl res reff eff = ConnectCfg
  { eqs :: state' -> state' -> Boolean
  -- | compare props in `componentShouldUpdate`
  , eqp ::  props' -> props' -> Boolean
  -- | Gets the internal state (`state'`) from the store's state
  -- | (`state`).  The `state'` is recorded in components state and the
  -- | `shouldComponentUpdate` method guards changes of it using the `eqs`
  -- | function.
  , _lns :: state -> state'
  , _iso :: (DispatchFn res state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff) -> state' -> props' -> props
  , coerce :: (Fiber (redox :: RedoxStore (read :: ReadRedox, subscribe :: SubscribeRedox | reff) | eff) Unit) -> res
  , cls :: ReactClass props
  }


-- | Internal `_connect` implementation.
_connect
  :: forall state state' props props' dsl res reff eff
     -- | compare state in `componentShouldUpdate`
   . ConnectCfg state state' props props' dsl res reff eff
  -> ReactSpec props' (ConnectState state')
      ( context :: CONTEXT
      , redox :: RedoxStore (read :: ReadRedox, subscribe :: SubscribeRedox | reff)
      | eff
      )
_connect (ConnectCfg { eqs, eqp, _lns, _iso, coerce, cls }) = (R.spec' getInitialState rndr)
    { displayName = getDisplayName cls <> "Connect"
    , componentDidMount = didMount
    , componentWillUnmount = willUnmount
    , shouldComponentUpdate = shouldUpdate
    }
  where
    -- Connect the child component to the context, this is required for the
    -- `dispatch` function.
    cwc :: ReactClass props
    cwc = accessContext cls

    update this state = do
      st <- readState this
      isMounted <- readIsMounted this
      when isMounted do
        void $ writeState this $ over ConnectState (_ { state = _lns state }) st

    getInitialState this = do
      -- unsafeCoerceEff is used to add react effects to `readRC`
      RedoxContext ctx <- unsafeCoerceEff $ readRC this
      state <- _lns <$> Redox.getState ctx.store
      pure (ConnectState { state, sid: Nothing })

    -- | Subscription to redox store happens in `componentDidMount`, rather than
    -- | on `componentWillMount`.  This is because `componentWillUnmount` and
    -- | `componentDidMount` do not fire in SSR. Otherwise we'd have a memory
    -- | leak.
    didMount this = do
      writeIsMounted this true
      RedoxContext { store } <- unsafeCoerceEff $ readRC this
      ConnectState { state } <- readState this
      sid <- Redox.subscribe store $ update this
      state' <- _lns <$> Redox.getState store
      _ <- if state `eqs` state'
        then writeState this (ConnectState { sid: Just sid, state })
        else writeState this (ConnectState { sid: Just sid, state: state' })
      pure unit

    willUnmount this = do
      writeIsMounted this false
      RedoxContext ctx <- unsafeCoerceEff $ readRC this
      ConnectState { sid } <- R.readState this
      traverse_ (Redox.unsubscribe ctx.store) sid

    -- | If `Eq state'` and `Eq props'` are not tight enought you might end up
    -- | with an infitinte loop of actions.  If you see a stack overflow error
    -- | this the usal the cause.  You can use `Data.Record.equal` or
    -- | `Unsafe.Reference.unsafeRefEq` (i.e. JavaScripts `===`) or
    -- | `React.Redox.unsafeShallowEqual` functions.
    shouldUpdate this nPr (ConnectState nSt) = do
      pr <- getProps this
      ConnectState st <- readState this
      -- Take care only of `st.state` changes, `st.sid` is not used for
      -- rendering.
      pure $ not (st.state `eqs` nSt.state) || not (pr `eqp` nPr)

    rndr this = do
      props' <- R.getProps this
      children <- R.getChildren this
      RedoxContext ctx <- unsafeCoerceEff $ readRC this
      ConnectState { state } <- R.readState this
      pure $ R.createElement cwc (_iso (map coerce <<< ctx.dispatch) state props') children

-- | Newtype wrapper around `ReactSpec`.  Use  `overRedoxSpec` to change the
-- | underlying spec and `asReactClass` to create a react class.
newtype RedoxSpec props state eff = RedoxSpec (ReactSpec props state eff)

overRedoxSpec
  :: forall props state eff
   . (ReactSpec props state eff ->  ReactSpec props state eff)
  -> RedoxSpec props state eff
  -> RedoxSpec props state eff
overRedoxSpec f (RedoxSpec spec) = RedoxSpec <<< f $ spec

asReactClass :: forall props state eff. RedoxSpec props state eff -> ReactClass props
asReactClass (RedoxSpec spec) = accessContext <<< createClass $ spec

coerceAny :: forall a. a -> Unit
coerceAny _ = unit

-- | This function makes the redox store and dispatch function available
-- | through context.  The first argument is a lens that identifies the part
-- | of redox store's state that you want to subscribe for.  The view on this
-- | lens will be checked for changes, so if you want to optimise your code,
-- | return as 'tight' lens as possible.
-- |
-- | Use `asReactClass` function to create react class from `RedoxSpec` that
-- | this function returns and `overRedoxSpec` to possibly change the spec, for
-- | example by changing the `shouldComponentUpdate` react life cycle method.
-- |
-- | The second argument let you combine state and additional properties
-- | `props'` to get props of the class that you are connecting to the store.
-- | You can read the context with:
-- | ```purescript
-- | ReactHocs.readContext this >>= pure <<< _.redox :: Eff eff (RedoxContext state (Free dsl (state -> state))  eff)
-- | ```
-- |
-- | Note that the `Eq` instance must be resolved when you apply this fuction.
-- | Otherwise you will endup with
-- | [re-mounts](https://github.com/purescript-contrib/purescript-react/issues/105).
-- | This can lead to components loosing focus (in case of `input` elements).
connect'
  :: forall state state' dsl props props' reff eff
   . Proxy state
  -> (state' -> state' -> Boolean)
  -> (props' -> props' -> Boolean)
  -> (state -> state')
  -> (DispatchFnFiber state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff -> state' -> props' -> props)
  -> ReactClass props
  -> RedoxSpec props' (ConnectState state') ( context :: CONTEXT, redox :: RedoxStore (read :: ReadRedox, subscribe :: SubscribeRedox | reff) | eff )
connect' _ eqs eqp _lns _iso cls = RedoxSpec $ _connect (ConnectCfg { eqs, eqp, _lns, _iso, coerce: id,  cls })

-- | This is useful when you have (or can derive) a lawful `Eq` instances for
-- | `state'` and `props'`.
connectEq'
  :: forall state state' dsl props props' reff eff
   . Eq state'
  => Eq props'
  => Proxy state
  -> (state -> state')
  -> (DispatchFnFiber state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff -> state' -> props' -> props)
  -> ReactClass props
  -> RedoxSpec props' (ConnectState state') ( context :: CONTEXT, redox :: RedoxStore (read :: ReadRedox, subscribe :: SubscribeRedox | reff) | eff )
connectEq' p = connect' p (==) (==)

-- | Like `connect'` but for `ReactClass`-es.
connect
  :: forall state state' dsl props props' reff eff'
   . Proxy state
  -> (state' -> state' -> Boolean)
  -> (props' -> props' -> Boolean)
  -> (state -> state')
  -> (DispatchFnFiber state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff' -> state' -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
connect p eqs eqp _lns _iso cls = asReactClass $ connect' p eqs eqp _lns _iso cls

-- | Like `connect` but with `Eq` constraints.
connectEq
  :: forall state state' dsl props props' reff eff'
   . Eq state'
  => Eq props'
  => Proxy state
  -> (state -> state')
  -> (DispatchFnFiber state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff' -> state' -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
connectEq p = connect p (==) (==)

-- | Like `connect'`, only the dispatch function returns `Eff eff Unit` rather
-- | than `Eff eff (Fiber eff Unit)`.
connectV'
  :: forall state state' dsl props props' reff eff
   . Proxy state
  -> (state' -> state' -> Boolean)
  -> (props' -> props' -> Boolean)
  -> (state -> state')
  -> (DispatchFn Unit state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff -> state' -> props' -> props)
  -> ReactClass props
  -> RedoxSpec props' (ConnectState state') ( context :: CONTEXT, redox :: RedoxStore (read :: ReadRedox, subscribe :: SubscribeRedox | reff) | eff )
connectV' _ eqs eqp _lns _iso cls = RedoxSpec $ _connect (ConnectCfg {eqs, eqp, _lns, _iso, coerce: coerceAny, cls})

-- | Like `connectEq'`, only the dispatch function returns `Eff eff Unit` rather
-- | than `Eff eff (Fiber eff Unit)`.
connectEqV'
  :: forall state state' dsl props props' reff eff
   . Eq state'
  => Eq props'
  => Proxy state
  -> (state -> state')
  -> (DispatchFn Unit state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff -> state' -> props' -> props)
  -> ReactClass props
  -> RedoxSpec props' (ConnectState state') ( context :: CONTEXT, redox :: RedoxStore (read :: ReadRedox, subscribe :: SubscribeRedox | reff) | eff )
connectEqV' p = connectV' p (==) (==)

-- | Like `connectEq`, only the dispatch function returns `Eff eff Unit` rather
-- | than `Eff eff (Fiber eff Unit)`.
connectEqV
  :: forall state state' dsl props props' reff eff'
   . Eq state'
  => Eq props'
  => Proxy state
  -> (state -> state')
  -> (DispatchFn Unit state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff' -> state' -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
connectEqV p = connectV p (==) (==)

-- | Like `connect`, only the dispatch function returns `Eff eff Unit` rather
-- | than `Eff eff (Fiber eff Unit)`.
connectV
  :: forall state state' dsl props props' reff eff'
   . Proxy state
  -> (state' -> state' -> Boolean)
  -> (props' -> props' -> Boolean)
  -> (state -> state')
  -> (DispatchFn Unit state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff' -> state' -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
connectV p eqs eqp _lns _iso cls = asReactClass $ connectV' p eqs eqp _lns _iso cls

_withDispatch
  :: forall res state props props' dsl reff eff'
   . (Fiber (redox :: RedoxStore (read :: ReadRedox, subscribe :: SubscribeRedox | reff) | eff') Unit -> res)
  -> (DispatchFn res state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff' -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
_withDispatch gn fn cls = accessContext $ createClassStatelessWithContext
  \props' { redox: RedoxContext { dispatch: disp }}
  -> createElement cls (fn (map gn <<< disp) props') (childrenToArray (unsafeCoerce props').children)

-- | If you just want to wrap your actions with a dispatch function use this
-- | function.  Unlike `connect'` (and `connect`) it does not wrap your
-- | component inside another component.
withDispatch
  :: forall state props props' dsl reff eff'
   . (DispatchFnFiber state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff' -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
withDispatch = _withDispatch id

withDispatchV
  :: forall state props props' dsl reff eff'
   . (DispatchFn Unit state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff' -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
withDispatchV = _withDispatch coerceAny

_dispatch
  :: forall res dsl rProps rState state reff eff
   . ((Fiber (context :: CONTEXT, redox :: RedoxStore reff | eff) Unit) -> res)
  -> ReactThis rProps rState
  -> Free dsl (state -> state)
  -> Eff (context :: CONTEXT, redox :: RedoxStore reff | eff) res
_dispatch fn this dsl =
  let proxy :: Proxy ({ redox :: RedoxContext state dsl reff (context :: CONTEXT | eff) })
      proxy = Proxy
  in do
    { redox: RedoxContext { dispatch: disp } } <- readContext proxy this
    fn <$> disp dsl

-- | The component must be wrapped with `accessContext` to use this function.
-- | `connect` and `asReactClass` do that for you.
dispatch
  :: forall dsl rProps rState state reff eff
   . ReactThis rProps rState
  -> Free dsl (state -> state)
  -> Eff (context :: CONTEXT, redox :: RedoxStore reff | eff) (Fiber (context :: CONTEXT, redox :: RedoxStore reff | eff) Unit)
dispatch = _dispatch id

-- | Like `dispatch'`, only the dispatch function returns `Eff eff Unit` rather
-- | than `Eff eff (Fiber eff Unit)`.
dispatchV
  :: forall dsl rProps rState state reff eff
   . ReactThis rProps rState
  -> Free dsl (state -> state)
  -> Eff (context :: CONTEXT, redox :: RedoxStore reff | eff) Unit
dispatchV = _dispatch coerceAny
