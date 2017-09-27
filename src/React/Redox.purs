module React.Redox
  ( ConnectState(..)
  , DispatchFn
  , RedoxContext
  , RedoxSpec
  , withStore
  , StoreProvider(..)
  , storeProvider
  , connect'
  , connect
  , withDispatch
  , dispatch
  , asReactClass
  , overRedoxSpec
  , unsafeShallowEqual
  , unsafeStrictEqual
  ) where

import Prelude

import Control.Monad.Aff (Fiber)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Free (Free)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn2, runFn3)
import Data.Lens (Getter', view)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, over)
import React (ReactClass, ReactElement, ReactSpec, ReactThis, childrenToArray, createClass, createClassStateless', createElement, forceUpdate, getProps, readState, writeState)
import React as R
import React.DOM (div')
import ReactHocs (CONTEXT, accessContext, getDisplayName, readContext, withContext, withContext')
import ReactHocs.DisplayName (setDisplayName)
import Redox as Redox
import Redox.Store (ReadRedox, RedoxStore, Store, SubscribeRedox, SubscriptionId)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type DispatchFn state dsl reff eff = Free dsl (state -> state) -> Eff (redox :: RedoxStore reff | eff) (Fiber (redox :: RedoxStore reff | eff) Unit)

-- | Shallowly compare two objects.  If the first argument is true it skips
-- | comparing the `key` property which should not be accessed on a property
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
-- | the store and the bound dispatch function available through React context.
-- | Then you can connect a component with `connect` (or `connect'`) and get
-- | access to the store and the dispatch function.
withStore
  :: forall state props dsl reff eff
  -- redox store
   . Store state
  -- bound redox dispatch function
  -> (Store state -> DispatchFn state dsl reff eff)
  -> ReactClass props
  -> Eff eff (ReactClass props)
withStore store dispatch_ cls =
  pure $ withContext { redox: RedoxContext { store, dispatch: dispatch_ store } } cls

newtype StoreProvider state dsl reff eff  = StoreProvider
  { store :: Store state
  , dispatch :: Store state -> DispatchFn state dsl reff eff
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

_connect
  :: forall state state' dsl props props' reff eff
   . Eq state'
  => (ReactThis props' (ConnectState state')
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

    -- | If `Eq state'` and `Eq props'` are not tight enought you might end up
    -- | with an infitinte loop of actions.  If you see a stack overflow error
    -- | this the usal the cause.  You can use `Data.Record.equal` or one of
    -- | `unsafeShallowEqual`, `unsafeStrictEqual` functions.
    shouldComponentUpdate this nPr (ConnectState nSt) = do
      pr <- getProps this
      ConnectState st <- readState this
      -- Take care only of `st.state` changes, `st.sid` is not used for
      -- rendering.
      pure $ st.state /= nSt.state || not (runFn3 unsafeShallowEqual true pr nPr)

    renderFn this = do
      props' <- R.getProps this
      children <- R.getChildren this
      RedoxContext ctx <- unsafeCoerceEff $ ctxEff this
      ConnectState { state } <- R.readState this
      pure $ R.createElement cls_ (_iso ctx.dispatch state props') children

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
-- | Not the `Eq` type classes must be resolved when you apply this fuction.
-- | Otherwise you will endup with
-- | [re-mounts](https://github.com/purescript-contrib/purescript-react/issues/105).
-- | This can lead to components loosing focus (in case of `input` elements).
connect'
  :: forall state state' dsl props props' reff eff
   . Eq state'
  => Proxy state
  -> Getter' state state'
  -> (DispatchFn state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff -> state' -> props' -> props)
  -> ReactClass props
  -> RedoxSpec props' (ConnectState state') ( context :: CONTEXT, redox :: RedoxStore (read :: ReadRedox, subscribe :: SubscribeRedox | reff) | eff )
connect' _ _lns _iso cls = RedoxSpec $ _connect ctxEff _lns _iso cls
  where
    proxy :: Proxy ({ redox :: RedoxContext state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff })
    proxy = Proxy

    ctxEff this = _.redox <$> readContext proxy this

-- | Like `connect'` but for `ReactClass`-es.
connect
  :: forall state state' dsl props props' reff eff'
   . Eq state'
  => Proxy state
  -> Getter' state state'
  -> (DispatchFn state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff' -> state' -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
connect p _lns _iso cls = asReactClass $ connect' p _lns _iso cls

-- | If you just want to wrap your actions with a dispatch function use this
-- | function.  Unlike `connect'` (and `connect`) it does not wrap your
-- | component inside another component.
withDispatch
  :: forall state props props' dsl reff eff'
   . (DispatchFn state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff' -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
withDispatch fn cls = accessContext $ createClassStatelessWithContext
  \props' { redox: RedoxContext { dispatch: disp }}
  -- todo: childrenToArray
  -> createElement cls (fn disp props') (childrenToArray (unsafeCoerce props').children)

-- | The component must be wrapped with `accessContext` to use this function.
-- | `connect` and `asReactClass` do that for you.
dispatch
  :: forall dsl rProps rState state reff eff
   . ReactThis rProps rState
  -> Free dsl (state -> state)
  -> Eff (context :: CONTEXT, redox :: RedoxStore reff | eff) (Fiber (context :: CONTEXT, redox :: RedoxStore reff | eff) Unit)
dispatch this dsl =
  let proxy :: Proxy ({ redox :: RedoxContext state dsl reff (context :: CONTEXT | eff) })
      proxy = Proxy
  in do
    { redox: RedoxContext { dispatch: disp } } <- readContext proxy this
    disp dsl
