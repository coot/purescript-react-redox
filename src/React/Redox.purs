module React.Redox
  ( ConnectState(..)
  , DispatchFn
  , RedoxContext
  , RedoxSpec
  , withStore
  , connect'
  , connectEq'
  , connect
  , connectEq
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
import Data.Function.Uncurried (Fn2, Fn3, mkFn2)
import Data.Lens (Getter', view)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, over)
import Data.Traversable (traverse_)
import React (ReactClass, ReactElement, ReactSpec, ReactThis, childrenToArray, createClass, createElement, getProps, readState, writeState)
import React as R
import ReactHocs (CONTEXT, withContext, accessContext, readContext, getDisplayName)
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
  pure $ withContext cls { redox: RedoxContext { store, dispatch: dispatch_ store } }

newtype ConnectState state = ConnectState { state :: state, sid :: Maybe SubscriptionId }

derive instance newtypeConnectState :: Newtype (ConnectState state) _

-- | internal `_connect` implementation
_connect
  :: forall state state' dsl props props' reff eff
  -- | compare state in `componentShouldUpdate`
   . (state' -> state' -> Boolean)
  -- | compare props in `componentShouldUpdate`
  -> (props' -> props' -> Boolean)
  -- | lens which gets the internal state (`state'`) from the store's state
  -- | (`state`)
  -> Getter' state state'
  -> ((DispatchFn state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff) -> state' -> props' -> props)
  -> ReactClass props
  -> ReactSpec props' (ConnectState state')
      ( context :: CONTEXT
      , redox :: RedoxStore (read :: ReadRedox, subscribe :: SubscribeRedox | reff)
      | eff
      )
_connect eqs eqp _lns _iso cls = (R.spec' getInitialState rndr)
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

    cp :: Proxy ({ redox :: RedoxContext state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff })
    cp = Proxy

    readRC this = _.redox <$> readContext cp this

    update this state = do
      st <- readState this
      isMounted <- readIsMounted this
      when isMounted do
        void $ writeState this $ over ConnectState (_ { state = view _lns state }) st

    getInitialState this = do
      -- unsafeCoerceEff is used to add react effects to `readRC`
      RedoxContext ctx <- unsafeCoerceEff $ readRC this
      state <- view _lns <$> Redox.getState ctx.store
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
      state' <- view _lns <$> Redox.getState store
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
      pure $ R.createElement cwc (_iso ctx.dispatch state props') children

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
   . Proxy state
  -> (state' -> state' -> Boolean)
  -> (props' -> props' -> Boolean)
  -> Getter' state state'
  -> (DispatchFn state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff -> state' -> props' -> props)
  -> ReactClass props
  -> RedoxSpec props' (ConnectState state') ( context :: CONTEXT, redox :: RedoxStore (read :: ReadRedox, subscribe :: SubscribeRedox | reff) | eff )
connect' _ eqs eqp _lns _iso cls = RedoxSpec $ _connect eqs eqp _lns _iso cls

connectEq'
  :: forall state state' dsl props props' reff eff
   . Eq state'
  => Eq props'
  => Proxy state
  -> Getter' state state'
  -> (DispatchFn state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff -> state' -> props' -> props)
  -> ReactClass props
  -> RedoxSpec props' (ConnectState state') ( context :: CONTEXT, redox :: RedoxStore (read :: ReadRedox, subscribe :: SubscribeRedox | reff) | eff )
connectEq' p = connect' p (==) (==)

-- | Like `connect'` but for `ReactClass`-es.
connect
  :: forall state state' dsl props props' reff eff'
   . Proxy state
  -> (state' -> state' -> Boolean)
  -> (props' -> props' -> Boolean)
  -> Getter' state state'
  -> (DispatchFn state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff' -> state' -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
connect p eqs eqp _lns _iso cls = asReactClass $ connect' p eqs eqp _lns _iso cls

connectEq
  :: forall state state' dsl props props' reff eff'
   . Eq state'
  => Eq props'
  => Proxy state
  -> Getter' state state'
  -> (DispatchFn state dsl (read :: ReadRedox, subscribe :: SubscribeRedox | reff) eff' -> state' -> props' -> props)
  -> ReactClass props
  -> ReactClass props'
connectEq p = connect p (==) (==)

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
