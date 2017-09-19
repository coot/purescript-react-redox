module Test.ServerSideRendering where

import Control.Comonad.Cofree (Cofree, exploreM, unfoldCofree)
import Control.Monad.Aff (Aff, Fiber)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Free (Free, liftF)
import Data.Lens (to)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, over, un)
import Prelude (class Functor, Unit, bind, const, discard, id, pure, unit, void, ($), (<$>))
import React (ReactClass, createClass, createClassStateless, createElement, getProps, spec)
import React.DOM (div', text)
import React.Redox (connect, withStore)
import ReactDOM (renderToString)
import Redox (CreateRedox, RedoxStore, Store, ReadWriteRedox, mkStore)
import Redox (dispatch) as Redox
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Type.Proxy (Proxy(..))

data Command a
  = PutMsg String a
  | Reset a

derive instance functorCommand :: Functor Command

putMsg :: forall a. String -> Free Command (a -> a)
putMsg msg = liftF (PutMsg msg id)

reset :: forall a. Free Command (a -> a)
reset = liftF (Reset id)

newtype Run eff a = Run
  { putMsg :: String -> Aff eff a
  , reset :: Aff eff a
  }

derive instance functorRun :: Functor (Run eff)

newtype State = State { msg :: Maybe String }

derive instance newtypeState :: Newtype State _

mkInterp :: forall eff. State -> Cofree (Run eff) State
mkInterp state = unfoldCofree id next state
  where
    next :: State -> Run eff State
    next st = Run
      { putMsg: \msg -> pure $ over State (_ { msg = (Just msg) }) st
      , reset: pure $ over State (_ { msg = Nothing } ) st
      }

pair :: forall eff x y. Command (x -> y) -> Run eff x -> Aff eff y
pair (PutMsg msg next) (Run i) = next <$> (i.putMsg msg)
pair (Reset next) (Run i) = next <$> i.reset

runInterp :: forall eff. Store State -> Free Command (State -> State) -> State -> Aff eff State
runInterp store cmds state = exploreM pair cmds (mkInterp state)

testSuite :: forall e. TestSuite (redox :: RedoxStore (create :: CreateRedox) | e)
testSuite =
  suite "server side rendering"
    let
      disp
        :: forall e1
         . Store State
        -> Free Command (State -> State)
        -> Eff
            ( redox :: RedoxStore ReadWriteRedox | e1)
            (Fiber ( redox :: RedoxStore ReadWriteRedox | e1) Unit)
      disp store = Redox.dispatch (const $ pure unit) (runInterp store) store

      cls :: ReactClass { msg :: Maybe String }
      cls = createClass (spec unit renderCls)
      renderCls this = do
        { msg } <- getProps this
        let msg_ = fromMaybe "" msg
        pure (div' [ text msg_ ])

      connCls :: ReactClass Unit
      connCls = connect
        (Proxy :: Proxy State)
        (to (un State))
        (\_ { msg } _ -> { msg })
        cls
    in do
      test "connect"
        do
          str <- liftEff do
            store <- mkStore (State { msg: Just "hello" })
            top <- withStore store disp (createClassStateless \_ -> createElement connCls unit [])
            pure $ renderToString (createElement top unit [])
          equal str """<div data-reactroot="" data-reactid="1" data-react-checksum="-857140882">hello</div>"""

      test "componentWillMount" do
        str <- liftEff do
          store <- mkStore (State { msg: Nothing })
          let

            topSpec = (spec unit renderTop)
              { componentWillMount = componentWillMount
              , componentDidMount = componentDidMount
              }

            renderTop _ = pure $ createElement connCls unit []

            componentWillMount _ = do
              -- is called
              void $ disp store (putMsg "hello")

            componentDidMount _ = do
              -- is not called
              void $ disp store reset

          top <- withStore store disp (createClass topSpec)
          pure $ renderToString (createElement top unit [])
        equal str """<div data-reactroot="" data-reactid="1" data-react-checksum="-857140882">hello</div>"""
