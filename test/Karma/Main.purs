module Test.Karma.Main where

import Control.Comonad.Cofree (Cofree, exploreM, unfoldCofree)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, errorShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Control.Monad.Free (Free, liftF)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, readInt)
import Data.Foreign.Index ((!))
import Data.Newtype (class Newtype)
import Enzyme.Mount (mount)
import Enzyme.ReactWrapper as E
import Enzyme.Types (ENZYME)
import Prelude hiding (add,div)
import React (ReactClass, createClass, createClassStateless, createElement, spec)
import React.DOM (div)
import React.DOM.Props as P
import React.Redox (StoreProvider(StoreProvider), connectEq, storeProvider, withStore)
import ReactHocs (accessContext, readContext)
import Redox (CreateRedox, RedoxStore, mkStore)
import Redox as Redox
import Test.Unit (failure, success, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Karma (runKarma)
import Type.Proxy (Proxy(..))

newtype Counter = Counter
  { count :: Int }

derive instance newtypeCounter :: Newtype Counter _

instance showCounter :: Show Counter where
  show (Counter { count }) = "Counter { count: " <> show count <> "}"

-- Additive commands
data Command a = Add Int a

add :: Int -> Free Command (Counter -> Counter)
add x = liftF $ Add x id

derive instance functorCommand :: Functor Command

-- Additive interpreter
data RunCommand e a = RunCommand
  { add :: Int -> Aff e a }

derive instance functorRunCommand :: Functor (RunCommand e)

run :: forall e. Free Command (Counter -> Counter) -> Counter -> Aff e Counter
run cmds state = exploreM pair cmds mkInterp
  where
    pair :: forall x y. Command (x -> y) -> RunCommand e x -> Aff e y
    pair (Add x f) (RunCommand i) = f <$> i.add x

    mkInterp :: Cofree (RunCommand e) Counter
    mkInterp = unfoldCofree id next state
      where
        add' :: Counter -> Int -> Aff e Counter
        add' (Counter st@{ count }) x = pure $ Counter (st { count = count + x})

        next :: Counter -> RunCommand e Counter
        next st = RunCommand { add: add' st }

main :: forall eff. Eff
  ( avar :: AVAR
  , console :: CONSOLE
  , dom :: DOM
  , enzyme :: ENZYME
  , exception :: EXCEPTION
  , testOutput :: TESTOUTPUT
  , redox :: RedoxStore (create :: CreateRedox)
  | eff
  ) Unit
main = runKarma do
  suite "React.Redox" do
    let ac :: ReactClass Unit
        ac = accessContext $ createClass ((spec unit renderFn) { displayName = "AccessContext" })

        renderFn this = do
          ctx <- readContext (Proxy :: Proxy { store :: Foreign, dispatch :: Foreign }) this
          pure $ div [ P._id "ac", P._data ctx ] []

        readRedoxContext :: Foreign -> F { store :: Foreign, dispatch :: Foreign }
        readRedoxContext obj =
          { store: _, dispatch: _ }
          <$> obj ! "data-redox" ! "store"
          <*> obj ! "data-redox" ! "dispatch"

    test "withStore" do
      store <- liftEff $ mkStore (Counter { count: 0 })
      let cls :: ReactClass Unit
          cls = createClassStateless \_ -> createElement ac unit []

      props <- liftEff $
        withStore store (Redox.dispatch errorShow run) cls
        >>= mount <<< (\cls' -> createElement cls' unit [])
        >>= E.find "#ac"
        >>= E.props

      case runExcept (readRedoxContext props) of
        Left err -> failure ("failed to read context: " <> show err)
        Right _ -> success

    test "storeProvider" do
      store <- liftEff $ mkStore (Counter { count: 0 })

      props <- liftEff
          $ mount (createElement storeProvider (StoreProvider { store, dispatch: Redox.dispatch errorShow run }) [ createElement ac unit [] ])
        >>= E.find "#ac"
        >>= E.props

      case runExcept (readRedoxContext props) of
        Left err -> failure ("failed to read context: " <> show err)
        Right _ -> success

    suite "connect" do
      let cls = connectEq (Proxy :: Proxy (Command (Counter -> Counter)))
                  (\(Counter { count }) -> count)
                  (\_ count _ -> { count })
                  $ createClassStateless
                    \{ count } -> div [ P._id "count", P._data { count } ]  []

      test "withStore" do
        store <- liftEff $ mkStore (Counter { count: 1 })
        count <- liftEff
            $ withStore store (Redox.dispatch errorShow run) cls
          >>= (\cls' -> createElement cls' unit []) >>> mount 
          >>= E.find "#count"
          >>= E.prop "data-count"

        case runExcept (readInt count) of
          Left err  -> failure ("failed to read count: " <> show err)
          Right c   -> assert ("wrong value: " <> show c) $ c == 1

      test "storeProvider" do
        store <- liftEff $ mkStore (Counter { count: 1 })

        count <- liftEff
            $ mount (createElement storeProvider (StoreProvider { store, dispatch: Redox.dispatch errorShow run }) [ createElement cls unit [] ])
          >>= E.find "#count"
          >>= E.prop "data-count"

        case runExcept (readInt count) of
          Left err  -> failure ("failed to read count: " <> show err)
          Right c   -> assert ("wrong value: " <> show c) $ c == 1

  suite "dispatch" do
      let cls = connectEq (Proxy :: Proxy (Command (Counter -> Counter)))
                  (\(Counter { count }) -> count)
                  (\disp count _ -> { count, onClick: clickHandler disp })
                  $ createClassStateless
                    \{ count, onClick } -> div [ P._id "count", P._data { count }, P.onClick onClick ]  []

          clickHandler disp _ = void $ disp $ add 1

      test "withStore" do
        store <- liftEff $ mkStore (Counter { count: 0 })
        count <- liftEff
            $ withStore store (Redox.dispatch errorShow run) cls
          >>= (\cls' -> createElement cls' unit []) >>> mount
          >>= E.find "#count"
          >>= E.simulate "click"
          >>= E.prop "data-count"

        case runExcept (readInt count) of
          Left err  -> failure ("failed to read count: " <> show err)
          Right c   -> assert ("wrong value: " <> show c) $ c == 1

      test "storeProvider" do
        store <- liftEff $ mkStore (Counter { count: 0 })

        count <- liftEff
            $ mount (createElement storeProvider (StoreProvider { store, dispatch: Redox.dispatch errorShow run }) [ createElement cls unit [] ])
          >>= E.find "#count"
          >>= E.simulate "click"
          >>= E.prop "data-count"

        case runExcept (readInt count) of
          Left err  -> failure ("failed to read count: " <> show err)
          Right c   -> assert ("wrong value: " <> show c) $ c == 1
