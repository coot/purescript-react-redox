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
import Data.Foreign (Foreign, F)
import Data.Foreign.Index ((!))
import Data.Newtype (class Newtype)
import Enzyme.Mount (mount)
import Enzyme.ReactWrapper as E
import Enzyme.Types (ENZYME)
import Prelude hiding (div)
import React (ReactClass, createClass, createClassStateless, createElement, spec)
import React.DOM (div)
import React.DOM.Props as P
import React.Redox (withStore)
import ReactHocs (accessContext, readContext)
import Redox (CreateRedox, RedoxStore, mkStore)
import Redox as Redox
import Test.Unit (failure, success, suite, test)
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
    test "withStore" do
      store <- liftEff $ mkStore (Counter { count: 0 })
      let 
          cls :: ReactClass Unit
          cls = createClassStateless \_ -> createElement ac unit []

          ac :: ReactClass Unit
          ac = accessContext $ createClass ((spec unit renderFn) { displayName = "AccessContext" })

          renderFn this = do
            ctx <- readContext (Proxy :: Proxy { store :: Foreign, dispatch :: Foreign }) this
            pure $ div [ P._id "ac", P._data ctx ] []

          readRedoxContext :: Foreign -> F { store :: Foreign, dispatch :: Foreign }
          readRedoxContext obj =
            { store: _, dispatch: _ }
            <$> obj ! "data-redox" ! "store"
            <*> obj ! "data-redox" ! "dispatch"

      props <- liftEff $
        withStore store (Redox.dispatch errorShow run) cls
        >>= mount <<< (\cls' -> createElement cls' unit [])
        >>= E.find "#ac"
        >>= E.props

      case runExcept (readRedoxContext props) of
        Left err -> failure "ups.."
        Right _ -> success
