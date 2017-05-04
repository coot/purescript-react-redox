module Test.Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Function.Uncurried (runFn2)
import Data.Lens (lens, view)
import React.Redox (unsafeShallowEqual, unsafeStrictEqual)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall eff. Eff (avar :: AVAR, console :: CONSOLE, testOutput :: TESTOUTPUT, err :: EXCEPTION | eff) Unit
main = runTest do
  suite "unsafeStrictEqual" do
    test "record update"
      let r = { field: "Hello" }
      in do
        assert "updated records should be unequal" $ not $ runFn2 unsafeStrictEqual r (r { field = "Hay" })

    test "new records with the same content" do
      -- not equal since we create a new top level Object
      assert "new records with primitive value should not be equal" $ not $ runFn2 unsafeStrictEqual { field: 1 } { field: 1 }
      let x = { x: 1 }
      assert "new records should not be equal" $ not $ runFn2 unsafeStrictEqual { x } { x }

    test "lens view"
      let 
        lens1_ = lens (_.field) (_ { field = _ })
        lens2_ = lens (_.field) (_ { field = _ })
        r1 = { field: { str: "Hello" } }
        r2 = { field: ":)" }

      in do
        assert "lens view should be equal" $ runFn2 unsafeStrictEqual (view lens1_ r1) (view lens1_ r1)
        assert "lens view should be equal" $ runFn2 unsafeStrictEqual (view lens2_ r2) (view lens2_ r2)

  suite "unsafeShallowEqual" do
    test "record update"
      let r = { field: "Hello" }
      in do
        assert "updated records should be unequal" $ not $ runFn2 unsafeShallowEqual r (r { field = "Hey!" })

    test "new records with the same content" do
      -- not equal since we create a new top level Object
      assert "new records with primitive value should not be equal" $ not $ runFn2 unsafeShallowEqual { field: 1 } { field: 1 }
      let x = { x: 1 }
      assert "new records should not be equal" $ not $ runFn2 unsafeShallowEqual { x } { x }

    test "lens view"
      let
        lens1_ = lens (_.field) (_ { field = _ })
        lens2_ = lens (_.field) (_ { field = _ })
        r1 = { field: { str: "Hello" } }
        r2 = { field: ":)" }

      in do
        assert "lens view should be equal" $ runFn2 unsafeShallowEqual (view lens1_ r1) (view lens1_ r1)
        assert "lens view should be equal" $ runFn2 unsafeShallowEqual (view lens2_ r2) (view lens2_ r2)
