module SambaDeUmaNotaSo.Tessellation where

import Prelude
import Control.Monad.Free (Free, liftF)
import Data.Array as A
import Data.Maybe (Maybe)
import Test.QuickCheck.Gen (Gen, chooseInt)

type Point
  = { x :: Int, y :: Int }

type Rectangle
  = { x0 :: Int, y0 :: Int, x1 :: Int, y1 :: Int }

grid :: Array (Array Boolean)
grid = A.replicate 1000 (A.replicate 1000 false)

ambitus :: Gen Int
ambitus = chooseInt 10 300

data ProgramF a
  = GetBias (Point -> a)
  | GetFreePoint (Maybe Point -> a)
  | AttemptLeftExtension Rectangle Int (Rectangle -> a)
  | AttemptRightExtension Rectangle Int (Rectangle -> a)
  | AttemptUpExtension Rectangle Int (Rectangle -> a)
  | AttemptDownExtension Rectangle Int (Rectangle -> a)
  | StoreRectangle Rectangle a

type Program a
  = Free ProgramF a

getBias :: Program Point
getBias = liftF $ GetBias identity

getFreePoint :: Program (Maybe Point)
getFreePoint = liftF $ GetFreePoint identity

attemptLeftExtension :: Rectangle -> Int -> Program Rectangle
attemptLeftExtension r i = liftF $ AttemptLeftExtension r i identity

attemptRightExtension :: Rectangle -> Int -> Program Rectangle
attemptRightExtension r i = liftF $ AttemptRightExtension r i identity

attemptUpExtension :: Rectangle -> Int -> Program Rectangle
attemptUpExtension r i = liftF $ AttemptUpExtension r i identity

attemptDownExtension :: Rectangle -> Int -> Program Rectangle
attemptDownExtension r i = liftF $ AttemptDownExtension r i identity

storeRectangle :: Rectangle -> Program Unit
storeRectangle r = liftF $ StoreRectangle r unit
