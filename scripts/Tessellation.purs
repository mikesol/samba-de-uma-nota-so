module SambaDeUmaNotaSo.Tessellation where

import Prelude
import Color (Color, rgb)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.State (StateT, evalStateT, get, gets, modify_)
import Control.Plus (empty)
import Data.Array as A
import Data.Array ((..))
import Data.Array.NonEmpty (fromArray, fromNonEmpty)
import Data.Filterable (filter)
import Data.Int.Bits (shl)
import Data.List (List(..), notElem, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty ((:|), NonEmpty)
import Data.Ord (abs)
import Data.Traversable (for_, sequence)
import Effect (Effect)
import Effect.Class.Console (log)
import Random.LCG (mkSeed)
import Record as R
import Test.QuickCheck.Gen (Gen, chooseInt, elements, evalGen, suchThat)

type Point
  = { x :: Int, y :: Int }

type Rectangle
  = { x0 :: Int, y0 :: Int, x1 :: Int, y1 :: Int }

type Rectangle0
  = { x0 :: Int, y0 :: Int, x1 :: Int, y1 :: Int, disallowed :: List Color }

type RectangleRGB
  = { x0 :: Int, y0 :: Int, x1 :: Int, y1 :: Int, color :: Color }

type StateImpl
  = { grid :: Array (Array Boolean)
    , rectangles :: List Rectangle
    }

type ReaderImpl
  = { minXBias :: Int
    , minYBias :: Int
    , maxXBias :: Int
    , maxYBias :: Int
    , minXRange :: Int
    , minYRange :: Int
    , maxXRange :: Int
    , maxYRange :: Int
    , paddingLeft :: Int
    , paddingRight :: Int
    , paddingUp :: Int
    , paddingDown :: Int
    , width :: Int
    , height :: Int
    , colors :: NonEmpty Array Color
    }

ambitus :: Gen Int
ambitus = chooseInt 10 300

data ProgramF a
  = GetBias (Point -> a)
  | GetTargetArea (Int -> a)
  | GetFreePoint (Maybe Point -> a)
  | AttemptLeftExtension Int Rectangle (Rectangle -> a)
  | AttemptRightExtension Int Rectangle (Rectangle -> a)
  | AttemptUpExtension Int Rectangle (Rectangle -> a)
  | AttemptDownExtension Int Rectangle (Rectangle -> a)
  | AttemptLeftPadding Rectangle (Rectangle -> a)
  | AttemptRightPadding Rectangle (Rectangle -> a)
  | AttemptUpPadding Rectangle (Rectangle -> a)
  | AttemptDownPadding Rectangle (Rectangle -> a)
  | StoreRectangle Rectangle a
  | GetRectangles (List RectangleRGB -> a)

type Program a
  = Free ProgramF a

getBias :: Program Point
getBias = liftF $ GetBias identity

getTargetArea :: Program Int
getTargetArea = liftF $ GetTargetArea identity

getFreePoint :: Program (Maybe Point)
getFreePoint = liftF $ GetFreePoint identity

attemptLeftExtension :: Int -> Rectangle -> Program Rectangle
attemptLeftExtension i r = liftF $ AttemptLeftExtension i r identity

attemptRightExtension :: Int -> Rectangle -> Program Rectangle
attemptRightExtension i r = liftF $ AttemptRightExtension i r identity

attemptUpExtension :: Int -> Rectangle -> Program Rectangle
attemptUpExtension i r = liftF $ AttemptUpExtension i r identity

attemptDownExtension :: Int -> Rectangle -> Program Rectangle
attemptDownExtension i r = liftF $ AttemptDownExtension i r identity

attemptLeftPadding :: Rectangle -> Program Rectangle
attemptLeftPadding r = liftF $ AttemptLeftPadding r identity

attemptRightPadding :: Rectangle -> Program Rectangle
attemptRightPadding r = liftF $ AttemptRightPadding r identity

attemptUpPadding :: Rectangle -> Program Rectangle
attemptUpPadding r = liftF $ AttemptUpPadding r identity

attemptDownPadding :: Rectangle -> Program Rectangle
attemptDownPadding r = liftF $ AttemptDownPadding r identity

storeRectangle :: Rectangle -> Program Unit
storeRectangle r = liftF $ StoreRectangle r unit

getRectangles :: Program (List RectangleRGB)
getRectangles = liftF $ GetRectangles identity

area :: Rectangle -> Int
area { x0, y0, x1, y1 } = abs (y0 - y1) * abs (x0 - x1)

pointToRect :: Point -> Rectangle
pointToRect { x, y } = { x0: x, y0: y, x1: x, y1: y }

makeSingleRectangle :: Point -> Program Unit
makeSingleRectangle point = do
  targetArea <- getTargetArea
  bias <- getBias
  let
    initialRect = pointToRect point
  result <- go targetArea bias initialRect
  paddedResult <- attemptLeftPadding result >>= attemptRightPadding >>= attemptUpPadding >>= attemptDownPadding
  storeRectangle paddedResult
  where
  go targetArea bias rect
    | area rect >= targetArea = pure rect
    | otherwise = do
      newRect <- attemptLeftExtension bias.x rect >>= attemptRightExtension bias.x >>= attemptUpExtension bias.y >>= attemptDownExtension bias.y
      if newRect == rect then pure rect else go targetArea bias newRect

prog :: Program (List RectangleRGB)
prog = do
  freePoint <- getFreePoint
  case freePoint of
    Nothing -> getRectangles
    Just point -> do
      makeSingleRectangle point
      prog

type RenderM a
  = ReaderT ReaderImpl (StateT StateImpl Gen) a

lift2 :: Gen ~> RenderM
lift2 = lift <<< lift

--interpreter :: ProgramF ~> StateT Gen
--interpreter GetBias f = f <$> getBiasImpl
getBiasImpl :: RenderM Point
getBiasImpl = do
  { maxXBias, maxYBias, minXBias, minYBias } <- ask
  { x: _, y: _ }
    <$> lift2 (chooseInt minXBias maxXBias)
    <*> lift2 (chooseInt minYBias maxYBias)

getTargetAreaImpl :: RenderM Int
getTargetAreaImpl = do
  { maxXRange, maxYRange, minXRange, minYRange } <- ask
  mul
    <$> lift2 (chooseInt minXRange maxXRange)
    <*> lift2 (chooseInt minYRange maxYRange)

gridToIndexed :: Array (Array Boolean) -> Array (Array { x :: Int, y :: Int, tf :: Boolean })
gridToIndexed =
  A.mapWithIndex
    ( \x a ->
        A.mapWithIndex (\y tf -> { x, y, tf }) a
    )

getFreePointImpl :: RenderM (Maybe Point)
getFreePointImpl = do
  { grid } <- get
  lift2
    $ (map <<< map) (\{ x, y } -> { x, y })
    $ sequence
    $ map elements
    $ fromArray
    $ filter (\{ tf } -> not tf)
    $ join
    $ gridToIndexed grid

attemptExtensionImpl :: (Rectangle -> RenderM Rectangle) -> Int -> Rectangle -> RenderM Rectangle
attemptExtensionImpl f i rr = go i rr
  where
  go 0 r = pure r

  go n r = do
    r' <- f r
    if r == r' then pure r else go (n - 1) r'

keepCollidingEntries :: Int -> Int -> Int -> Boolean -> Boolean
keepCollidingEntries pos l r tf = pos >= l && pos <= r && tf

attemptLeftSingleExtensionImpl :: Rectangle -> RenderM Rectangle
attemptLeftSingleExtensionImpl r@{ x0, y0, x1, y1 }
  | x0 == 0 = pure r
  | otherwise = do
    { grid } <- get
    let
      ggi = gridToIndexed grid

      col = A.index ggi (x0 - 1)

      collidingEntries = map (filter (\{ y, tf } -> keepCollidingEntries y y0 y1 tf)) col
    maybe (pure r)
      ( \c -> do
          pure $ if (A.length c == 0) then { x0: x0 - 1, y0, x1, y1 } else r
      )
      collidingEntries

attemptRightSingleExtensionImpl :: Rectangle -> RenderM Rectangle
attemptRightSingleExtensionImpl r@{ x0, y0, x1, y1 } = do
  { width } <- ask
  if x1 == width then
    pure r
  else do
    { grid } <- get
    let
      ggi = gridToIndexed grid

      col = A.index ggi (x1 + 1)

      collidingEntries = map (filter (\{ y, tf } -> keepCollidingEntries y y0 y1 tf)) col
    maybe (pure r)
      ( \c -> do
          pure $ if (A.length c == 0) then { x0, y0, x1: x1 + 1, y1 } else r
      )
      collidingEntries

attemptUpSingleExtensionImpl :: Rectangle -> RenderM Rectangle
attemptUpSingleExtensionImpl r@{ x0, y0, x1, y1 }
  | y0 == 0 = pure r
  | otherwise = do
    { grid } <- get
    let
      ggi = gridToIndexed grid

      col = sequence $ map (flip A.index (y0 - 1)) ggi

      collidingEntries = map (filter (\{ x, tf } -> keepCollidingEntries x x0 x1 tf)) col
    maybe (pure r)
      ( \c -> do
          pure $ if (A.length c == 0) then { x0, y0: y0 - 1, x1, y1 } else r
      )
      collidingEntries

attemptDownSingleExtensionImpl :: Rectangle -> RenderM Rectangle
attemptDownSingleExtensionImpl r@{ x0, y0, x1, y1 } = do
  { height } <- ask
  if y1 == height then
    pure r
  else do
    { grid } <- get
    let
      ggi = gridToIndexed grid

      col = sequence $ map (flip A.index (y1 + 1)) ggi

      collidingEntries = map (filter (\{ x, tf } -> keepCollidingEntries x x0 x1 tf)) col
    maybe (pure r)
      ( \c -> do
          pure $ if (A.length c == 0) then { x0, y0, x1, y1: y1 + 1 } else r
      )
      collidingEntries

attemptLeftPaddingImpl :: Rectangle -> RenderM Rectangle
attemptLeftPaddingImpl incoming = do
  { paddingLeft } <- ask
  padded <- attemptExtensionImpl attemptLeftSingleExtensionImpl paddingLeft incoming
  pure $ if padded.x0 == incoming.x0 - paddingLeft then incoming else padded

attemptRightPaddingImpl :: Rectangle -> RenderM Rectangle
attemptRightPaddingImpl incoming = do
  { paddingRight } <- ask
  padded <- attemptExtensionImpl attemptRightSingleExtensionImpl paddingRight incoming
  pure $ if padded.x1 == incoming.x1 + paddingRight then incoming else padded

attemptUpPaddingImpl :: Rectangle -> RenderM Rectangle
attemptUpPaddingImpl incoming = do
  { paddingUp } <- ask
  padded <- attemptExtensionImpl attemptUpSingleExtensionImpl paddingUp incoming
  pure $ if padded.y0 == incoming.y0 - paddingUp then incoming else padded

attemptDownPaddingImpl :: Rectangle -> RenderM Rectangle
attemptDownPaddingImpl incoming = do
  { paddingDown } <- ask
  padded <- attemptExtensionImpl attemptDownSingleExtensionImpl paddingDown incoming
  pure $ if padded.y1 == incoming.y1 + paddingDown then incoming else padded

attemptLeftExtensionImpl :: Int -> Rectangle -> RenderM Rectangle
attemptLeftExtensionImpl = attemptExtensionImpl attemptLeftSingleExtensionImpl

attemptRightExtensionImpl :: Int -> Rectangle -> RenderM Rectangle
attemptRightExtensionImpl = attemptExtensionImpl attemptRightSingleExtensionImpl

attemptUpExtensionImpl :: Int -> Rectangle -> RenderM Rectangle
attemptUpExtensionImpl = attemptExtensionImpl attemptUpSingleExtensionImpl

attemptDownExtensionImpl :: Int -> Rectangle -> RenderM Rectangle
attemptDownExtensionImpl = attemptExtensionImpl attemptDownSingleExtensionImpl

storeRectangleImpl :: Rectangle -> RenderM Unit
storeRectangleImpl r@{ x0, y0, x1, y1 } =
  modify_
    ( \i ->
        i
          { rectangles = r : i.rectangles
          , grid =
            (map <<< map)
              ( \{ x, y, tf } ->
                  (x >= x0 && x <= x1 && y >= y0 && y <= y1) || tf
              )
              $ gridToIndexed i.grid
          }
    )

isAbutting :: Rectangle -> Rectangle0 -> Boolean
isAbutting r0 r1 =
  (r0.y0 == r1.y1 && ((r0.x0 > r1.x0 && r0.x0 < r1.x1) || (r0.x1 > r1.x0 && r0.x1 < r1.x1)))
    || (r0.y1 == r1.y0 && ((r0.x0 > r1.x0 && r0.x0 < r1.x1) || (r0.x1 > r1.x0 && r0.x1 < r1.x1)))
    || (r0.x0 == r1.x1 && ((r0.y0 > r1.y0 && r0.y0 < r1.y1) || (r0.y1 > r1.y0 && r0.y1 < r1.y1)))
    || (r0.x1 == r1.x0 && ((r0.y0 > r1.y0 && r0.y0 < r1.y1) || (r0.y1 > r1.y0 && r0.y1 < r1.y1)))

addColorToDisallowed :: Color -> Rectangle -> List Rectangle0 -> List Rectangle0
addColorToDisallowed color rect lr =
  map
    ( \i ->
        i
          { disallowed =
            if isAbutting rect i then color : i.disallowed else i.disallowed
          }
    )
    lr

getRectanglesImpl :: RenderM (List RectangleRGB)
getRectanglesImpl = do
  noColors <- gets _.rectangles
  let
    intermediary = map (\i -> i `R.union` { disallowed: Nil }) noColors
  go Nil intermediary
  where
  go acc Nil = pure acc

  go acc ({ x0, x1, y0, y1, disallowed } : b) = do
    { colors } <- ask
    color <- lift2 $ suchThat (elements (fromNonEmpty colors)) (flip notElem disallowed)
    go ({ x0, x1, y0, y1, color } : acc) (addColorToDisallowed color { x0, x1, y0, y1 } b)

interpret :: ProgramF ~> RenderM
interpret = case _ of
  GetBias f -> f <$> getBiasImpl
  GetTargetArea f -> f <$> getTargetAreaImpl
  GetFreePoint f -> f <$> getFreePointImpl
  AttemptLeftExtension i r f -> f <$> attemptLeftExtensionImpl i r
  AttemptRightExtension i r f -> f <$> attemptRightExtensionImpl i r
  AttemptUpExtension i r f -> f <$> attemptUpExtensionImpl i r
  AttemptDownExtension i r f -> f <$> attemptDownExtensionImpl i r
  AttemptLeftPadding r f -> f <$> attemptLeftPaddingImpl r
  AttemptRightPadding r f -> f <$> attemptRightPaddingImpl r
  AttemptUpPadding r f -> f <$> attemptUpPaddingImpl r
  AttemptDownPadding r f -> f <$> attemptDownPaddingImpl r
  StoreRectangle r u -> storeRectangleImpl r $> u
  GetRectangles f -> f <$> getRectanglesImpl

sh = 9 :: Int

defaultReaderCtxt =
  { minXBias: 1 `shl` (sh - 7)
  , minYBias: 1 `shl` (sh - 7)
  , maxXBias: 1 `shl` (sh - 4)
  , maxYBias: 1 `shl` (sh - 4)
  , minXRange: 1 `shl` (sh - 6)
  , minYRange: 1 `shl` (sh - 6)
  , maxXRange: 1 `shl` (sh - 1)
  , maxYRange: 1 `shl` (sh - 1)
  , paddingLeft: 1 `shl` (sh - 6)
  , paddingRight: 1 `shl` (sh - 6)
  , paddingUp: 1 `shl` (sh - 6)
  , paddingDown: 1 `shl` (sh - 6)
  , width: 1 `shl` (sh - 0)
  , height: 1 `shl` (sh - 0)
  , colors:
      rgb 152 221 202
        :| [ rgb 213 236 194
          , rgb 255 211 180
          , rgb 255 170 167
          , rgb 236 163 245
          , rgb 253 186 248
          , rgb 176 239 235
          , rgb 237 255 169
          ]
  } ::
    ReaderImpl

defaultStateCtxt =
  { grid: A.replicate (1 `shl` (sh - 0)) (A.replicate (1 `shl` (sh - 0)) false)
  , rectangles: empty
  } ::
    StateImpl

main :: Effect Unit
main =
  for_ (0 .. 31) \i -> do
    log $ "Calculating for " <> show i
    log $ "<<<<"
    log $ show
      $ evalGen
          ( evalStateT
              (runReaderT (foldFree interpret prog) defaultReaderCtxt)
              defaultStateCtxt
          )
          { newSeed: mkSeed i, size: 10 }
    log $ ">>>>"
