module Main where
import Prelude
import Color (white)
import Color.Scheme.MaterialDesign (blueGrey)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import FRP.Behavior (Behavior, animate, unfold)
import FRP.Event (Event, filterMap, sampleOn_)
import FRP.Event.Keyboard (down) as Keyboard
import FRP.Event.Time (interval) as Time
import Graphics.Canvas (getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Drawing (Drawing, fillColor, filled, rectangle, render)
import Partial.Unsafe (unsafePartial)

data Axis = Horizontal | Vertical
derive instance eqAxis :: Eq Axis

forwards :: Int
forwards = 1

backwards :: Int
backwards = (-1)

type Direction = { axis :: Axis, facing :: Int }

initDirection :: Direction
initDirection = {axis: Horizontal, facing: forwards}

-- can only switch direction via a change in axis
-- i.e: disallow a 180 turn in a single update
applyDirection :: Direction -> Direction -> Direction
applyDirection old new = if old.axis == new.axis then old else new

inputs :: Event Direction
inputs = sampleOn_ (filterMap interpret Keyboard.down) (Time.interval 100) where
  interpret = case _ of
    37 -> Maybe.Just {axis: Horizontal, facing: backwards}
    38 -> Maybe.Just {axis: Vertical, facing: backwards}
    39 -> Maybe.Just {axis: Horizontal, facing: forwards}
    40 -> Maybe.Just {axis: Vertical, facing: forwards}
    _ -> Maybe.Nothing

columns :: Int
columns = 48

rows :: Int
rows = 32

type Vector = Tuple Int Int

type State =
  { direction :: Direction
  , head :: Vector
  , tail :: Map.Map Vector Int
  , health :: Int
  }

initState :: State
initState =
  { direction: initDirection
  , head: Tuple (columns / 2) (rows / 2)
  , tail: Map.empty
  , health: 15
  }

posModulo :: Int -> Int -> Int
posModulo n d = ((n `mod` d) + d) `mod` d

wrapPlusVector :: Vector -> Vector -> Vector
wrapPlusVector (Tuple lx ly) (Tuple rx ry) =
  Tuple ((lx + rx) `posModulo` columns) ((ly + ry) `posModulo` rows)

dirVector :: Direction -> Vector
dirVector dir = case dir.axis of
  Vertical -> Tuple 0 dir.facing
  Horizontal -> Tuple dir.facing 0

decay :: Int -> Maybe.Maybe Int
decay i = if i > 1 then Maybe.Just (i - 1) else Maybe.Nothing

update :: Direction -> State -> State
update input state =
  let direction = applyDirection state.direction input
      head = state.head `wrapPlusVector` dirVector direction
      tail = Map.insert head state.health (filterMap decay state.tail)
  in {direction, head, tail, health: state.health}

games :: Behavior State
games = unfold update inputs initState

scenes :: Number -> Behavior Drawing
scenes scaleFactor = pure background <> map renderGame games where
  background = filled (fillColor blueGrey) (rectangle 0.0 0.0 (toNumber columns * scaleFactor) (toNumber rows * scaleFactor))

  renderHead (Tuple x y) =
    let l = (scaleFactor * toNumber x)
        t = (scaleFactor * toNumber y)
        w = scaleFactor
        h = scaleFactor
    in filled (fillColor white) (rectangle l t w h)

  renderTail health (Tuple (Tuple x y) life) =
    let offset = (scaleFactor * (1.0 - (toNumber life / toNumber (max health life)))) / 3.0
        l = (scaleFactor * toNumber x) + offset
        t = (scaleFactor * toNumber y) + offset
        w = scaleFactor - (offset * 2.0)
        h = scaleFactor - (offset * 2.0)
    in filled (fillColor white) (rectangle l t w h)

  renderGame game =
    foldMap (renderTail game.health) (Map.toUnfoldable game.tail :: Array (Tuple Vector Int)) <>
    renderHead game.head

main = do
  mcanvas <- getCanvasElementById "canvas"
  let canvas = unsafePartial (Maybe.fromJust mcanvas)
  ctx <- getContext2D canvas
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas
  let scale = min (width / toNumber columns) (height / toNumber rows)
  _ <- setCanvasWidth width canvas
  _ <- setCanvasHeight height canvas
  _ <- animate (scenes scale) (render ctx)
  pure unit
