{-# LANGUAGE Arrows #-}

module Main where

--------------------------------------------------------------------------------

import qualified Codec.Picture           as JP
import qualified Codec.Picture.RGBA8     as JP
import           Control.Wire            hiding ((.))
import qualified Data.Vector.Storable    as V
import           Data.Word
import qualified Graphics.UI.GLFW        as GLFW
import qualified Lambency                as L
import           Linear
import           System.FilePath

import           Paths_lambency_examples

-----------------------------------------------------------------------------

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

-----------------------------------------------------------------------------

textureFromPNG :: JP.DynamicImage -> IO (Maybe L.Texture)
textureFromPNG img = case img of
  (JP.ImageRGBA8 (JP.Image width height dat)) -> do
    tex <- V.unsafeWith dat $ \ptr ->
      L.initializeTexture ptr (fromIntegral width, fromIntegral height) L.RGBA8
    return $ Just tex
  (JP.ImageRGB8 (JP.Image width height dat)) -> do
    tex <- V.unsafeWith dat $ \ptr ->
      L.initializeTexture ptr (fromIntegral width, fromIntegral height) L.RGB8
    return $ Just tex
  _ -> return Nothing

toRGBA8 :: JP.PixelRGB8 -> JP.PixelRGBA8
toRGBA8 (JP.PixelRGB8 255 255 255) = JP.PixelRGBA8 0 0 0 127
toRGBA8 (JP.PixelRGB8 r g b) = JP.PixelRGBA8 r g b 127

-----------------------------------------------------------------------------

mkMario :: IO (L.Transform, L.RenderObject)
mkMario = do
  f <- getDataFileName ("mario-stand-right" <.> "gif")
  Right i <- JP.readGifImages f
  Just tex <- textureFromPNG $ JP.ImageRGBA8 $ JP.pixelMap toRGBA8 $ head i
  ro <- L.createRenderObject L.quad (L.createTexturedMaterial tex)
  return (xform, ro)
  where
  xform =   
    L.translate (V3 100 100 (-1)) $
    L.nonuniformScale (V3 300 300 1) $
    L.identity

-----------------------------------------------------------------------------

gameCam :: L.GameWire () L.Camera
gameCam = pure zero >>> (L.mk2DCam screenWidth screenHeight)

-----------------------------------------------------------------------------

loadGame :: IO (L.Game Int)
loadGame = do
  mario <- mkMario
  nolight <- L.createNoLight
  return $ L.Game 
    { L.staticLights = [nolight]
    , L.staticGeometry = [mario]
    , L.mainCamera = gameCam
    , L.dynamicLights = []
    , L.gameLogic = L.quitWire GLFW.Key'Q
    }

-----------------------------------------------------------------------------

main :: IO ()
main = L.runWindow screenWidth screenHeight "Mario Demo" 0 loadGame

-----------------------------------------------------------------------------

{- Mario code from elm examples
---

import Keyboard
import Window

-- MODEL
mario = { x=0, y=0, vx=0, vy=0, dir="right" }


-- UPDATE -- ("m" is for Mario)
jump {y} m = if y > 0 && m.y == 0 then { m | vy <- 5 } else m
gravity t m = if m.y > 0 then { m | vy <- m.vy - t/4 } else m
physics t m = { m | x <- m.x + t*m.vx , y <- max 0 (m.y + t*m.vy) }
walk {x} m = { m | vx <- toFloat x
                 , dir <- if | x < 0     -> "left"
                             | x > 0     -> "right"
                             | otherwise -> m.dir }

step (dt, keys) =
  jump keys >> gravity dt >> walk keys >> physics dt


-- DISPLAY
render (w',h') mario =
  let (w,h) = (toFloat w', toFloat h')
      verb = if | mario.y  >  0 -> "jump"
                | mario.vx /= 0 -> "walk"
                | otherwise     -> "stand"
      src = "/imgs/mario/" ++ verb ++ "/" ++ mario.dir ++ ".gif"
  in collage w' h'
      [ rect w h  |> filled (rgb 174 238 238)
      , rect w 50 |> filled (rgb 74 163 41)
                  |> move (0, 24 - h/2)
      , toForm (image 35 35 src) |> move (mario.x, mario.y + 62 - h/2)
      ]

-- MARIO
input = let delta = lift (\t -> t/20) (fps 25)
        in  sampleOn delta (lift2 (,) delta Keyboard.arrows)

main = lift2 render Window.dimensions (foldp step mario input)
-}
