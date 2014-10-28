{-# LANGUAGE Arrows #-}

module Main where

--------------------------------------------------------------------------------

import           Codec.Picture           as JP
import           Codec.Picture.RGBA8     as JP
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

textureFromPNG :: DynamicImage -> IO (Maybe L.Texture)
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

brightnessRGB8 :: Int -> Image PixelRGB8 -> Image PixelRGB8
brightnessRGB8 add = pixelMap brightFunction
      where up v = fromIntegral (fromIntegral v + add)
            brightFunction (PixelRGB8 r g b) =
                    PixelRGB8 (up r) (up g) (up b)

colorizeRGBA8 :: (Word8,Word8,Word8) -> Image PixelRGBA8 -> Image PixelRGBA8
colorizeRGBA8 (r,g,b) i = pixelMap colorize i
  where
  colorize p@(PixelRGBA8 r' g' b' a) = case all (<50) [r',g',b'] of
    True -> p
    False -> PixelRGBA8 r g b a

-----------------------------------------------------------------------------

mkGlyphs :: IO (L.Transform, L.RenderObject)
mkGlyphs = do
  (Just tex) <- getDataFileName ("glyphs" <.> "png") >>= L.loadTexture
  ro <- L.createRenderObject L.quad (L.createTexturedMaterial tex)
  return (xform, ro)
  where
  xform =   
    L.translate (V3 200 200 (-1)) $
    L.nonuniformScale (V3 250 250 1) $
    L.identity

mkGlyphs' :: IO (L.Transform, L.RenderObject)
mkGlyphs' = do
  f <- getDataFileName ("glyphs" <.> "png")
  i <- JP.readImageRGBA8 f
  let i' = JP.trimImage i (64,64) (0,0)
  let i'' = colorizeRGBA8 (0,255,0) i'
  Just tex <- textureFromPNG $ JP.ImageRGBA8 i''
  ro <- L.createRenderObject L.quad (L.createTexturedMaterial tex)
  return (xform, ro)
  where
  xform =   
    L.translate (V3 50 50 (-1)) $
    L.nonuniformScale (V3 100 100 1) $
    L.identity

-----------------------------------------------------------------------------

gameCam :: L.GameWire () L.Camera
gameCam = pure zero >>> (L.mk2DCam screenWidth screenHeight)

-----------------------------------------------------------------------------

loadGame :: IO (L.Game Int)
loadGame = do
  glyphs <- mkGlyphs
  glyphs' <- mkGlyphs'
  nolight <- L.createNoLight
  return $ L.Game 
    { L.staticLights = [nolight]
    , L.staticGeometry = [glyphs,glyphs']
    , L.mainCamera = gameCam
    , L.dynamicLights = []
    , L.gameLogic = L.quitWire GLFW.Key'Q
    }

-----------------------------------------------------------------------------

main :: IO ()
main = L.runWindow screenWidth screenHeight "Matrix Demo" 0 loadGame
