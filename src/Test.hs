module Main (main) where

--------------------------------------------------------------------------------

import           Control.Applicative
import           Control.Wire        hiding ((.))
import qualified Graphics.UI.GLFW    as GLFW
import qualified Lambency            as L
import           Linear.V2
import           Linear.V3
import           Linear.Vector

-----------------------------------------------------------------------------

screenSizeX :: Int
screenSizeX = 1024

screenSizeY :: Int
screenSizeY = 768

-----------------------------------------------------------------------------

mkV2 :: (Float, Float) -> V2 Float
mkV2 (a, b) = V2 a b

mkV3 :: (Float, Float, Float) -> V3 Float
mkV3 (a, b, c) = V3 a b c

-----------------------------------------------------------------------------

triangleMesh :: L.Mesh L.TVertex3
triangleMesh = L.Mesh {
  L.vertices = 
   let verts = mkV3 <$> [ (-1, -1, 0), (1, -1, 0), (0, 1, 0)]
       tcs = mkV2 <$> [(0, 0), (0, 0), (0, 0)]
   in  zipWith L.mkTexVertex3 verts tcs,
  L.indices = [0, 1, 2]}

polygonMesh :: L.Mesh L.TVertex3
polygonMesh = L.Mesh {
  L.vertices = 
   let verts = mkV3 <$> [ (-1, -1, 0), (1, -1, 0), (1, 0.5, 0), (0, 1, 0), (-1, 0.5, 0)]
       tcs = mkV2 <$> [(0, 0), (0, 0), (0, 0), (0, 0), (0, 0)]
   in  zipWith L.mkTexVertex3 verts tcs,
  L.indices = [0, 1, 2, 2, 3, 0, 3, 4, 0]}

-----------------------------------------------------------------------------

mkMaterial :: IO L.Material
mkMaterial = do
  t <- L.createSolidTexture (127, 0, 255, 255) 
  return $ L.createTexturedMaterial t

mkTriangle :: IO (L.Transform, L.RenderObject)
mkTriangle = do
  m <- L.createSimpleMaterial
  ro <- L.createRenderObject triangleMesh m
  return (xform, ro)
  where
  xform = L.translate (V3 500 500 (-5)) $
          L.nonuniformScale (V3 100 100 1) $
          L.identity

mkPolygon :: IO (L.Transform, L.RenderObject)
mkPolygon = do
  m <- mkMaterial
  ro <- L.createRenderObject polygonMesh m
  return (xform, ro)
  where
  xform = L.translate (V3 200 200 (-5)) $
          L.nonuniformScale (V3 100 100 1) $
          L.identity

-----------------------------------------------------------------------------

camera :: L.GameWire a L.Camera
camera = pure zero >>> (L.mk2DCam screenSizeX screenSizeY)

initGame :: IO (L.Game ())
initGame = do
  noLight <- L.createNoLight
  triangle <- mkTriangle
  polygon <- mkPolygon
  return $ L.Game
    { L.staticLights = [noLight]
    , L.staticGeometry = [triangle,polygon]
    , L.mainCamera = camera
    , L.dynamicLights = []
    , L.gameLogic = L.quitWire GLFW.Key'Q
    }

main :: IO ()
main = do
  m <- L.makeWindow screenSizeX screenSizeY "Game"
  g <- initGame
  case m of
    (Just win) -> L.run win () g
    Nothing -> return ()
  L.destroyWindow m
