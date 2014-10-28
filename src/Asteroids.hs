{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

--------------------------------------------------------------------------------

import           Control.Lens
import           Control.Monad     (liftM)
import           Control.Wire      hiding ((.))
import           Data.Fixed
import           Data.Foldable     (forM_)
import           Data.IntMap       (IntMap)
import qualified Data.IntMap       as I
import           Data.Set          (Set)
import qualified Data.Set          as S
import           FRP.Netwire.Input
import qualified Graphics.UI.GLFW  as GLFW
import qualified Lambency          as L
import           Linear

import           Paths_lambency_examples

--------------------------------------------------------------------------------

data Object = Object
  { _objPos :: V2 Float
  , _objDir :: Float
  , _objVel :: V2 Float
  , _objRot :: Float
  }

data Ship = Ship 
  { _shipObj :: Object
  }

mkShip :: V2 Float -> Float -> V2 Float -> Float -> Ship
mkShip p d v r = Ship $ Object p d v r

data Bullet = Bullet
  { _bltObj :: Object
  , _bltLife :: Float
  }

mkBullet :: V2 Float -> Float -> V2 Float -> Float -> Float -> Bullet
mkBullet p d v r l = Bullet (Object p d v r) l

type Bullets = IntMap Bullet

data Asteroid = Asteroid
  { _astObj :: Object
  }

mkAsteroid :: V2 Float -> Float -> V2 Float -> Float -> Asteroid
mkAsteroid p d v r = Asteroid $ Object p d v r

type Asteroids = IntMap Asteroid

type Collisions = Set Int 

makeLenses ''Object
makeLenses ''Ship
makeLenses ''Bullet
makeLenses ''Asteroid

-----------------------------------------------------------------------------

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

scoreX :: Int 
scoreX = screenWidth `div` 2

scoreY :: Int
scoreY = 10

shipSize :: Float
shipSize = 10

shipMaxSpeed :: Float
shipMaxSpeed = 200

shipDecaySpeed :: Float
shipDecaySpeed = 2.5

shipRotationSpeed :: Float
shipRotationSpeed = 2

startShip :: Ship
startShip = Ship $ Object (0.5 *^ (vi2f2 $ V2 screenWidth screenHeight)) 0 0 0

bulletWidth :: Float
bulletWidth = 1

bulletLength :: Float
bulletLength = 5

bulletSpeed :: Float
bulletSpeed = 400

bulletLife :: Float
bulletLife = 1

fireInterval :: Float
fireInterval = 0.1

asteroidSize :: Float
asteroidSize = 25

asteroidSpeed :: Float
asteroidSpeed = 50

asteroidRotationSpeed :: Float
asteroidRotationSpeed = 1

startAsteroids :: Asteroids
startAsteroids = I.fromList $ zip [1..]
  [ mkAsteroid (0.5 *^ (vi2f2 $ V2 0 screenHeight)) 0 (V2 asteroidSpeed 0) asteroidRotationSpeed
  , mkAsteroid (0.5 *^ (vi2f2 $ V2 screenWidth 0)) (pi/2) (V2 0 asteroidSpeed) asteroidRotationSpeed
  , mkAsteroid (V2 0 0) (pi/2) (V2 asteroidSpeed asteroidSpeed) asteroidRotationSpeed
  , mkAsteroid (vi2f2 $ V2 screenWidth screenHeight) (pi/2) (V2 (-asteroidSpeed) (-asteroidSpeed)) asteroidRotationSpeed
  ]

-----------------------------------------------------------------------------

bool :: a -> a -> Bool -> a
bool t f v = if v then t else f

dupW :: L.GameWire a (a,a) 
dupW = mkId &&& mkId

wrap :: V2 Float -> V2 Float
wrap (V2 x y) = V2
  (mod' x (fromIntegral screenWidth))
  (mod' y (fromIntegral screenHeight))

vi2f2 :: (Integral a, Floating f) => V2 a -> V2 f
vi2f2 = fmap fromIntegral

mkV2 :: (Float, Float) -> V2 Float
mkV2 (a, b) = V2 a b

mkV3 :: (Float, Float, Float) -> V3 Float
mkV3 (a, b, c) = V3 a b c

-----------------------------------------------------------------------------

triangle :: L.Mesh L.TVertex3
triangle = L.Mesh {
  L.vertices = 
   let verts = mkV3 <$> [ (-1, -1, 0), (1, -1, 0), (0, 1, 0)]
       tcs = mkV2 <$> [(0, 0), (0, 0), (0, 0)]
   in  zipWith L.mkTexVertex3 verts tcs,
  L.indices = [0, 1, 2]}

quad :: L.Mesh L.TVertex3
quad = L.Mesh
  { L.vertices = zipWith L.mkTexVertex3 (map texToVert texcoords) texcoords
  , L.indices = [0, 2, 1, 1, 2, 3]
  }
  where
  texcoords = [ V2 x y | x <- [-1, 1], y <- [1, -1] ]
  texToVert (V2 x y) = V3 x (negate y) 0

-----------------------------------------------------------------------------

-- controlShipW :: L.GameWire Ship Ship
-- controlShipW = mkGen $ \ds (Ship (Object p d v rot)) -> do
--   t <- keyIsPressed GLFW.Key'Up 
--   l <- bool 1 0 <$> keyIsPressed GLFW.Key'Left
--   r <- bool (-1) 0 <$> keyIsPressed GLFW.Key'Right 
--   let d' = d + (dtime ds) * (l+r) * shipRotationSpeed
--       v' = bool (shipMaxSpeed *^ angle (d' + pi/2)) v t
--   return (Right (mkShip p d' v' rot), controlShipW)

controlShipW :: L.GameWire Ship Ship
controlShipW = mkGen $ \ds ship -> do
  t <- keyIsPressed GLFW.Key'Up 
  l <- keyIsPressed GLFW.Key'Left
  r <- keyIsPressed GLFW.Key'Right 
  let
    d   = ship ^. (shipObj . objDir)
    v   = ship ^. (shipObj . objVel)
    rot = bool 1 0 l + bool (-1) 0 r
    d'  = d + (dtime ds) * rot * shipRotationSpeed
    v'  = bool (shipMaxSpeed *^ angle (d' + pi/2)) v t
    ship' = ship 
      & (shipObj . objDir) .~ d'
      & (shipObj . objVel) .~ v'
  return (Right ship', controlShipW)

moveShipW :: L.GameWire Ship Ship
moveShipW = mkSF $ \ds (Ship (Object p d v r)) ->
  let dt = dtime ds
      vn = norm v
      vd = normalize v
      v' = vd ^* max 0 (vn - shipDecaySpeed)
      p' = wrap (p ^+^ (dt *^ v))
  in  (mkShip p' d v' r, moveShipW)

transformShip :: Ship -> L.Transform
transformShip (Ship (Object (V2 x y) d _ _)) = 
  L.rotate (axisAngle (V3 0 0 (-1)) d) $
  L.translate (V3 x y (-1)) $
  L.nonuniformScale (V3 shipSize shipSize 1) $
  L.identity

renderShipW :: L.RenderObject -> L.GameWire Ship Ship
renderShipW ro = mkGen_ $ \s -> do
  L.addRenderAction (transformShip s) ro
  return (Right s)

shipW :: L.RenderObject -> L.GameWire Ship Ship
shipW ro = 
  controlShipW
  >>> moveShipW
  >>> renderShipW ro

-----------------------------------------------------------------------------

controlBulletsW :: L.GameWire (Ship,Bullets) Bullets
controlBulletsW = controlBulletsW' 0 1
  where
  controlBulletsW' :: Float -> Int -> L.GameWire (Ship,Bullets) Bullets
  controlBulletsW' fdt i = mkGen $ \ds (Ship (Object p d _ _),bs) -> do 
    fire <- keyIsPressed GLFW.Key'Space
    let
      dt = dtime ds
      fdt' = max 0 (fdt - dt)
      (bs',fdt'',i') = 
        if fire && fdt' == 0 then
          let v' = bulletSpeed *^ angle (d + pi/2)
          in  (I.insert i (mkBullet p d v' 0 bulletLife) bs,fireInterval,succ i)
        else (bs,fdt',i)
    return (Right bs', controlBulletsW' fdt'' i')

moveBulletsW :: L.GameWire Bullets Bullets
moveBulletsW = mkSF $ \ds bs -> 
  let moveBullet (Bullet (Object p d v r) l) = 
        mkBullet (p ^+^ (dt *^ v)) d v r (l-dt)
      dt = dtime ds
  in  (I.filter ((>0) . _bltLife) (I.map moveBullet bs), moveBulletsW)

transformBullet :: Bullet -> L.Transform
transformBullet (Bullet (Object (V2 x y) d _ _) _) = 
  L.rotate (axisAngle (V3 0 0 (-1)) d) $
  L.translate (V3 x y (-1)) $
  L.nonuniformScale (V3 bulletWidth bulletLength 1) $
  L.identity

renderBulletsW :: L.RenderObject -> L.GameWire Bullets Bullets
renderBulletsW ro = mkGen_ $ \bs -> do
  forM_ bs $ \b -> L.addRenderAction (transformBullet b) ro
  return (Right bs)

bulletsW :: L.RenderObject -> L.GameWire Ship Bullets
bulletsW ro = loop $
  second (delay I.empty) 
  >>> controlBulletsW
  >>> moveBulletsW
  >>> renderBulletsW ro
  >>> dupW

-----------------------------------------------------------------------------

collideAsteroids :: L.GameWire (Collisions,Asteroids) Asteroids
collideAsteroids = mkSF_ $ \ (cs,as) -> S.foldl (flip I.delete) as cs

moveAsteroid :: Float -> Asteroid -> Asteroid
moveAsteroid dt (Asteroid (Object p d v r)) =
  let p' = wrap (p ^+^ (dt *^ v))
  in  mkAsteroid p' (d + r*dt) v r

moveAsteroidsW :: L.GameWire Asteroids Asteroids
moveAsteroidsW = mkSF $ \ds as -> 
  (I.map (moveAsteroid (dtime ds)) as, moveAsteroidsW)

transformAsteroid :: Asteroid -> L.Transform
transformAsteroid (Asteroid (Object (V2 x y) d _ _)) = 
  L.rotate (axisAngle (V3 0 0 (-1)) d) $
  L.translate (V3 x y (-1)) $
  L.nonuniformScale (V3 asteroidSize asteroidSize 1) $
  L.identity

renderAsteroidsW :: L.RenderObject -> L.GameWire Asteroids Asteroids
renderAsteroidsW ro = mkGen_ $ \as -> do
  forM_ as $ \a -> L.addRenderAction (transformAsteroid a) ro
  return (Right as)

asteroidsW :: L.RenderObject -> L.GameWire Collisions Asteroids
asteroidsW ro = loop $
  second (delay startAsteroids) 
  >>> collideAsteroids
  >>> moveAsteroidsW
  >>> renderAsteroidsW ro
  >>> dupW

-----------------------------------------------------------------------------

collisionsW :: L.GameWire (Bullets,Asteroids) (Collisions,Collisions)
collisionsW = mkSF_ $ \(bs,as) -> 
  let blts = I.toList $ I.map _bltObj bs
      asts = I.toList $ I.map _astObj as
      bas = (,) <$> blts <*> asts
      bas' = filter (\((_,bo),(_,ao)) -> collides bo ao) bas
      (bs',as') = unzip bas'
      bs'' = map fst bs'
      as'' = map fst as'
  in (S.fromList bs'',S.fromList as'')

collides :: Object -> Object -> Bool
collides (Object (V2 x1 y1) _ _ _) (Object (V2 x2 y2) _ _ _) =
  abs (x1 - x2) < 10 && abs (y1 - y2) < asteroidSize

-----------------------------------------------------------------------------

renderScore :: L.Font -> Int -> L.GameMonad ()
renderScore f s = do
  let score = show s
      scoreLen = round $ L.stringWidth f score
      scoreLoc = vi2f2 $ V2 (scoreX - scoreLen) scoreY
  L.renderUIString f score scoreLoc

scoreW :: L.Font -> L.GameWire (Int,Collisions) Int
scoreW f = mkGen_ $ \(s,cs) -> do
  let s' = s + S.size cs
  renderScore f s' 
  return $ Right s'

-----------------------------------------------------------------------------

gameWire :: L.Font -> L.RenderObject -> L.RenderObject -> L.RenderObject -> L.GameWire Int Int
gameWire font sro bro aro = proc score -> do
  rec ship' <- shipW sro -< ship
      ship <- delay startShip -< ship'
      bullets <- bulletsW bro -< ship'
      (_,astColls) <- delay (S.empty,S.empty) <<< collisionsW -< (bullets,asteroids) 
      asteroids <- asteroidsW aro -< astColls 
      score' <- scoreW font -< (score,astColls) 
  returnA -< score' 

gameLogic :: L.Font -> L.RenderObject -> L.RenderObject -> L.RenderObject -> L.GameWire Int Int
gameLogic font sro bro aro = 
  when (< I.size startAsteroids)
  >>> gameWire font sro bro aro
  >>> L.quitWire GLFW.Key'Q

-----------------------------------------------------------------------------

gameCam :: L.GameWire () L.Camera
gameCam = pure zero >>> (L.mk2DCam screenWidth screenHeight)

-----------------------------------------------------------------------------

loadGame :: IO (L.Game Int)
loadGame = do
  white <- L.createSolidTexture (255, 255, 255, 255)
  let whiteM = L.createTexturedMaterial white
  ship <- L.createRenderObject triangle whiteM
  bullet <- L.createRenderObject quad whiteM
  asteroid <- L.createRenderObject quad whiteM
  nolight <- L.createNoLight
  font <- getDataFileName ("kenpixel.ttf") >>= L.loadTTFont 36 (V3 1 1 1)
  return $ L.Game 
    { L.staticLights = [nolight]
    , L.staticGeometry = []
    , L.mainCamera = gameCam
    , L.dynamicLights = []
    , L.gameLogic = gameLogic font ship bullet asteroid
    }

-----------------------------------------------------------------------------

main :: IO ()
main = L.runWindow screenWidth screenHeight "Asteroids Demo" 0 loadGame


