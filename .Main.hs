module Main where

import Game

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad
import System.Environment (getArgs, getProgName)
import Data.Maybe (fromMaybe)
import System.Random (newStdGen, randoms)

data Action = Action (IO Action)

data TestObject = TO {
    position :: Position2D,
    size :: Size2D,
    color :: (GL.Color3 GL.GLfloat)
  }

main = do
  args <- getArgs
  prog <- getProgName
  main' active

main' run = do
  GLFW.initialize
  GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"

  GL.shadeModel $= GL.Smooth
  GL.lineSmooth $= GL.Enabled
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth $= 1.5
  GL.clearColor $= GL.Color4 0 0 0 0

  GLFW.windowSizeCallback $= \size@(GL.Size w h) -> do
    GL.viewport $= (GL.Position 0 0, size)
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

  --game <- newIORef (TestGame [Player (Just PlayerTag) 10 (GL.Color3 1 0 0) (100,100) (50,50) 100])
  gen <- newStdGen
  game <- newIORef (TestGame [Player PlayerTag (GL.Color3 1 0 0) (100,100) (50,50) 10, EnemyServer ServerTag 100] (400,400) (randoms gen))
  run game

  GLFW.closeWindow
  GLFW.terminate


active game = loop waitForPress
  where
    keys = [ key_up
           , key_down
           , key_right
           , key_left
           , key_shot
           , key_esc
           ]
    loop action = do
      keyInputs <- zip keys <$> mapM GLFW.getKey keys

      modifyIORef game (update keyInputs)

      GL.clear [GL.ColorBuffer]
      readIORef game >>= renderGame
      GLFW.swapBuffers

      --unless (getKeyState (GLFW.SpecialKey GLFW.ESC) keyInputs) $ do
      unless (isPressed key_esc keyInputs) $ do
        Action action' <- action
        GLFW.sleep 0.001

        windowOpen <- GLFW.getParam GLFW.Opened
        unless (not windowOpen) $ 
          loop action'

    waitForPress = do
      b <- GLFW.getMouseButton GLFW.ButtonLeft
      case b of
        GLFW.Release -> return (Action waitForPress)
        GLFW.Press -> do
          (GL.Position x y) <- GL.get GLFW.mousePos
          return (Action waitForRelease)

    waitForRelease = do
      b <- GLFW.getMouseButton GLFW.ButtonLeft
      case b of
        GLFW.Release -> return (Action waitForPress)
        GLFW.Press -> return (Action waitForRelease)


{-
render lines = do
  l <- readIORef lines
  GL.clear [GL.ColorBuffer]
  GL.color $ color3 1 0 0
  GL.renderPrimitive GL.Lines $ mapM_
    (\(x,y) -> GL.vertex (vertex3 (fromIntegral x) (fromIntegral y) 0)) l
-}

render objs = do
  o <- readIORef objs
  mapM_ (\(TO pos siz col) -> drawQuads col pos siz) o

vertex3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Vertex3 GL.GLfloat
vertex3 = GL.Vertex3

color3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Color3 GL.GLfloat
color3 = GL.Color3
