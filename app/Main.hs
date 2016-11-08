
module Main where

import Common
import Game
import Shooting

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad (when,unless)


main = do
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

  game <- newgame

  mainloop game
  
  GLFW.closeWindow
  GLFW.terminate

  where
    keys = [ key_up
           , key_down
           , key_right
           , key_left
           , key_shot
           , key_esc
           , key_restart
           ]
    mainloop game = do
      -- キー入力の状態を取得
      keyInputs <- zip keys <$> mapM GLFW.getKey keys

      -- ゲームを更新
      modifyIORef game (updateGame keyInputs)

      -- ゲームを描画
      readIORef game >>= renderGame

      GLFW.sleep 0.02

      windowOpen <- GLFW.getParam GLFW.Opened
      unless (isPressed key_esc keyInputs || not windowOpen) $
        if isPressed key_restart keyInputs then do
          -- リスタート
          game' <- newgame
          mainloop game'
        else
          mainloop game
