
module Common where

import Data.Maybe (maybe)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Vector as Vec

-- キー入力のデータ型
type KeyInputs = [(GLFW.Key, GLFW.KeyButtonState)]

type ColorF = GL.Color3 GL.GLfloat
type Position2D = Vec.Vector2D
type Size2D = Vec.Vector2D

-- キーの定義
key_up = GLFW.CharKey 'W'
key_down = GLFW.CharKey 'S'
key_right = GLFW.CharKey 'D'
key_left = GLFW.CharKey 'A'
key_shot = GLFW.CharKey 'J'
key_esc = GLFW.SpecialKey GLFW.ESC
key_restart = GLFW.CharKey 'R'

-- 四角形を描画する
drawQuad :: ColorF -> Position2D -> Size2D -> IO ()
drawQuad c (px,py) (sx,sy) = do
  let hsx = sx/2
      hsy = sy/2
  GL.color c
  GL.renderPrimitive GL.Quads $ mapM_ GL.vertex [ GL.Vertex2 (px-hsx) (py-hsy)
                                                , GL.Vertex2 (px+hsx) (py-hsy)
                                                , GL.Vertex2 (px+hsx) (py+hsy)
                                                , GL.Vertex2 (px-hsx) (py+hsy)
                                                ]

-- キーが押されているかどうかを調べる
isPressed :: GLFW.Key -> KeyInputs -> Bool
isPressed key inputs = maybe False (==GLFW.Press) $ lookup key inputs
