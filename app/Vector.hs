
module Vector where

import qualified Graphics.Rendering.OpenGL as GL

type Vector2D = (GL.GLfloat, GL.GLfloat)
getx = fst
gety = snd

add :: Vector2D -> Vector2D -> Vector2D
add (x1,y1) (x2,y2) = (x1+x2, y1+y2)

sub :: Vector2D -> Vector2D -> Vector2D
sub (x1,y1) (x2,y2) = (x1-x2, y1-y2)

len :: Vector2D -> GL.GLfloat
len (x,y) = sqrt $ x^2 + y^2

scale :: GL.GLfloat -> Vector2D -> Vector2D
scale s (x,y) = (s*x,s*y)
