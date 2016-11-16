
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

scale :: Vector2D -> GL.GLfloat -> Vector2D
scale (x,y) s = (s*x,s*y)

unit :: Vector2D -> Vector2D
unit v = v `scale` recip (len v)
