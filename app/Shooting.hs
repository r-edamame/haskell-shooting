
module Shooting where

import Common
import Game
import qualified Vector as Vec
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import System.Random (newStdGen,randoms)
import Data.IORef (IORef,newIORef)

-- オブジェクト識別用タグ
data ObjectTag = PlayerTag | PBulletTag | EnemyTag | EBulletTag | ServerTag deriving (Show,Eq,Enum)

-- ゲーム内オブジェクトの宣言
data Object =
  Player { tag :: ObjectTag
         , position :: Position2D
         , size :: Size2D
         , color :: ColorF
         , life :: Int
         , shotTimer :: Int
  } |
  Bullet1 { tag :: ObjectTag
          , position :: Position2D
          , size :: Size2D
          , color :: ColorF
          , speed :: Vec.Vector2D
  } |
  Enemy1 { tag :: ObjectTag
         , position :: Position2D
         , size :: Size2D
         , color :: ColorF
         , life :: Int
         , speed :: Vec.Vector2D
         , shotTimer :: Int
  } |
  -- Enemyを出現させるためのオブジェクト
  EnemyServer { tag :: ObjectTag
              , timer :: Int
  }deriving (Show)

-- オブジェクトがステージの外にいるかどうかの判定
isOutOfStage :: Shooting -> Object -> Bool
isOutOfStage game obj = px < 0 || px > sx ||
                        py < 0 || py > sy
  where
    (px,py) = position obj
    (sx,sy) = stageSize game

-- 2つのオブジェクトの衝突判定
isCollided :: Object -> Object -> Bool
isCollided g1 g2 =
    (abs $ p1x-p2x) <= ((s1x+s2x) / 2) &&
    (abs $ p1y-p2y) <= ((s1y+s2y) / 2)
  where
    (p1x,p1y) = position g1
    (s1x,s1y) = size g1
    (p2x,p2y) = position g2
    (s2x,s2y) = size g2


-- 各オブジェクトの更新用の関数
update :: KeyInputs -> Shooting -> Object -> [Object]
-- Player
update inputs game pl@(Player {}) = player' ++ bul
  where
    r = isPressed key_right inputs
    l = isPressed key_left inputs
    u = isPressed key_up inputs
    d = isPressed key_down inputs
    -- del :: Vector2d
    -- Player機の移動量
    del   = (if r then 4 else if l then -4 else 0,
             if u then -4 else if d then 4 else 0 )
    pos'  = position pl `Vec.add` del
    -- [o| ~ isCollided p o] でPlayerと衝突している敵の弾を計算し，
    -- その数だけlifeを減らす
    life' = life pl - length [o|o<-objects game, tag o==EBulletTag, isCollided pl o]
    (bul,timer') = if (shotTimer pl<=0) then
                     if isPressed key_shot inputs then
                       -- 弾が発射可能(タイマーが0 & キーが押されている)なとき弾を生成する
                       (take 4 (map (\r->Bullet1 PBulletTag (position pl) (10,10) (GL.Color3 0 1 0) (r*6-3,(abs (r-0.5))*5-8)) (randomVal game)),2)
                     else
                       ([],0)
                   else
                     ([], shotTimer pl - 1)
    -- Playerが生存しているとき座標と体力，タイマを更新した値を返す
    player' = if life'>0 then [pl {position=pos', life=life', shotTimer=timer'}] else []
                      
-- Bullet1(自機敵機兼用)
update _ game bul@(Bullet1 {}) = bullet'
  where
    pos'    = position bul `Vec.add` speed bul
    bullet' = if isOutOfStage game bul then 
                [] 
              else
                [bul {position=pos'}]

-- Enemy1
update _ game ene@(Enemy1 {}) = enemy' ++ bul
  where
    pos'   = position ene `Vec.add` speed ene
    -- 衝突したプレイヤーの弾の数だけlifeを引く
    life'  = life ene - length [o|o<-objects game, tag o==PBulletTag, isCollided o ene]
    enemy' = if isOutOfStage game ene || life'<=0 then 
               [] 
             else 
               [ene {position=pos', life=life', shotTimer=timer'}]
    timer' = let t = shotTimer ene in if t>=0 then t-1 else t
    bul    = if shotTimer ene == 0 then
               map (Bullet1 EBulletTag (position ene) (10,10) (GL.Color3 0 1 1)) [(x,y)|x<-[5,-5],y<-[5,-5]]
             else
               []

-- EnemyServer
update inputs game ser@(EnemyServer {}) = ser{timer=timer'} : enemy
  where
    sx = Vec.getx . stageSize $ game
    (enemy,timer') =  if timer ser <= 0 then
                          ([Enemy1 EnemyTag (sx*head (randomVal game), 0) (20,20) (GL.Color3 0 0 1) 4 (0,3) 50], 10)
                      else ([], timer ser -1)

-- オブジェクトの描画
render :: Object -> IO ()
render (EnemyServer {}) = return ()
render o = drawQuad (color o) (position o) (size o)

-- このゲームのデータ
data Shooting = Shooting {
  objects :: [Object],
  stageSize :: Size2D,
  randomVal :: [GL.GLfloat]
}

-- ShootingをGame型クラスのインスタンスに設定する
instance Game Shooting where
  -- Shooringを更新するときは各オブジェクトと乱数を更新する
  updateGame inputs game = game { objects=(objects game >>= update inputs game), randomVal=(tail $ randomVal game) }
  -- Shootingを描画するときは全オブジェクトをrenderする
  renderGame game = GL.clear [GL.ColorBuffer] >> mapM_ render (objects game) >> GLFW.swapBuffers

-- 新しいゲームを生成するIOアクション
newgame :: IO (IORef Shooting)
newgame = do
  gen <- newStdGen
  newIORef $ Shooting [Player PlayerTag (200,200) (20,20) (GL.Color3 1 0 0) 10 100, EnemyServer ServerTag (100)] (400,400) (randoms gen)
