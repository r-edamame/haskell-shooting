
module Game (Game(..)) where

import Common

-- ゲームに関する型クラスの宣言
class Game g where
  -- ゲームは描画可能
  renderGame :: g -> IO ()
  -- ゲームはキー入力により更新可能
  updateGame :: KeyInputs -> g -> g

