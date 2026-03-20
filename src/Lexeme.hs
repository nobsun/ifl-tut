-- # Lex
module Lexeme
    where

data Lexeme
    = LRsv String  -- ^ 予約語
    | LIde String  -- ^ 識別子
    | LBop String  -- ^ 二項演算子
    | LNum Int     -- ^ 数
    | LLambda      -- ^ λ
    | LOpar        -- ^ 開き括弧
    | LCpar        -- ^ 閉じ括弧
    | LObrc        -- ^ 開き波括弧
    | LCbrc        -- ^ 閉じ波括弧
    | LRarr        -- ^ 右矢印
    | LComm        -- ^ コンマ
    | LSemi        -- ^ セミコロン
    | LCmnt String -- ^ コメント
    | LUnkn Char   -- ^ そのほか
    deriving (Eq, Show)

fromLexeme :: Lexeme -> String
fromLexeme = \ case
    LRsv s  -> s
    LIde s  -> s
    LBop s  -> s
    LNum n  -> show n
    LLambda -> "\\"
    LOpar   -> "("
    LCpar   -> ")"
    LObrc   -> "{"
    LCbrc   -> "}"
    LRarr   -> "->"
    LComm   -> ","
    LSemi   -> ";"
    LCmnt s -> s
    LUnkn c -> [c]
