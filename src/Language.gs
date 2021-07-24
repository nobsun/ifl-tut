{- |
module:       Language
copyright:    (c) Nobuo Yamashita 2021
license:      BSD-3
maintainer:   nobsun@sampou.org
stability:    experimental
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language where

-- import Data.List

{- | コア言語の式 -}

data Expr a
  = EVar Name                 -- ^ 変数
  | ENum Int                  -- ^ 数
  | EConstr                   -- ^ 構成子
      Int                       -- ^ タグ
      Int                       -- ^ アリティ
  | EAp (Expr a) (Expr a)     -- ^ 適用
  | ELet                      -- ^ let(rec)式
      IsRec                     -- ^ 再帰的か
      [(a, Expr a)]             -- ^ 定義
      (Expr a)                  -- ^ let(rec)式の本体
  | ECAse                     -- ^ case式
      (Expr a)                  -- ^ 分析対象の式
      [Alter a]                 -- ^ 選択肢
  | ELam                      -- ^ λ抽象
      [a]                       -- ^ 束縛変数のリスト
      (Expr a)                  -- ^ λ抽象本体

type CoreExpr = Expr Name

type Name = String

type IsRec = Bool
recursive :: IsRec
recursive = True
nonRecursive :: IsRec
nonRecursive = False

{- バインダ -}

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [ name | (name, rhs) <- defns ]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [ rhs | (name, rhs) <- defns ]

{- 選択肢 -}

type Alter a = (Int, [a], Expr a)
type CoreAlter = Alter Name

{- アトミック式の判別 -}

isAtomicExpr :: Expr a -> Bool
isAtomicExpr e = case e of
  EVar _ -> True
  ENum _ -> True
  _      -> False

{- プログラム -}

type Program a = [ScDefn a]
type CoreProgram = Program Name

{- スーパーコンビネータ定義 -}

type ScDefn a = (Name, [a], Expr a)
type CoreScDefns = ScDefn Name

{- サンプル -}
sampleProg :: CoreProgram
sampleProg
  = [ ("main",   [],    EAp (EVar "double") (ENum 21))
    , ("double", ["x"], EAp (EAp (EVar "+") (EVar "x")) (EVar "x"))
    ]

{- コア言語の標準プレリュード -}

preludeCode :: String
preludeCode = unlines
  [ "I x = x ;"
  , "K x y = x ;"
  , "K1 x y = y ;"
  , "S f g x = f x (g x) ;"
  , "compose f g x = f (g x) ;"
  , "twice f = compose f f"
  ]

preludeDefs :: CoreProgram
preludeDefs 
  = [ ("I",  ["x"], EVar "x")
    , ("K",  ["x", "y"], EVar "x")
    , ("K1", ["x", "y"], EVar "x")
    , ("S",  ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
                                  (EAp (EVar "g") (EVar "x")))
    , ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x")))
    , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
    ]

{- プリティプリンタ -}

pprint :: CoreProgram -> String
pprint = undefined

{- -}
pprExpr :: CoreExpr -> String
pprExpr e = case e of
  EVar v    -> v
  ENum n    -> show n
  EAp e1 e2 -> pprExpr e1 ++ " " ++ pprAExpr e2
  _         -> error "not implemented"

pprAExpr :: CoreExpr -> String
pprAExpr e = case e of
  e | isAtomicExpr e -> pprExpr e
    | otherwise      -> "(" ++ pprExpr e ++ ")"
-- -}

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n (repeat e2))

