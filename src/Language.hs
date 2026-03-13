module Language where

import Data.Bool ( bool )
import Data.Char ( isDigit, isAlpha, isSpace )
import Data.Maybe ( fromJust, isJust )

import Utils

{- ** コア式の抽象構文木 -}
{- | 式 -}
data Expr a
  = EVar Name                 -- ^ 変数
  | ENum Int                  -- ^ 数
  | EConstr                   -- ^ 構成子
      Tag                       -- ^ タグ
      Arity                     -- ^ アリティ
  | EAp (Expr a) (Expr a)     -- ^ 適用
  | ELet                      -- ^ let(rec)式
      IsRec                     -- ^ 再帰的か
      [Binder a]                -- ^ 定義
      (Expr a)                  -- ^ let(rec)式の本体
  | ECase                     -- ^ case式
      (Expr a)                  -- ^ 分析対象の式
      [Alter a]                 -- ^ 選択肢
  | ELam                      -- ^ λ抽象
      [a]                       -- ^ 束縛変数のリスト
      (Expr a)                  -- ^ λ抽象本体
  deriving Show

{- | コア式 -}
type CoreExpr = Expr Name

dispatchCoreExpr :: (Name -> a)
                 -> (Int -> a)
                 -> (Tag -> Arity -> a)
                 -> (CoreExpr -> CoreExpr -> a)
                 -> (IsRec -> Assoc Name CoreExpr -> CoreExpr -> a)
                 -> (CoreExpr -> [CoreAlt] -> a)
                 -> ([Name] -> CoreExpr -> a)
                 -> CoreExpr -> a
dispatchCoreExpr contEVar contENum contEConstr contEAp contELet contECase contELam expr
  = case expr of
    EVar v -> contEVar v
    ENum n -> contENum n
    EConstr tag arity -> contEConstr tag arity
    EAp a b -> contEAp a b
    ELet isrec bindings body -> contELet isrec bindings body
    ECase e alters -> contECase e alters
    ELam vars body -> contELam vars body

{- | 名前 -}
type Name = String

{- | データ構成子 -}
type Tag = Int
type Arity = Int

{- *** let 式 -}
type IsRec = Bool
recursive :: IsRec
recursive = True
nonRecursive :: IsRec
nonRecursive = False

{- | バインダ -}
type Binder a = (a, Expr a)

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [ name | (name, _) <- defns ]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [ rhs | (_, rhs) <- defns ]

{- | 選択肢 -}
type Alter a 
  = ( Tag      -- タグ
    , [a]      -- 変数名リスト
    , Expr a   -- 選択肢本体
    ) 
type CoreAlt = Alter Name

{- アトミック式の判別 -}

isAtomicExpr :: Expr a -> Bool
isAtomicExpr expr = case expr of
  EVar _ -> True
  ENum _ -> True
  _      -> False

{- サンプル式 -}
varSampleE :: CoreExpr
varSampleE = EVar "var"

numSampleE :: CoreExpr
numSampleE = ENum 57

constrSampleE10 :: CoreExpr
constrSampleE10 = EConstr 1 0
constrSampleE21:: CoreExpr
constrSampleE21 = EConstr 2 1

appInfixSampleE :: CoreExpr
appInfixSampleE = EAp (EAp (EVar "<") 
                           (EAp (EAp (EVar "+") (EVar "x")) (EVar "y")))
                      (EAp (EAp (EVar "*") (EVar "p")) (EAp (EVar "length") (EVar "xs")))

appInfixSampleE1 :: CoreExpr
appInfixSampleE1 = EAp (EAp (EVar "/") (EAp (EAp (EVar "/") (ENum 12)) (ENum 2))) (EAp (EAp (EVar "/") (ENum 6)) (ENum 3))

appInfixSampleE2 :: CoreExpr
appInfixSampleE2 = EAp (EAp (EVar "/") (EAp (EAp (EVar "*") (ENum 12)) (ENum 2))) (EAp (EAp (EVar "*") (ENum 6)) (ENum 3))

appInfixSampleE3 :: CoreExpr
appInfixSampleE3 = EAp (EAp (EVar "*") (EAp (EAp (EVar "*") (ENum 12)) (ENum 2))) (EAp (EAp (EVar "*") (ENum 6)) (ENum 3))

appInfixSampleE4 :: CoreExpr
appInfixSampleE4 = EAp (EAp (EVar "-") (EAp (EAp (EVar "*") (ENum 12)) (ENum 2))) (EAp (EAp (EVar "*") (ENum 6)) (ENum 3))

letSampleE :: CoreExpr
letSampleE = ELet recursive 
                  [ ("y", EAp (EAp (EVar "+") (EVar "x")) (ENum 1))
                  , ("z", EAp (EAp (EVar "*") (EVar "Y")) (ENum 2))
                  ]
                  (EVar "z")

caseSampleE :: CoreExpr
caseSampleE = ECase (EVar "xxs")
                    [ (1, [], ENum 0)
                    , (2, ["x", "xs"], EAp (EAp (EVar "+") (ENum 1)) (EAp (EVar "length") (EVar "xs")))
                    ]

lambdaSampleE :: CoreExpr
lambdaSampleE = ELam ["x", "y"] (EAp (EAp (EConstr 1 2) (EVar "x")) (EVar "y"))

{- プログラム -}

type Program a = [ScDefn a]
type CoreProgram = Program Name

{- スーパーコンビネータ定義 -}

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

{- サンプルプログラム -}
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
    , ("K1", ["x", "y"], EVar "y")
    , ("S",  ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
                                  (EAp (EVar "g") (EVar "x")))
    , ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x")))
    , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
    ]

