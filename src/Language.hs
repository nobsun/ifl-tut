module Language where

import Data.Bool ( bool )
import Data.Char ( isDigit, isAlpha, isSpace )
import Data.Maybe ( fromJust, isJust )

import Iseq
import Parser hiding ( pVar, takeFirstParse )
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
      [(a, Expr a)]             -- ^ 定義
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
type Binder a b = (a, b)
bindersOf :: [Binder a b] -> [a]
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

{- プリティプリンタ -}

pprint :: CoreProgram -> String
pprint = iDisplay . pprProgram

pprProgram :: CoreProgram -> IseqRep
pprProgram prog = iInterleave (iAppend (iStr " ;") iNewline) (map pprSc prog)

pprSc :: CoreScDefn -> IseqRep
pprSc (name, args, body)
  = iConcat [ iStr name, if null args then iNil else iAppend iSpace (pprArgs args),
              iStr " = ", iIndent (pprExpr 0 body) ]

pprArgs :: [Name] -> IseqRep
pprArgs args = iConcat (map (iAppend iSpace . iStr) args)

-- | Pretty printer for CoreExpr

binOps :: [(Name, Fixity)]
binOps = [ ("*", (5, R))
         , ("/", (5, N))
         , ("+", (4, R))
         , ("-", (4, N))
         , ("==", (3, N)), ("/=", (3, N))
         , ("<", (3, N)), ("<=", (3, N))
         , (">=", (3, N)), (">", (3, N))
         , ("&&", (2, N))
         , ("||", (1, N)) ]

type Fixity = (Precedence, Associativity)
type Precedence = Int
data Associativity
  = N | L | R | O deriving (Eq, Ord, Show)


{- |
>>> es1 = [varSampleE, numSampleE]
>>> es2 = [constrSampleE10, constrSampleE21]
>>> es3 = [appInfixSampleE, appInfixSampleE1, appInfixSampleE2, appInfixSampleE3, appInfixSampleE4]
>>> es4 = [letSampleE, caseSampleE, lambdaSampleE]
>>> es = concat [es1,es2,es3,es4]
>>> putStrLn $ iDisplay $ iLayn $ map (pprExpr 0) es
   1) var
   2) 57
   3) Pack{1,0}
   4) Pack{2,1}
   5) x + y < p * length xs
   6) (12 / 2) / (6 / 3)
   7) (12 * 2) / (6 * 3)
   8) (12 * 2) * (6 * 3)
   9) 12 * 2 - 6 * 3
  10) letrec
        y = x + 1;
        z = Y * 2
      in z
  11) case xxs of
        <1> -> 0;
        <2> x xs -> 1 + length xs
  12) \ x y -> Pack{1,2} x y
<BLANKLINE>
-}
pprExpr :: Precedence -> CoreExpr -> IseqRep
pprExpr p expr = case expr of
  EVar v -> iStr v
  ENum n -> iNum n
  EConstr tag arity
    -> iConcat [ iStr "Pack{", iNum tag, iStr ",", iNum arity, iStr "}" ]
  EAp (EAp (EVar op) e1) e2
    | isJust mfx -> bool id iParen (p >= p') infixexpr
    where
      mfx = lookup op binOps
      (p', _) = fromJust mfx
      infixexpr = iConcat [ pprExpr p' e1
                          , iSpace, iStr op, iSpace
                          , pprExpr p' e2 ]
  EAp e1 e2 -> bool id iParen (p > 6) appexpr
    where
      appexpr = iConcat [ pprExpr 6 e1, iStr " ", pprExpr 7 e2]
  ELet isrec defns body
    -> bool id iParen (p > 0) letexpr
    where
      letexpr = iConcat [ iStr keyword, iNewline
                        , iStr "  ", iIndent (pprDefns defns), iNewline
                        , iStr "in ", pprExpr 0 body ]
      keyword | isrec     = "letrec"
              | otherwise = "let"
  ECase e alts
    -> bool id iParen (p > 0) caseexpr
    where
      caseexpr = iConcat [ iStr "case ", iIndent (pprExpr 0 e), iStr " of", iNewline,
                           iStr "  ", iIndent (iInterleave iNl (map pprAlt alts)) ]
      iNl = iConcat [ iStr ";", iNewline ]
      pprAlt (tag, args, rhs)
        = iConcat [ iStr "<", iNum tag, iStr ">",
                    pprArgs args, iStr " -> ",
                    iIndent (pprExpr 0 rhs) ]
  ELam args body
    -> bool id iParen (p > 0) lambda
    where
      lambda = iConcat [ iStr "\\", pprArgs args, iStr " -> ", iIndent (pprExpr 0 body) ]

pprDefns :: [(Name, CoreExpr)] -> IseqRep
pprDefns defns
  = iInterleave sep (map pprDefn defns)
    where
      sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (Name, CoreExpr) -> IseqRep
pprDefn (name, expr)
  = iConcat [ iStr name, iStr " = ", iIndent (pprExpr 0 expr) ]

{- 構文解析器 -}

clex :: Loc -> String -> [Token]
clex i ('-' : '-' : cs) = clex i (dropWhile ('\n' /=) cs)
clex i ('-' : '>' : cs) = (i, "->") : clex i cs
clex i (c1 : c2 : cs)
  | isJust (lookup [c1,c2] binOps) = (i,[c1,c2]) : clex i cs
clex i ('\n' : cs) = clex (succ i) cs
clex i (c : cs)
  | isDigit c = case span isDigit cs of 
      (ds, rs) -> (i, c : ds) : clex i rs
  | isAlpha c = case span isIdChar cs of
      (vs, rs) -> (i, c : vs) : clex i rs
  | isSpace c = clex i cs
  | otherwise = (i,[c]) : clex i cs 
clex _ "" = []

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || c == '_' || c == '\''

syntax :: [Token] -> CoreProgram
syntax = takeFirstParse . pProgram

parse :: String -> CoreProgram
parse = syntax . clex 1

parseSc :: String -> CoreScDefn
parseSc = takeFirstParse . pSc . clex 1

parseExpr :: String -> CoreExpr
parseExpr = takeFirstParse . pExpr . clex 1

pVar :: Parser String
pVar = pSat ((&&) . isAlpha . head <*> (`notElem` keywords))

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

{- コア言語の構文解析 -}

takeFirstParse :: Show a => [(a, [Token])] -> a
takeFirstParse res = case res of
  (x, []) : _ 
    -> x
  ((_, (i,_) : _): ps)
    -> case ps of
      _ : _ -> takeFirstParse ps
      []    -> error $ "syntax error at line " ++ show i
  _ -> error  $ "syntax error at line 1: " ++ show res

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = mkSc <$$> pVar <**> pMunch pVar <** pLit "=" <**> pExpr

mkSc :: Name -> [Name] -> CoreExpr -> (Name, [Name], CoreExpr)
mkSc = (,,)

{- コア式の構文解析器 -}
pExpr :: Parser CoreExpr
pExpr =  pELet `pAlt` (pECase `pAlt` (pELam `pAlt` (pAexpr `pAlt` pExpr1)))

pELet :: Parser CoreExpr
pELet = ELet <$$> pIsRec  <**> pDefns <** pLit "in" <**> pExpr

pIsRec :: Parser IsRec
pIsRec = pLit "let" **> pEmpty nonRecursive
  `pAlt` pLit "letrec" **> pEmpty recursive

pDefns :: Parser [(Name, CoreExpr)]
pDefns = pOneOrMoreWithSep pDefn (pLit ";")

pDefn :: Parser (Name, CoreExpr)
pDefn = (,) <$$> pVar <** pLit "=" <**> pExpr

pECase :: Parser CoreExpr
pECase = ECase <$$ pLit "case" <**> pExpr <** pLit "of" <**> pAlters

pAlters :: Parser [CoreAlt]
pAlters = pOneOrMoreWithSep pAlter (pLit ";")

pAlter :: Parser CoreAlt
pAlter = (,,) <$$> pTag <**> pMunch pVar <** pLit "->" <**> pExpr

pTag :: Parser Tag
pTag = pLit "<" **> pNum <** pLit ">"

pELam :: Parser CoreExpr
pELam = ELam <$$ pLit "\\" <**> pMunch1 pVar <** pLit "->" <**> pExpr

pAexpr :: Parser CoreExpr
pAexpr = pEVar `pAlt` pENum `pAlt` pEConstr
  `pAlt` pParen pExpr

pParen :: Parser CoreExpr -> Parser CoreExpr
pParen p = pLit "(" **> p <** pLit ")"

pEVar :: Parser CoreExpr
pEVar = EVar <$$> pVar

pENum :: Parser CoreExpr
pENum = ENum <$$> pNum

pEConstr :: Parser CoreExpr
pEConstr = uncurry EConstr <$$ pLit "Pack" <** pLit "{" <**> pTagArity <** pLit "}"

pTagArity :: Parser (Tag, Arity)
pTagArity = (,) <$$> pNum <** pLit "," <**> pNum

pExpr1 :: Parser CoreExpr
pExpr1 = assembleOp <$$> pExpr2 <**> pExpr1c

data PartialExpr
  = NoOp
  | FoundOp Name CoreExpr

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e pe = case pe of
  NoOp          -> e
  FoundOp op e' -> EAp (EAp (EVar op) e) e'

pExpr1c :: Parser PartialExpr
pExpr1c = FoundOp <$$> pLit "||" <**> pExpr1
   `pAlt` pEmpty NoOp

pExpr2 :: Parser CoreExpr
pExpr2 = assembleOp <$$> pExpr3 <**> pExpr2c

pExpr2c :: Parser PartialExpr
pExpr2c = FoundOp <$$> pLit "&&" <**> pExpr2
   `pAlt` pEmpty NoOp

pExpr3 :: Parser CoreExpr
pExpr3 = assembleOp <$$> pExpr4 <**> pExpr3c

pExpr3c :: Parser PartialExpr
pExpr3c = FoundOp <$$> pRelop <**> pExpr4
   `pAlt` pEmpty NoOp

pRelop :: Parser String
pRelop = pSat (`elem` relops)

relops :: [String]
relops = ["<", "<=", "==", "/=", ">=", ">"]

pExpr4 :: Parser CoreExpr
pExpr4 = assembleOp <$$> pExpr5 <**> pExpr4c

pExpr4c :: Parser PartialExpr
pExpr4c = FoundOp <$$> pLit "+" <**> pExpr4
  `pAlt`  FoundOp <$$> pLit "-" <**> pExpr5
  `pAlt`  pEmpty NoOp

pExpr5 :: Parser CoreExpr
pExpr5 = assembleOp <$$> pExpr6 <**> pExpr5c

pExpr5c :: Parser PartialExpr
pExpr5c = FoundOp <$$> pLit "*" <**> pExpr5
  `pAlt`  FoundOp <$$> pLit "/" <**> pExpr6
  `pAlt`  pEmpty NoOp

pExpr6 :: Parser CoreExpr
pExpr6 = foldl1 EAp <$$> pMunch1 pAexpr
