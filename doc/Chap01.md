---
marp: true
style: |
  section {
    font-family: 'Migu 1C', sans-serif;
  }
  p br {
    display: none;
  }
  code, pre {
    font-family: 'HackGenNerd Console';
  }
paginate: true
	
---
# 1. コア言語

1. コア言語の非形式的導入
2. コア言語の抽象構文
3. コア言語でのプログラムおよび式に対応するHaskellのデータ型 `CoreProgram`および`CoreExpr`．
4. コア言語の関数をまとめた**標準プレリュード**
5. プリティプリンタ
6. パーザ（構文解析器）

---
## 1.1 コア言語の概要

```
main = double 21 ;
double x = x + x
```

- コアプログラムは，スーパーコンビネータ定義の集まり
- コアプログラムの実行は，`main` を評価する
- 関数はスーパーコンビネータで定義する
- スーパーコンビネータにはCAFも含まれる

---
### 1.1.1 局所定義

- `let`（非再帰）または `letrec`（再帰）を使う
- `let` と `letrec` を区別するのは，`let` は `letrec` より実装が単純にできる
- バインディングの左辺は単一の変数

---
### 1.1.2 λ抽象

コンパイルの前処理としてλ持ち上げをおこなって、トップレベルでスーパーコンビネータとして定義する

---
### 1.1.3 構造をもつデータ型

代数データ型：

Haskell:
```
data Colour = Red | Green | Blue
data Complex = Rect Double Double | Polar Double Double
data Tree a = Leaf a | Branch (Tree a) (Tree a)
```

アプローチ方法:
- データコンストラクタを単純かつ統一的に表現
- パターン照合を単純な `case` 式に変換する

---
### 1.1.4 コンストラクタの表現

$\mathit{tag}$ をコンストラクタ識別子（整数）、$\mathit{arity}$ をそのコンストラクタのアリティとして、
$\texttt{Pack\{}tag,arity\texttt{\}}$ のように表現する

```
Red    = Pack{1,0}
Green  = Pack{2,0}
Blue   = Pack{3,0}

Rect   = Pack{4,2}
Polar  = Pack{5,2}

Leaf   = Pack{6,1}
Branch = Pack{7,2}
```

---
データ型を、実行時に区別する必要はないので、

```
Red    = Pack{1,0}
Green  = Pack{2,0}
Blue   = Pack{3,0}

Rect   = Pack{1,2}
Polar  = Pack{2,2}

Leaf   = Pack{1,1}
Branch = Pack{2,2}
```

でよい

---
### `case` 式

```
isRed c = case c of
              <1> -> True  ;
              <2> -> False ;
              <3> -> False
```
とか、
```
depth t = case t of
              <1> n     -> 0
              <2> t1 t2 -> 1 + max (depth t1) (depth t2)
```
入れ子のパターンはサポートしない

---
## 1.2 コア言語の構文


| 優先順位 | 結合方向 | 演算子 |
|:------:|:------:|:-----|
| 6      | 左     | 適用 |
| 5      | 右     | *    |
|        | 無     | /    |
| 4      | 右     | +    |
|        | 無     | -    |
| 3      | 無     | == ~= > >= < <= |
| 2      | 右     | & |
| 1      | 右     | \| |    

---
## 1.3 コア言語のデータ型
抽象構文木
```haskell
module Language where
import Utils

data Expr a
  = EVar Name
  | ENum Int
  | EConstr Int Int
  | EAp (Expr a) (Expr a)
  | ELet
      IsRec
      [(a, Expr a)]
      (Expr a)
  | ECase
      (Expr a)
      [Alter a]
  | ELam [a] (Expr a)
  deriving (Show)
```

---
抽象構文木

- `Expr` はバインダー（変数出現を束縛時の名前）でパラメータ化する
- 束縛位置では`Name`を使う
- 通常使う型名は、以下のようにする

```hasklell
type CoreExpr = Expr Name
type Name = String
```

---
抽象構文木
- 変数は、名前で区別する（`EVar Name`）
- 数は、整数のみ（`ENum Int`）
- データコンストラクタ（データ構成子）は §1.1.4 での議論どおり、構成子IDとアリティで表現（`EConstr Int Int`）
- 関数適用では関数と引数を子として並べて表現する。
- 中置演算子適用も抽象構文木では関数適用と同じ
    - `x + y` は
    `EAp (EAp (EVar "+") (EVar "x")) (EVar "y")`
    と表現する

---
`let` 式
```haskell
type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [ name | (name, rhs) <-defns ]

rhssOf    :: [(a, b)] -> [b]
rhssOf defns = [ rhs | (name, rhs) <- defns ]
```

---
`case` 式

```
ECase
  (Expr a)  -- 分析対象式
  [Alter a] -- 選択肢リスト  
```

選択肢は、タグ、束縛変数リスト、矢印右辺の（選択される）式の三つ組

```haskell
type Alter a = (Int, [a], Expr a)
type CoreAlter = Alter Name
```

---
アトミックな式かを判定する述語
```haskell
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False
```

---
プログラムはスーパーコンビネータ定義の集まり
```haskell
type Program a = [ScDefn a]
type CoreProgram = Program Name
```
スーパーコンビネータは、名前と仮パラメータ名と本体の三つ組
```haskell
type ScDefn a = (Name, [a], Expr a )
type CoreScDefn = ScDefn Name
```

---
```
main = double 21 ;
double x = x + x
```
というコアプログラムは、Haskellの以下のようなデータで表現される
```
[ ("main", [], EAp (EVar "double") (ENum 21))
, ("double", ["x"], EAp (EAp (EVar "+") (EVar "x")) (EVar "x"))
]
```

---
具象構文
$$
\begin{array}{lrcll}
  \text{プログラム} & \mathit{program} & \rightarrow & \mathit{sc}_1\texttt{;}\dots\texttt{;}\mathit{sc}_n & n\ge 1\\ \\
  \text{スーパーコンビネータ} & \mathit{sc} & \rightarrow & \mathit{var} \; \mathit{var}_1\;\dots\;\mathit{var}_n \;\texttt{=} \; \mathit{expr} & n \ge 0 \\ \\
  \text{式} & \mathit{expr} & \rightarrow & \mathit{expr}\; \mathit{aexpr} & 適用 \\
  & & \mid & \mathit{expr}_1 \; \mathit{binop} \; \mathit{expr}_2 & 中置二項演算子適用 \\
  & & \mid & \texttt{let} \; \mathit{defns} \; \texttt{in} \; \mathit{expr} & 局所定義\\
  & & \mid & \texttt{letrec} \; \mathit{defns} \; \texttt{in} \; \mathit{expr} & 局所再帰定義\\
  & & \mid & \texttt{case} \; \mathit{expr} \; \texttt{of} \; \mathit{alt} & case 式 \\
  & & \mid & \verb|\| \; \mathit{var}_1\;\dots\;\mathit{var}_n \; \verb|.| \; \mathit{expr} & \lambda 抽象\; n \ge 1\\
  & & \mid & \mathit{aexpr} & アトミックな式 \\\\
  & \mathit{aexpr} & \rightarrow & \mathit{var} & 変数 \\
  & & \mid & \mathit{num} & 数 \\
  & & \mid & \verb|Pack{|\mathit{num, num}\verb|}| & コンストラクタ \\
  & & \mid & \verb|(| \; \mathit{expr} \; \verb|)| & 括弧で囲まれた式 
\end{array}
$$
---
$$
\begin{array}{lrcll}
定義 & \mathit{defns} & \rightarrow & \mathit{defn}_1\texttt{;}\dots\texttt{;}\mathit{defn}_n & n\ge 1\\
& \mathit{defn} & \rightarrow & \mathit{var} \; \texttt{=} \; \mathit{expr} & \\ \\
選択肢 & \mathit{alts} & \rightarrow & \mathit{alt}_1\texttt{;}\dots\texttt{;}\mathit{alt}_n & n\ge 1\\
& \mathit{alt} & \rightarrow & \verb|<|\mathit{num}\verb|>| \;\mathit{var}_1\;\dots\;\mathit{var}_n \;\verb|->| \; \mathit{expr} & n \ge 0\\ \\
二項演算子 & \mathit{binop} & \rightarrow & \mathit{arithop} \; \mid \; \mathit{relop} \; \mid \mathit{boolop} & \\
& \mathit{arithop}  & \rightarrow & \verb|+| \; \mid \; \verb|-| \; \mid \; \verb|*| \; \mid \; \verb|/| & 算術 \\
& \mathit{relop} & \rightarrow & \verb|<| \; \mid \; \verb|<>=| \; \mid  \; \verb|==| \; \mid \; \verb|~=|  \; \mid \; \verb|>=| \; \mid \; \verb|>=| & 比較 \\
& \mathit{boolop} & \rightarrow & \verb|&| \; \mid \; \texttt{|} & 論理
\end{array}
$$

---
$$
\begin{array}{lrcll}
変数 & \mathit{var} & \rightarrow & \mathit{alpha} \; \mathit{varch}_1\;\dots\;\mathit{varch}_n & n \ge 0 \\
& \mathit{alpha} & \rightarrow & アルファベット文字 \\
& \mathit{varch} & \rightarrow & \mathit{alpha} \; \mid \; \mathit{digit} \; \mid \; \verb|_| \\\\
数 & \mathit{num} & \rightarrow & \mathit{digit}_1\dots\mathit{digit}_n & n\ge 1
\end{array}
$$

---
## 1.4 小さな標準プレリュード

```
I x = x ;
K x y = x ;
K1 x y = y ;
S f g x = f x (g x) ;
compose f g x = f (g x) ;
twice f = compose f f
```

```haskell
preludeDefs :: CoreProgram
preludeDefs
  = [ ("I", ["x"], EVar "x")
    , ("K", ["x","y"], EVar "x")
    , ("K1",["x","y"], EVar "y")
    , ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
                               (EAp (EVar "g") (EVar "x")))
    , ("compose", ["f","g","x"], EAp (EVar "f")
                                     (EAp (EVar "g") (EVar "x")))
    , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
    ]
```

---
## 1.5 コア言語プリティプリンタ

コア言語を`Show`クラスのインスタンスとしている（自動導出）ので、表示は可能だが、もうすこしなんとかしたい。
そこでプリティプリンタですよ。

```haskell
pprint :: CoreProgram -> String
```

---
### 1.5.1 文字列を用いたプリティプリンタ

```haskell
pprExpr :: CoreExpr -> String
pprExpr (ENum n)    = show n
pprExpr (EVar v)    = v
pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprAExpr e2
```
`pprAExpr` は引数の式がアトミックではないときに括弧で囲う
```haskell
pprAExpr :: CoreExpr -> String
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherewise     = "(" ++ pprExpr e ++ ")"
```

---
`++` をがっつり使っているので、パフォーマンスがすぐだめになる。

`pprExpr` は最悪、式のサイズ $n$ に対して $\Theta(n^2)$ の計算量になる。

抽象構文木で、左側が深い木を印字することを考えれば、理解できる。

```haskell
{- | 左側の深い構文木の生成 -}
mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n (repeat e2))
```

---
#### 練習問題 1.1

さまざまな、$n$ について、以下の式を評価するのに必要なステップ数を計測せよ

```haskell
pprExpr (mkMulti n (EVar "f") (EVar "x"))
```

---
### プリティプリント用抽象データ型

プリティプリントの問題を以下の2つに分解する
- 必要な操作は何か
- それらの操作の効率のよい実行方法は何か

抽象データ型（実装の詳細を抽象したデータ型）を考えることで ↑ を実現する
ここでは抽象データ型 `Iseq` を考える。

---
操作

```haskell
iNil :: Iseq                      -- ^ 空の Iseq
iStr :: String -> Iseq            -- ^ 文字列から Iseq への変換
iAppend :: Iseq -> Iseq -> Iseq   -- ^ 2つの Iseq の連結
iNewline :: Iseq                  -- ^ 改行
iIndent  :: Iseq -> Iseq          -- ^ Iseq の字下げ
iDisplay :: Iseq -> String        -- ^ Iseq から文字列への変換
```

---
`pprExpr`
```haskell
pprExpr :: CoreExpr -> Iseq
pprExpr (EVar v)    = iStr v
pprExpr (EAp e1 e2) = pprExpr e1 `iAppend` iStr " " `iAppend` pprAExpr e2

pprExpr (ELet isrec defns expr)
  = iConcat [ iStr keyword, iNewline
            , iStr "  ", iIndent (pprDefns defns), iNewline
            , iStr "in ", pprExpr expr
            ]
    where
      keyword | not isrec = iStr "let"
              | isrec     = iStr "letrec"

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) 
  = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]
```
---
```haskell
infixr 5 `iAppend`

iConcat     :: [Iseq] -> Iseq
iInterleave :: Iseq -> [Iseq] -> Iseq
```

---
#### 練習問題 1.2

`iConcat` および `iInterleave` を `iAppend` と `iNil` を使って定義せよ

---
ほとんどのプリティプリンティング関数は、`Iseq`型の値を返し、`iDisplay` は最後にトップレベルで適用してプログラムを表示する。


```haskell
pprint prog = iDisplay (pprProgram prog)
```

---
#### 練習問題 1.3

- `pprExpr` が `case`式とλ-抽象式を扱えるようにせよ
- `pprAExpr` および `pprProgram` を同様のスタイルで定義せよ

---
### 1.5.3 `Iseq` の実装

```haskell
data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
```
データ構造を用いて操作の表現をするのは、最後に `iDisplay` が呼ばれるまで、仕事を先延ばしにするという意図がある。

```haskell
iNil              = INil
iStr str          = IStr str
iAppend seq1 seq2 = IApend seq1 seq2
```

---
とりあえずインデントは無視する（次節で改良予定）ことにすると、`iIndent`と`iNewline`の定義は直截的に、

```
iIndent seq = seq
iNewline    = IStr "\n"
```

---
これで、`iDisplay` をどうするかがすべてということになる。
`Iseq`をサイズに線形な計算量で文字列にできるようにするのが目標。
`iDisplay` は `flatten` というより一般的な関数を使って定義する

```haskell
flatten :: [Iseq] -> String

iDisplay seq = flatten [seq]
```

---
```haskell
flatten []              = ""
flatten (INil : seqs)   = flatten seqs
flatten (Istr s : seqs) = s ++ flatten seqs
flatten (IAppend seq1 seq2 : seqs) = flatten (seq1 : seq2 : seqs)
```

---
#### 練習問題 1.4

`Iseq` による `flatten` の計算量は、`Iseq` のサイズに対してどうなっているか。
`pprExpr` を `Iseq` を返すようにしたうえで、練習問題 1.1 の実験を行って計測してみよ。
`pprExpr` の結果に `iDisplay` を適用することを忘れないように

---
#### 練習問題 1.5

抽象データ型を採用するもう１つの利点は、抽象データ型の実装がインターフェイスに影響しないということである。
`iAppend` を再定義して、一方の引数が`INil`であったときに結果が単純になるようにせよ。

---
### 1.5.4 配置と字下げ

`iIndent` の実装を自明なものから、まともなものにしよう。
`Iseq` に `IIndent` と `INewline` を追加する。

```
data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline

iIndent :: Iseq -> Iseq
iIndent seq = IIndent seq

iNewline :: Iseq
iNewline = INewline
```

---
`flatten` をより強力なものにする。

- 現在のカラム位置を保持する
- `Iseq` とそのインデントレベルとの対のリストをワーキングリストとする

```haskell
flatten :: Int                -- ^ 現在のカラム； 0 は最初のカラム
        -> [(Iseq, Int)]      -- ^ ワークリスト
        -> String             -- ^ 結果
```
あわせて、`iDisplay` も変更
```haskell
iDisplay :: Iseq -> String
iDisplay seq = flatten 0 [(seq, 0)]
```

---
```haskell
flatten col ((INewline, indent) : seqs)
  = '\n' : space indent ++ flatten indent seqs
```