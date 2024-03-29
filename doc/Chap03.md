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
    font-family: 'HackGen Console NFJ', monospace;
  }
paginate: true

---
# G-machine

この教科書での、コンパイラベースの最初の実装は G-machine による

---
## 3.1 G-machine の紹介

雛形具体化機械の基本的な操作 `instantiate`
- スーパーコンビネータの本体のインスタンスを構成する
- 具体化のたびに `instantiate` が本体の式を再帰的に走査

G-machine のアイデア
- スーパーコンビネータの本体を命令列に変換する（コンパイル時）
- 命令列を実行し、スーパーコンビネータの本体が具体化（実行時）

---
### 3.1.1 例

ソースコード
```
f g x = K (g x)
```
G-code
```
Push 1
Push 1
Mkap
Pushglobal K
Mkap
Slide 3
Unwind
```

---
(a) 初期状態
```
--+------+--
  |   ---|-----> @
  +------+      / \
  |   ---|---> @   x
  +------+    / \
  |   ---|-> f   g
  +------+                  
```

---
(b) Push 1 ： スタックトップから1飛した先のアプリケーションノードの引数側のポインタを Push
```
--+------+--
  |   ---|-----> @
  +------+      / \
  |   ---|---> @   x
  +------+    / \  ↑
  |   ---|-> f   g |
  +------+         |  
  |   ---|---------+
  +------+
```

---
(c) Push 1 ： スタックトップから1飛した先のアプリケーションノードの引数側のポインタを Push
```
--+------+--
  |   ---|-----> @
  +------+      / \
  |   ---|---> @   x
  +------+    / \  ↑
  |   ---|-> f   g |
  +------+       ↑ |  
  |   ---|-------|-+
  +------+       |
  |   ---|-------+
  +------+
```

---
(d) Mkap ： スタックトップ 2 つのポインタをポップして、関数適用ノードをアロケートし、それへのポインタを Push
```
--+------+--
  |   ---|-----> @
  +------+      / \
  |   ---|---> @   x
  +------+    / \  ↑
  |   ---|-> f   g |
  +------+       ↑ |  
  |   ---|-> @   | |
  +------+  / \  | |
           |   --|-+
           +-----+
```

---
(e) Pushglobal K ： スーパーコンビネータ K をスタックにプッシュ
```
--+------+--
  |   ---|---------> @
  +------+          / \
  |   ---|-------> @   x
  +------+        / \  ↑
  |   ---|-----> f   g |
  +------+           ↑ |  
  |   ---|-----> @   | |
  +------+      / \  | |
  |   ---|-> K |   --|-+
  +------+     +-----+
```

---
(f) Mkap
```
--+------+--
  |   ---|-------> @
  +------+        / \
  |   ---|-----> @   x
  +------+      / \  ↑
  |   ---|---> f   g |
  +------+         ↑ |  
  |   ---|-> @     | |
  +------+  / \    | |
           K   @   | |
              / \  | |
             |   --|-+
             +-----+
```

---
(g) Slide 3 ： スタックトップより先に積んだポインタ 3 つを破棄
```
--+------+--
  |   ---|-> @     
  +------+  / \    
           K   @   
              / \  
             g   x
```

---
### 3.1.2 その先の最適化

---

## 3.2 雛形構成のためのコード列

- `instantiate` は再帰的に定義された式の木構造にそって、式を再帰的に走査して具体化する
- 線形の命令列をコンパイルして式の具体化を行いたい

---
### 3.2.1 算術式の後置評価

```haskell
data AExpr = Num Int
           | Plus AExpr AExpr
           | Mult AExpr AExpr
```
意味は `aInterpret :: AExpr -> Int` で与えられる
```haskell
aInterpret :: AExpr -> Int
aInterpret (Num n)    = n
aInterpret (Plus e1 e2) = aInterpret e1 + aInterpret e2
aInterpret (Mult e1 e2) = aInterpret e1 * aInterpret e2
```

---
この算術式を後置算術演算子列にコンパイルし、スタックをつかって評価

たとえば $2 + 3 \times 4$ は `[INum 2, INum 3, INum 4, IMult, IPlus]` にコンパイルする

```haskell
data AInstruction = INum Int
                  | IPlus
                  | IMult
```

---
(3.1)
$$
\fbox{$
\begin{array}{rrr}
  & [] & [n] \\
  \Longrightarrow & & n
\end{array} $}
$$

(3.2)
$$
\fbox{$
\begin{array}{rrr}
  &\texttt{INum }n : i & \mathit{ns} \\
  \Longrightarrow & i & n : \mathit{ns}
\end{array} $}
$$

---
(3.3)
$$
\fbox{$
\begin{array}{rrr}
  &\texttt{IPlus} : i & n_0 : n_1 :\mathit{ns} \\
  \Longrightarrow & i & (n_0 + n_1) : \mathit{ns}
\end{array} $}
$$

(3.4)
$$
\fbox{$
\begin{array}{rrr}
  &\texttt{IMult} : i & n_0 : n_1 :\mathit{ns} \\
  \Longrightarrow & i & (n_0 \times n_1) : \mathit{ns}
\end{array} $}
$$

---
```haskell
aEval :: ([AInstruction], [Int]) -> Int
aEval ([],      [n]) = n
aEval (INum n:is, s) = aEval (is, n : s)
aEval (IPlus :is, n0:n1:s) = aEval (is, (n0 + n1) : s)
aEval (IMult :is, n0:n1:s) = aEval (is, (n0 * n1) : s)
```

---
```haskell
aCompile :: AExpr -> [AInstruction]
aCompile (Num n) = [INum n]
aCompile (Plus e1 e2) = aCompile e1 ++ aCompile e2 ++ [IPlus]
aCompile (Mult e1 e2) = aCompile e1 ++ aCompile e2 ++ [IMult]
```

---
#### 練習問題 3.1

任意の e :: AExpr について以下が成立つことを構造帰納法により証明せよ

```
aInterprete e = aEval (aCompile e, [])
```

---
#### 練習問題 3.2

`let`式が扱えるように`aInterpret`、`aCompile`、`aEval`を拡張せよ。このとき
```
aInterpret e = aEval (aCompile e, [])
```
であることを証明せよ

言語をさらに複雑な式、たとえば`letrec`式を扱えるように拡張できるかできるか？拡張できるなら、実装が正しいことを証明できるか。

---
### 3.2.2 後置コードを使ってグラフを構成する

後置評価の技法によりスーパーコンビネータの本体を構成できる。

- スタックに積むのは具体化された式の各部へのポインタ
- 雛形を構成する命令にはヒープ上にノードをアロケートするという副作用がある

スタックに関しては、雛形具体化機械で用意したものでよいが、

> スタック操作のたびに、変数名に対応するアドレスのスタック上での位置が変化することに注意が必要
> 式をコンパイルする際その変化を追跡する必要がある

ここではスタック上の項にアクセスするのにスタックトップからのオフセットを使う。したがって、push、pop のたびに各項のオフセットが1増減する。

---
### 3.2.3 具体化のあとに起こること

- スタックトップには、新しくインスタンス化された本体のヒープ内のアドレス
- つづいて、(n+1)個のポインタ（スーパーコンビネータとn個の引数への適用）
- n+1 個目ポインタは、ルートノードを指す

図 3.2 参照

- `Slide`命令で、redex を新しくインスタンス化された本体に置き換え、スタックから n 個の項目をポップする
- 次のスーパーコンビネーターを見つけるには、`Unwind` 命令で再び巻き戻しを開始する

---
## 3.3 Mark 1: ミニマル G-machine

- 更新なし
- 算術演算なし

---
### 3.3.1 全体構造

`Gmachine.Mark1.Machine` モジュール

---
### 3.3.2 データ型定義

- `Gmachine.Mark1.State` モジュール
- `Gmachine.Mark1.Code` モジュール
- `Gmachine.Mark1.Node` モジュール

---
### 3.3.3 評価器

`Gmachine.Mark1.Machine` モジュール

---
### 3.3.4 プログラムのコンパイル

`Gmachine.Mark1.Machine` モジュール

---
#### 練習問題 3.3
プレリュードにある S コンビネータに対応する変換列を書け。コンパイラとGmachineを走らせて、付録Bの単純なプログラムを実行して最終結果を確認せよ。

---
### 結果の表示
`Gmachine.Mark1.PPrint` モジュール

---
### 3.3.6 Mark1 Gmachine の改良

---
#### 練習問題 3.4 
`main = S K K 3` というプログラムを走らせたとき、ステップはどうなるか。雛形マシンと、どのくらい違うか。Gマシンと雛形マシンとをステップ数で比較するのは公平だと思うか。

---
#### 練習問題 3.5
付録B にある他のプログラムも試してみよ。まだ、算術演算には対応していないことに注意せよ。

---
#### 練習問題 3.6
`pushint` を実装するとき、`pushglobal` で使った方法がそのまま使える。つまり、`Pushint 2`を最初に実行したときに、`"2"`をヒープ内ノード `NNum 2` のアドレスに結びつけるのである。

$n$ というグローバル変数があれば、そのノードを再利用する
$$
\begin{array}{rrrll}
& \texttt{Pushint}\;n : i & s & h & m[n : a]\\
\Longrightarrow & i & a : s & h & m
\end{array}
$$
なければ、新しいノードを生成して登録する
$$
\begin{array}{rrrll}
& \texttt{Pushint}\;n : i & s & h & m\\
\Longrightarrow & i & a : s & h[a : \texttt{NNum}\;n] & m[n : a]
\end{array}
$$
この変更を反映した `pushint` を実装せよ。

---
## 3.4 Mark 2：Lazyにする

Mark 1 は、巻き戻しに先立って、元の式のルートノードを上書きしないので、Lazy にはなっていない。
Mark 2 では、元の式のルートノードを具体化したスーパーコンビネータの本体を指す間接参照ノードで上書きする。
こうすることで、前回、簡約可能項を簡約して得た値を記憶し、再計算を排除できる。

---
### 3.4.1 データ構造

#### `Instruction`
- `Update Int` と `Pop Int` を追加
- `Slide Int` を削除

#### 練習問題 3.7 
`showInstruction` を対応させよ

---
#### `Node`

- `NInd Addr` を追加

#### 練習問題 3.8
`showNode` を対応させよ

---
### 3.4.2 評価器

`Update` $n$ はスタック上の n+1 番目の項目をスタックトップの項目への間接参照で上書きする

(3.15)
$$
\begin{array}{rrrll}
& \texttt{Update}\;n : i & a:a_0:...:a_n:s & h & m\\
\Longrightarrow & i & a_0:...:a_n : s & h[a_n:\texttt{NInd}\;a] & m
\end{array}
$$

---
`Pop` $n$ は単純に$n$個の項目をスタックから取り除く

(3.16)
$$
\begin{array}{rrrll}
& \texttt{Pop}\;n : i & a_0:...:a_n:s & h & m\\
\Longrightarrow & i & s & h & m
\end{array}
$$

---
スタックトップの項目が間接参照ノードであった場合は、`Unwind` は、間接参照ノードが指す先に置き換える

(3.17)
$$
\begin{array}{rrrll}
& [\texttt{Unwind}] & a_0:s & h[a_0:\texttt{NInd}\;a] & m\\
\Longrightarrow & [\texttt{Unwind}] & a : s & h & m
\end{array}
$$

#### 練習問題 3.9
Mark 1 の `dispatch`関数を新しく導入したインストラクションと遷移規則に対応できるよう変更せよ。

---
### 3.4.3 コンパイラ
$\mathcal{R}$-図式（図3.6）の部分が変更になる

#### 練習問題 3.10
新しい$\mathcal{R}$-図式に対応するよう`compileR`を変更せよ。
#### 練習問題 3.11
以下のプログラムをlazy評価器で実行せよ。
Mark 1との違いはどこからくるか。ステップ数は大きく違っているか

---
## 3.5 Mark 3

本体部に `let(rec)` 変数を含むスーパーコンビネータを扱えるよう、言語を拡張する。

先に変数へのアクセス方法を見直す

---
#### スタックからの引数へのアクセス

アリティが n のスーパーコンビネータ適用のときのスタックレイアウトは Mark 1 および Mark 2 では、図 3.7 の左の図式になっていた。これを、右の図式のように、スタック上位 n 項目が直接、引数の式を指すようにする。

スーパーコンビネータの仮引数を置き換える式へのアクセスが速くなる
- `Push`命令実行時に`getArg`が必要なくなる
- 引数が満たされたスーパーコンビネータを`Unwind`するときにスタックの再配置する
- ルートノードへのポインタは`Update`ができるように維持する

---
#### 命令への影響

##### `Push` の変更

$$
\begin{array}{rrrll}
& \texttt{Push}\;n : i& a_0: \dots :a_n:s & h & m\\
\Longrightarrow & i & a_n : a_0 : \dots :a_n : s & h & m
\end{array}
$$

---
##### `Unwind` の変更

(3.19)
$$
\begin{array}{rrrll}
& [\texttt{Unwind}] & a_0: \dots :a_n:s & 
  h \left[ \begin{array}{lcl}
            a_0 &:& \texttt{NGlobal}\;n\;c\\
            a_1 &:& \texttt{NAp}\; a_0\;a^{\prime}_1\\
            &&\cdots\\
            a_n &:& \texttt{NAp}\; a_{n-1}\;a^{\prime}_n
          \end{array}
   \right]& m\\
\Longrightarrow & c & a_n : a_0 : \dots :a_n : s & h & m
\end{array}
$$

---
#### 練習問題 3.12

`dispatch` と遷移ルールを書き直せ。

---
#### 練習問題 3.13
コンパイラと新しいマシンを付録Bのプログラムで試して、壊れていないか確かめよ。

---
### 3.5.1 局所束縛変数

##### `let`

式 $\texttt{let}\;x_1\texttt{=}e_1\texttt{;}\dots\texttt{;}x_n\texttt{=}e_n\;\texttt{in}\;e$ 中の変数 $x_1\dots x_n$ は式 $e_1\cdot e_n$を構成してしまえば、スーパーコンビネータの引数と同にあつかえる。

図 3.8

---

