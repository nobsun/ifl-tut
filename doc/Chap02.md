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
    font-family: 'HackGenNerd Console', monospace;
  }
paginate: true
	
---
# 2. 雛形具体化

**雛形具体化**を用いるグラフ簡約器

---
## 2.1 雛形具体化

[Implementation of Functional Programming Language]()の11章、12章

- 関数プログラムは**式を評価**によって実行される
- 式は**グラフ**で表現される
- 評価は一連の簡約を行うことで実施される
- 簡約はグラフ中の簡約可能式(redex)を簡約しその結果で置き換える
- 評価は対象とする式が**正規形**(normal form)になれば終了
- 簡約系列な複数ありうるが停止したときには同じ正規形になる
- 正規形に到達する簡約系列があれば最外簡約戦略で必ず停止する

---
#### 簡約例
```
main = square (square 3)
square x = x * x
```
スーパーコンビネータ `main` には引数はなく、それ自身redexなので、ボディ部と置き換える。
```
main          ===>      @
              簡約     / \
                 square   @
                         / \
                   square   3
```

----
最外のredexは`square`の適用式。関数本体を具体化したものでredexを置き換える。仮引数の各出現を引数へのポインタで置き換える。
```
       @!     ===>      @! 
      / \     簡約     / \
square   @            @   \
        / \          / \___@
  square   3        *     / \
                    square   3
```

---
ここでredexは内側の`square`の適用式だけなので、これを簡約する。

```
       @      ===>      @
      / \     簡約     / \
     @   \            @   \
    / \___@!         / \___@!
   *     / \        *     / \
   square   3            @   \
                        / \___3
                       *
```

---
ここで内側のかけ算が、唯一のredexとなるのでこれを簡約する。
```
       @      ===>      @
      / \     簡約     / \
     @   \            @   \
    / \___@!         / \___9
   *     / \        *     
        @   \
       / \___3
      *
```

---
最後の簡約は簡単。
```
       @      ===>     81
      / \     簡約
     @   \
    / \___9
   *
```

---
### 2.1.2 簡約の3ステップ

以下を正規形が得られるまで繰り返す
1. 次に簡約するredexを見つける
2. そのredexを簡約する
3. redexのルートを結果で更新する

---
最外の関数適用が

- スーパーコンビネータ適用の場合
    - この適用は必ずredexなので簡約（β簡約）
- 組込みのプリミティブ適用の場合
    - 2つの引数が共に正規形ならこの適用はredexなので簡約（δ簡約）
    - そうでないなら、引数を正規形にして（この適用がredexになって）から簡約（δ簡約）

---
### 2.1.3 背骨を巻き戻して次のredexをみつける
ルートから適用ノードの左側を辿る
```
   Stack
--+------+--
                   @*
                  / \
                 @   E3
                / \
               @   E2
              / \
             f   E1

```
---
ルートの適用ノードをスタックに積んで、左へ降りる
```
   Stack
--+------+--
  |   ---|-------> @ 
  +------+        / \
                 @*  E3
                / \
               @   E2
              / \
             f   E1

```
---
適用ノードをスタックに積んで、左に降りる
```
   Stack
--+------+--
  |   ---|-------> @ 
  +------+        / \
  |   ---|-----> @   E3
  +------+      / \
               @*  E2
              / \
             f   E1
  
```
---
適用ノードをスタックに積んで、左に降りる
```
   Stack
--+------+--
  |   ---|-------> @ 
  +------+        / \
  |   ---|-----> @   E3
  +------+      / \
  |   ---|---> @   E2
  +------+    / \
             f*  E1

```
---
`f`がスーパーコンビネータの場合： `f` をスタックに積んで、`f` のアリティ（ここでは2とする）を確認
```
   Stack
--+------+--
  |   ---|-------> @ 
  +------+        / \
  |   ---|-----> @   E3
  +------+      / \
  |   ---|---> @   E2
  +------+    / \
  |   ---|-> f  E1
  +------+                  
```
---
`f`がスーパーコンビネータの場合： アリティ（ここでは2とする）の分だけノードを上へもどったところが、最外の簡約可能項のルートノード
```
   Stack
--+------+--
  |   ---|-------> @ 
  +------+        / \
  |   ---|-----> @!  E3
  +------+      / \
  |   ---|---> @   E2
  +------+    / \
  |   ---|-> f  E1
  +------+
```
---
`f` がプリミティブ（算術2項演算子）の場合： 被演算子 `E1`および`E2`が共に正規形なら`!`のついたノードが最外の簡約可能項のルートノード。さもなければ、先に被演算子項を正規形にまで簡約する。

--- 
### 2.1.4 スーパーコンビネータの簡約可能項

- スーパーコンビネータの簡約可能項は、仮パラメータの出現位置を対応する実引数項へのポインタに置き換えたスーパーコンビネータ本体で置き換える
- 実引数項をコピーせず、ポインタを使い、共有していることに注意

```
  @     ===>     @
 / \    簡約    / \
f   3          @   \
              / \___@y
             +     / \
                  @   \
                 / \___3
                *
```

---
### 2.1.5 更新

```
id x = x
f p = (id p) * p
main = f (sqrt 4)

f の簡約後

     @      ===>      @
    / \              / \
   @   \            @   \
  / \   \          / \   \
 *   @   \        *   #   \
    / \___@            \___@
  id     / \              / \
     sqrt   4         sqrt   4
```
`#` は間接参照ノード

---
### 2.1.6 CAF

```
fac20 = factorial 20
```
スーパーコンビネータ `fac20` は CAF でかつ redex のルートなので、`fac20` の簡約結果で上書きする

---
## 2.2 状態遷移システム

かけ算機械

```haskell
type MultState = (Int, Int, Int, Int) -- ^ (n, m, d, t)

evalMult :: MultState -> [MultState]
evalMult state = if multFinal state
  then [state]
  else state : evalMult (stepMult state)

stepMult (n, m, d, t)
  | d >  0 = (n,   m, d-1, t+1)  -- ^ 規則 1
  | d == 0 = (n, m-1,   n, t  )  -- ^ 規則 2
```

---
#### 練習問題 2.1
かけ算マシンを手で走らせよ。初期状態 (2,3,0,0) からはじめ、各ステップで発火する規則を特定し、最終状態が (2,0,0,6) であることを確かめよ。

---
#### 練習問題 2.2
状態列の不変条件とは、すべての状態で真となる述語である。
$n$ および $m$ の初期値 $N$ および $M$ と現在の $n$、$m$、$d$、$t$ の値のとの関係を見つけよ。これにより、このかけ算機械が、かけ算を実行するものであることを証明せよ。すなわち、以下を示せ。
1. 初期状態で不変条件が成り立つ
2. ある状態で、不変条件が成り立てば、次の状態でも不変状態が成り立つ
3. 不変条件と停止条件が成り立てば、$t = N \times M$ である
4. このかけ算機械は停止する

---
状態遷移システムは、以下の点で便利
- 低レベルの詳細にわずらわされない程度に抽象的
- 隠れた詳細に依存していないことが確認できる程度に具体的
- 状態遷移システムは直截に実行可能なHaskellのコードに変換できる

---
#### 練習問題 2.3
状態が最終状態であるかを判断する述語 `multFinal :: multState -> Bool` を定義し、初期状態 `(2,3,0,0)` からかけ算機械を走らせると、最終状態が `(2,0,0,6)` になることを示せ。

---
## 2.3 Mark 1: 最小雛形具体化グラフ簡約器

マシン状態： $(\mathit{stack}, \mathit{dump}, \mathit{heap}, \mathit{globals})$ の4つ組
- $\mathit{stack}$: ヒープ上のノードを特定するアドレスのスタック
    - $a_1 : s$ という記法は、$a_1$ がスタックトップ、$s$ がのこりのスタックであることを示す
- $\mathit{dump}$: 正格なプリミティブ演算の引数評価に先立ち、スパインのスタックを記録
- $\mathit{heap}$: タグ付きノードを集めたもの
    - $h[a : node]$ という記法は、ヒープ $h$ において、$a$ はノード$\mathit{node}$のアドレスであることを示す
- $\mathit{globals}$: スーパーコンビネータおよびプリミティブを表すノードへのアドレス

---
ノードの表現
```haskell
data Node
  = NAp Addr Addr
  | NSupercomb Name [Name] CoreExpr
  | NNum Int
```
- `NAp`$a_1$ $a_2$ はアドレス$a_1$にあるノードのアドレス$a_2$にあるノードへの適用を表す
- `NSupercomb` $\mathit{args}$ $\mathit{body}$ は引数$\mathit{args}$と本体$\mathit{body}$をもつスーパーコンビネータを表す
- `NNum` $n$ は整数$n$を表す

---
状態遷移規則

(2.1)
$$
\begin{array}{rrrcll}
& a:s & d & & h[a:\texttt{NAp}\;a_1\;a_2] & f\\
\Longrightarrow & a_1 : a : s & d && h & f \\
\end{array}
$$

---
状態遷移規則

(2.2)
$$
\begin{array}{rrrcll}
& a_0 : a_1 : \dots : a_n : s & d && h[a_0 : \texttt{NSupercomb}\;[x_1,\dots,x_n]\;\mathit{body}] & f \\
\Longrightarrow & a_r : s & d && h' & f \\
\end{array}
$$
ここで、$(h',a_r) = \mathit{instantiate}\;\mathit{body}\;h\;f[x_1 \mapsto a_1,\dots,x_n \mapsto a_n]$

---
関数 $\mathit{instatiate}$ の引数は、
1. 具体化する式
2. ヒープ
3. 名前からヒープ上のアドレスへのグローバルマッピング $f$ をスタックにある引数名からヒープアドレスへのマッピングで拡張したもの

返り値は、

- 新しいヒープと、新しく構成されたインスタンスの（ルートノードの）アドレス

---
### 2.3.2 実装の構造

```haskell
run :: String -> String
run = showResult . eval . compile . parse
```

---

1. `parse` はソースコード(`:: String`)を構文解析して`CoreProgram`を構成する関数（教科書とは意味を変更）
```haskell
parse :: String -> CoreProgram
```
2. `compile` は `CoreProgram` を雛形具体化機械の初期状態に変換
```haskell
compile :: CoreProgram -> TiState
```
3. `eval` はプログラム実行関数、初期状態から状態遷移を繰り返し、最終状態にまで遷移させ、結果は通過したすべての状態のリスト
```haskell
eval :: TiState -> [TiState]
```
4. `showResult` は最終結果を整形して表示する
```haskell
showResult :: [TiState] -> String
```

---
### 2.3.3 パーザ

`Language`モジュールをインポートする

---
### 2.3.4 コンパイラ

`TiState`
```haskell
type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
```

---
1. `TiStack` スパインスタック、ヒープアドレス`Addr`のスタック
```haskell
type TiStack = [Addr]
```

---
2. `TiDump` ダンプ、2.6節までは不要。ダミー
```haskell
data TiDump = DummyTiDump
initalTiDump :: TiDump
initalTiDump = DummyTiDump
```

---
3. `TiHeap` は `Node`を格納するヒープ
```haskell
type TiHeap = Heap Node
type Heap a = (Int, [Addr], [(Addr, a)])
```
`Heap` は使用アドレス数、未使用アドレス集合、アドレスと内容の2つ組のリスト、の3つを組にしたもの

---
4. `TiGlobal` はスーパーコンビネータ名とその定義が納められているヒープ上のアドレスの連想リスト
```haskell
type TiGlobals = Assoc Name Addr
type Assoc a b = [(a, b)]
```

---
5. `TiStats` 実行時性能統計のためのデータ収集用、ひとまずステップカウント
```haskell
type TiStats = Int

tiStatInitial :: TiStats
tiStatInitial = 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps s = s + 1
tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (stack, dump, heap, scDefs, stats)
  = (stack, dump, heap, scDefs, f stats)
```

---
`compile`
```haskell
compile program
  = (initialStack, initalTiDump, initialHeap, globals, tiStatInitial)
    where
      scDefs = program ++ preludeDefs ++ extraPreludeDefs
      (initialHeap, globals) = buildInitialHeap scDefs
      initialStack = [addressOfMain]
      addressOfMain = aLookup globals "main" (eerror "main is not defined")

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []
```

---
`buildInitialHeap` プログラムから`NSupercomb`ノードを含むヒープと、スーパーコンビネータ名とヒープ上のアドレスの対応を示す連想リストを構成する。

```haskell
buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = mapAccumL allocateSc hInitial scDefs

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap scDefn = case scDefn of
  (name, args, body) -> (heap', (name, addr))
    where
      (heap', addr) = hAlloc heap (NSupercomb name args body)
```

---
### 2.3.5 評価器

```haskell
eval state = state : restStates
  where
    restStates
      | tiFinal state = []
      | otherwise     = eval nextState
    nextState = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state
```

---
最終状態の判定
- 計算はスタックに単一の数またはデータオブジェクトが含まれる状態になったときにのみ停止する

```haskell
tiFinal :: TiState -> Bool
tiFinal state = case state of
  ([soleAddr], _, heap, _, _) -> isDataNode (hLookup heap soleAddr)
  ([], _, _, _, _)            -> error "Empty stack!"
  _                           -> False

isDataNode :: Node -> Bool
isDataNode node = case node of
  NNum _ -> True
  _      -> False
```

---
`step` ある状態から1つ次の状態への遷移
```haskell
step :: TiState -> TiState
step state = case state of
  (stack, dump, heap, globals, stats) -> dispatch (hLookup heap (head stack))
  where
    dispatch (NNum n)                  = numStep state n
    dispatch (NAp a1 a2)               = apStep  state a1 a2
    dispatch (NSupercomb sc args body) = scStep  state sc args body
```

---
`NNum`ノードと`NAp`ノードはやさしい

```haskell
numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as a function"

apStep :: TiState -> Addr -> Addr -> TiState
apStep state a1 a2 = case state of
  (stack, dump, heap, globals, stats) -> (a1:stack, dump, heap, globals, stats)
```

---
スーパーコンビネータの適用： 
1. 本体を具体化、引数名をスタックにあるアドレスと結びつける（規則2.2）
2. 簡約可能項のルートを含む引数をスタックから除去、簡約結果をスタックにプッシュ

（Mark 1 では更新は行わない）
```haskell
scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep state scName argNames body = case state of
  (stack, dump, heap, globals, stats)
    -> if length stack < length argNames + 1
       then error "Too few argments given"
       else (stack', dump, heap', globals, stats)
    where
      stack' = resultAddr : drop (length argNames + 1) stack
      (heap', resultAddr) = instantiate body heap env
      env = argBindings ++ globals
      argBindings = zip argNames (getargs heap stack)
```

---




















