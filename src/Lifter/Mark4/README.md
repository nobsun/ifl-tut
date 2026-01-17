## 6.6 Mark 4: A separate full laziness pass

We now turn our attention to an important property of functional programs called **full laziness**. Previous accounts of full laziness have invariably linked it to lambda lifting, by describing ‘fully lazy lambda lifting’, which turns out to be rather a complex process. [Hughes 1983] gives an algorithm, but it is extremely subtle and does not handle let(rec) expressions. On the other hand, [Peyton Jones 1987] does cover let(rec) expressions, but the description is only informal and no algorithm is given.

In this section we show how full laziness and lambda lifting can be cleanly separated. This is done by means of a transformation involving let expressions. Lest it be supposed that we have simplified things in one way only by complicating them in another, we also show that performing fully lazy lambda lifting without let(rec) expressions risks an unexpected loss of laziness. Furthermore, much more efficient code can be generated for let(rec) expressions in later phases of most compilers than for their equivalent lambda expressions.

## 6.6 マーク4: 完全怠惰性のための分離パス

ここでは関数型プログラムの重要な特性である**完全怠惰性**について考察する。従来の完全怠惰性に関する説明はいずれもラムダリフトングと密接に関連しており、「完全な遅延ラムダリフトング」として記述されてきたが、これは実際には非常に複雑な処理過程を伴うものである。[Hughes 1983]ではアルゴリズムが提示されているものの、その内容は高度に専門的で、let(rec)式を適切に扱えないという欠点がある。一方、[Peyton Jones 1987]ではlet(rec)式にも対応しているが、記述は形式的ではなく、具体的なアルゴリズムは示されていない。

本節では、完全怠惰性とラムダリフトングを明確に分離する方法を示す。これはlet式を用いた変換手法によって実現される。単に一方を簡素化する代わりに別の方法で複雑化しているという誤解を避けるため、let(rec)式を使用しない場合の完全な遅延ラムダリフトングには、予期せぬ形で遅延評価が失われる危険性があることを明らかにする。さらに、ほとんどのコンパイラにおいて、let(rec)式はその等価なラムダ式と比較して、後段の処理段階でより効率的なコードを生成できる点についても言及する。

### 6.6.1 A review of full laziness

We begin by briefly reviewing the concept of full laziness. Consider again the example given in Section 6.3.

```
f x = let g = \y. x*x + y in (g 3 + g 4);
main = f 6
```

The simple lambda lifter generates the program:

```
$g x y = x*x + y;
f x = let g = $g x in (g 3 + g 4);
main = f 6
```

In the body of `f` there are two calls to `g` and hence to `$g`. But `($g x)` is not a reducible expression, so `x*x` will be computed twice. But `x` is fixed in the body of `f`, so some work is being duplicated. It would be better to share the calculation of `x*x` between the two calls to `$g`. This can be achieved as follows: instead of making `x` a parameter to `$g`, we make `x*x` into a parameter, like this:

```
$g p y = p + y;
f x = let g = $g (x*x) in (g 3 + g 4);
```

(we omit the definition of `main` from now on, since it does not change). So a fully lazy lambda lifter will make each **maximal free sub-expresssion** (rather than each free variable) of a lambda abstraction into an argument of the corresponding supercombinator. A maximal free expression (or MFE) of a lambda abstraction is an expression which contains no occurrences of the variable bound by the abstraction, and is not a sub-expression of a larger expression with this property.

Full laziness corresponds precisely to moving a loop-invariant expression outside the loop, so that it is computed just once at the beginning rather than once for each loop iteration.

How important is full laziness for ‘real’ programs? No serious studies have yet been made of this question, though we plan to do so. However, recent work by Holst suggests that the importance of full laziness may be greater than might at first be supposed [Holst 1990]. He shows how to perform a transformation which automatically enhances the effect of full laziness, to the point where the optimisations obtained compare favourably with those gained by partial evaluation [Jones et al. 1989], though with much less effort.

### 6.6.1 完全怠惰性の再検討

まず、完全怠惰性の概念について簡潔に復習する。6.3節で示した以下の例を再び検討しよう。

```
f x = let g = \y. x*x + y in (g 3 + g 4);
main = f 6
```

単純なラムダ変換器によって生成されるプログラムは以下の通りである：

```
$g x y = x*x + y;
f x = let g = $g x in (g 3 + g 4);
main = f 6
```

`f`の本体内では`g`が2回呼び出され、その結果として`$g`も2回呼び出される。しかし`($g x)`は簡約可能な式ではないため、`x*x`の計算が2回行われることになる。ただし`x`は`f`の本体内で固定値であるため、計算の一部が重複している。理想的には、`$g`への2回の呼び出し間で`x*x`の計算結果を共有すべきである。これは以下のように実現可能である：`x`を`$g`のパラメータとするのではなく、`x*x`そのものをパラメータとして扱うようにする。具体的には以下のように記述する：
```
$g p y = p + y;
f x = let g = $g (x*x) in (g 3 + g 4);
```

（以降では`main`の定義は省略する。この部分は変更がないためである）。したがって、完全怠惰性を行うラムダ変換器は、ラムダ抽象の各**最大自由部分式**（各自由変数ではなく）を、対応するスーパーコンビネータの引数として扱うようになる。ラムダ抽象の最大自由式（Maximal Free Expression: MFE）とは、抽象によって束縛された変数の出現を含まず、かつこの性質を持つより大きな式の部分式でもない式のことである。
完全怠惰性は、ループ不変式をループの外側に移動させることに相当し、これにより式はループの各反復ごとに計算されるのではなく、最初に1度だけ計算されるようになる。

「実際のプログラム」において完全怠惰性はどれほど重要なのだろうか？この問題に関する本格的な研究はまだ行われていないが、我々は今後このテーマに取り組む予定である。ただし、最近のホルストによる研究によれば、完全怠惰性の重要性は当初考えられていたよりも大きい可能性がある［Holst 1990］。彼は、完全怠惰性の効果を自動的に向上させる変換手法を示しており、その結果得られる最適化は、部分評価によって得られる最適化結果と比較しても遜色ないレベルに達することを示している［Jones et al. 1989］。しかも、この手法ははるかに少ない労力で実現できるという。

### 6.6.2 Fully-lazy lambda lifting in the presence of let(rec)s

Writing a fully lazy lambda lifter, as outlined in the previous section, is surprisingly difficult. Our language, which includes `let(rec)` expressions, appears to make this worse by introducing a new language construct. For example, suppose the definition of `g` in our running example was slightly more complex, thus:

```
g = \y. let z = x*x
        in let p = z*z
           in p + y
```

Now, the sub-expression `x*x` is an MFE of the `\y`-abstraction, but sub-expression `z*z` is not since `z` is bound inside the `\y`-abstraction. Yet it is clear that `p` depends only on `x` (albeit indirectly), and so we should ensure that `z*z` is only computed once.

Does a fully lazy lambda lifter spot this if `let` expressions are coded as lambda applications? No, it does not. The definition of `g` would become

```
g = \y. (\z. (\p. p+y) (z*z)) (x*x)
```

Now, `x*x` is free as before, but `z*z` is not. In other words, **if the compiler does not treat `let(rec)` expressions specially, it may lose full laziness which the programmer might reasonably expect to be preserved.**

Fortunately, there is a straightforward way to handle `let(rec)` expressions — described in [Peyton Jones 1987, Chapter 15] — namely to ‘float’ each `let(rec)` definition outward until it is outside any lambda abstraction in which it is free. For example, all we need do is transform the definition of g to the following:

```
g = let z = x*x
    in let p = z*z
       in \y. p + y
```

Now `x*x` and `z*z` will each be computed only once. Notice that this property should hold **for any implementation of the language**, not merely for one based on lambda lifting and graph reduction. This is a clue that full laziness and lambda lifting are not as closely related as at first appears, a topic to which we will return in the next section.

Meanwhile, how can we decide how far out to float a definition? It is most easily done by using **lexical level numbers** (or **de Bruijn numbers**). There are three steps:

- First, assign to each lambda-bound variable a level number, which says how many lambdas enclose it. Thus in our example, `x` would be assigned level number $1$, and `y` level number $2$.
- Now, assign a level number to each `let(rec)`-bound variable (outermost first), which is the maximum of the level numbers of its free variables, or zero if there are none. In our example, both `p` and `z` would be assigned level number $1$. Some care needs to be taken to handle letrecs correctly.
- Finally, float each definition (whose binder has level $n$, say) outward, until it is outside the lambda abstraction whose binder has level $n + 1$, but still inside the level-$n$ abstraction. There is some freedom in this step about exactly where between the two the definition should be placed.

Each mutually recursive set of definitions defined in a `letrec` should be floated out together, because they depend on each other and must remain in a single `letrec`. If, in fact, the definitions are **not** mutually recursive despite appearing in the same `letrec`, this policy might lose laziness by retaining in an inner scope a definition which could otherwise be floated further outwards. The standard solution is to perform **dependency analysis** on the definitions in each `letrec` expression, to break each group of definitions into its minimal subgroups. We will look at this in Section 6.8.

Finally, a renaming pass should be carried out before the floating operation, so that there is no risk that the bindings will be altered by the movement of the `let(rec)` definitions. For example, the expression

```
\y. let y = x*x in y
```

is obviously not equivalent to

```
let y = x*x in \y->y
```

All that is required is to give every binder a unique name to eliminate the name clash.

### 6.6.2 let(rec) 式が存在する場合の完全怠惰性ラムダリフト

前節で概説した完全怠惰のラムダリフトを実装することは、意外にも非常に困難である。`let(rec)`式を含む我々の言語は、新たな言語構成要素を導入することでこの問題をさらに複雑にしているように見える。例えば、実行例における`g`の定義がわずかに複雑であった場合、以下のように記述される：

```
g = \y. let z = x*x
        in let p = z*z
           in p + y
```

この場合、部分式`x*x`は`\y`抽象の最小評価式（MFE）であるが、`z*z`はMFEではない。これは`z`が`\y`抽象内で束縛されているためである。しかしながら、`p`は間接的ではあるものの、あくまで`x`のみに依存していることは明らかである。したがって、`z*z`は一度だけ計算されるように保証する必要がある。

**`let`式をラムダ適用として実装した場合、完全怠惰のラムダリフトはこれを検出できるだろうか？答えは否である。`g`の定義は以下のようになってしまう：**

```
g = \y. (\z. (\p. p+y) (z*z)) (x*x)
```

この場合、`x*x`は以前と同様に自由変数であるが、`z*z`は自由変数ではない。つまり、**コンパイラが`let(rec)`式を特別な扱いをしない場合、プログラマーが当然期待する完全怠惰性が失われる可能性がある**のである。

幸いなことに、`let(rec)`式を適切に処理する簡潔な方法が存在する――[Peyton Jones 1987, Chapter 15]で説明されている方法である――それは、各`let(rec)`定義を、自由変数として含まれるラムダ抽象の外側に移動させる「フローティング」処理を行うことである。具体的には、`g`の定義を以下のように変換するだけでよい：

```
g = let z = x*x
    in let p = z*z
       in \y. p + y
```

この変換により、`x*x`と`z*z`はそれぞれ一度だけ計算されるようになる。この性質は、**ラムダリフトとグラフリダクションに基づく実装に限らず、言語のいかなる実装においても保持されるべきものである**という点に注目してほしい。これは、完全怠惰性とラムダリフトが当初考えられていたほど密接に関連しているわけではないことを示す重要な手がかりであり、このテーマについては次節で改めて考察する。

では、定義をどの程度外側に移動させればよいかをどのように決定すればよいだろうか？最も簡単な方法は、**語彙レベル番号**（または**デ・ブライユン番号**）を使用することである。その手順は以下の通りである：

- まず、各ラムダ束縛変数にレベル番号を割り当てる。この番号は、その変数を囲むラムダの数を示す。したがって、この例では`x`にはレベル番号$1$が、`y`にはレベル番号$2$が割り当てられる。
- 次に、各`let(rec)`束縛変数にレベル番号を割り当てる（外側から順に）。割り当て値は、その変数の自由変数のレベル番号の最大値か、あるいは自由変数が存在しない場合は0とする。この例では、`p`と`z`の両方にレベル番号$1$が割り当てられる。`letrec`を正しく処理するためには、注意が必要である。
- 最後に、各定義（その束縛子がレベル$n$であるもの）を、レベル$n + 1$のラムダ抽象の外側まで、かつレベル$n$の抽象の内側まで移動させる。このステップでは、定義を2つの境界のどちら側に配置するかについて、ある程度の自由度が存在する。

各`letrec`で定義された相互再帰的な定義群は、すべてまとめて移動させる必要がある。これらは互いに依存関係にあり、単一の`letrec`内で保持されなければならないからである。もし実際には、同じ`letrec`内で定義されていてもそれらが相互再帰的でない場合、この方針では遅延評価の利点が失われる可能性がある。つまり、本来はより外側に移動可能な定義が、内側のスコープに保持されてしまう場合があるのだ。標準的な解決策としては、各`letrec`式内の定義に対して**依存関係解析**を実施し、各定義群を最小の部分集合に分割する方法がある。この手法については第6.8節で詳しく説明する。
最後に、浮動処理の前に名前変更処理を実施する必要がある。これにより、`let(rec)`定義の移動によって束縛変数が変更されるリスクを回避できる。例えば、次の式は明らかに

```
\y. let y = x*x in y
```

と

```
let y = x*x in \y->y
```

とは等価ではない。必要なのは、名前衝突を回避するためにすべての束縛変数に一意の名前を付与することだけである。

### 6.6.3 Full laziness without lambda lifting

At first it appears that the requirement to float `let(rec)`s outward in order to preserve full laziness merely further complicates the already subtle fully lazy lambda lifting algorithm suggested by Hughes. However, a simple transformation allows all the full laziness to be achieved by `let(rec)` floating, while lambda lifting is performed by the original simple lambda lifter.

The transformation is this: **before floating `let(rec)` definitions, replace each MFE $e$ with the expression `let v =` $e$ `in v`**. This transformation both gives a name to the MFE and makes it accessible to the `let(rec)` floating transformation, which can now float out the new definitions. Ordinary lambda lifting can then be performed. For example, consider the original definition of `g`:

```
f x = let g = \y. x*x + y
      in (g 3 + g 4);
main = f 6
```

The sub-expression `x*x` is an MFE, so it is replaced by a trivial let expression:

```
f x = let g = \y. (let v = x*x in v) + y
      in (g 3 + g 4);
main = f 6
```

Now the let expression is floated outward:

```
f x = let g = let v = x*x in \y. v + y
      in (g 3 + g 4)
in
f 6
```

Finally, ordinary lambda lifting will discover that `v` is free in the `\y`-abstraction, and the resulting program becomes:

```
$g v y = v + y;
f x = let g = let v = x*x in $g v
      in (g 3 + g 4);
main = f 6
```

A few points should be noted here. Firstly, the original definition of a maximal free expression was relative to a **particular** lambda abstraction. The new algorithm we have just developed transforms certain expressions into trivial `let` expressions. Which expressions are so transformed? Just the ones which are MFEs of **any** enclosing lambda abstraction. For example, in the expression:

```
\y. \z. (y + (x*x)) / z
```

two MFEs are identified: `(x*x)`, since it is an MFE of the `\y`-abstraction, and `(y + (x*x))`, since it is an MFE of the `\z`-abstraction. After introducing the trivial let bindings, the expression becomes

```
\y. \z. (let v1 = y + (let v2 = x*x in v2) in v1) / z
```

Secondly, the newly introduced variable v must either be unique, or the expression must be uniquely renamed after the MFE-identification pass.

Thirdly, in the final form of the program v is only referenced once, so it would be sensible to replace the reference by the right-hand side of the definition and eliminate the definition, yielding exactly the program we obtained using Hughes’s algorithm. This is a straightforward transformation, and we will not discuss it further here, except to note that this property will hold for all `let` definitions which are floated out past a lambda. In any case, many compiler back ends will generate the same code regardless of whether or not the transformation is performed.

### 6.6.3 完全怠惰性とラムダ持ち上げの両立

一見すると、完全怠惰性を維持するために`let(rec)`式を外側に持ち上げる必要は、ヒューズが提案した既に複雑な完全遅延ラムダ持ち上げアルゴリズムをさらに複雑化させるように思える。しかし実際には、単純な変換を施すことで、`let(rec)`式の持ち上げだけで完全怠惰性を実現しつつ、ラムダ持ち上げ自体は従来の単純なラムダ持ち上げ器で処理することが可能となる。

この変換手法は以下の通りである：**`let(rec)`定義を持ち上げる前に、各最大自由式（MFE）$e$を`let v = `$e` in v`という式に置き換える**。この変換には二つの効果がある：第一に、MFEに名前を付与することで識別可能にし、第二に、`let(rec)`式の持ち上げ変換がこの新しい定義を利用できるようにすることで、外側への持ち上げ処理を可能にする。その後、通常のラムダ持ち上げ処理を実行すればよい。例えば、元の`g`の定義を考えてみよう：
```
f x = let g = \y. x*x + y
      in (g 3 + g 4);
main = f 6
```

この中の部分式`x*x`はMFEであるため、これを単純なlet式に置き換える：

```
f x = let g = \y. (let v = x*x in v) + y
      in (g 3 + g 4);
main = f 6
```

次に、このlet式を外側に持ち上げる：

```
f x = let g = let v = x*x in \y. v + y
      in (g 3 + g 4)
main = f 6
```

最終的に、通常のラムダ持ち上げ処理によって、`v`が`\y`の抽象化内で自由変数であることが検出され、最終的なプログラムは以下のようになる：

```
$g v y = v + y;
f x = let g = let v = x*x in $g v
      in (g 3 + g 4);
main = f 6
```
ここで注意すべき点がいくつかある。まず、最大自由式の定義は**特定の**ラムダ抽象を基準としていた。今回開発した新しいアルゴリズムでは、特定の式を単純な`let`式に変換する。どのような式がこのように変換されるのか？それは、**任意の**包含ラムダ抽象のMFEである式に限られる。例えば、以下の式を考えてみよう：

```
\y. \z. (y + (x*x)) / z
```

この式では2つのMFEが特定される：`(x*x)`は`\y`の抽象化のMFEであり、`(y + (x*x))`は`\z`の抽象化のMFEである。単純なlet束縛を導入した後、この式は以下のように変化する：
```
\y. \z. (let v1 = y + (let v2 = x*x in v2) in v1) / z
```

第二に、新たに導入される変数`v`は一意でなければならないか、あるいはMFE識別処理後に式が一意に名前変更されなければならない。

第三に、プログラムの最終形態では変数vは1回しか参照されないため、この参照を定義式の右辺で置き換え、定義自体を削除することが合理的である。これにより、ヒューズのアルゴリズムを用いて得られたものと全く同じプログラムが得られる。これは単純な変換処理であり、ここではこれ以上詳しく説明しないが、この性質はラムダ式の外側に浮動されたすべての`let`定義に対して成立することに留意すべきである。いずれにせよ、多くのコンパイラバックエンドでは、この変換を実行するかどうかにかかわらず、同じコードが生成される。

### 6.6.4 A fully lazy lambda lifter

Now we are ready to define the fully lazy lambda lifter. It can be decomposed into the following stages:

- First we must make sure that each `ELam` constructor and supercombinator definition binds only a single argument, because the fully lazy lambda lifter must treat each lambda individually. It would be possible to encode this in later phases of the algorithm, by dealing with a list of arguments, but it turns out that we can express an important optimisation by altering this pass alone.
  ```
  > separateLams :: CoreProgram -> CoreProgram
  ```
- First we annotate all binders and expressions with level numbers, which we represent by natural numbers starting with zero:
  ```
  > type Level = Int
  > addLevels :: CoreProgram -> AnnProgram (Name, Level) Level
  ```
- Next we identify all MFEs, by replacing them with trivial `let` expressions. Level numbers are no longer required on every sub-expression, only on binders.
  ```
  > identifyMFEs :: AnnProgram (Name, Level) Level -> Program (Name, Level)
  ```
- A renaming pass makes all binders unique, so that floating does not cause name-capture errors. This must be done after `identifyMFEs`, which introduces new bindings. Sadly, this means that we cannot use our current rename function because it works on a `coreProgram`, whereas `identifyMFEs` has produced a `program (name, level)`. We invent a new function `renameL` for the purpose:
  ```
  > renameL :: Program (Name, a) -> Program (Name, a)
  ```
- Now the `let(rec)` definitions can be floated outwards. The level numbers are not required any further.
  ```
  > float :: Program (Name,Level) -> CoreProgram
  ```
- Finally, ordinary lambda lifting can be carried out, using lambdaLift from Section 6.3.1.

The fully lazy lambda lifter is just the composition of these passes:

```
> fullyLazyLift = float . renameL . identifyMFEs . addLevels . separateLams
> runF          = pprint . lambdaLift . fullyLazyLift . parse
```

As before, we leave most of the equations for `case` expressions as an exercise.

### 6.6.4 完全怠惰ラムダリフト処理

ここで、いよいよ完全怠惰ラムダリフト処理の定義に移ることができる。この処理は以下の段階に分解できる：

- まず各`ELam`コンストラクタとスーパーコンビネータ定義が単一の引数のみを束縛するようにしなければならない。完全怠惰ラムダリフト処理では、各ラムダ式を個別に処理する必要があるためである。この制約はアルゴリズムの後段でリスト形式の引数を扱うことで実現可能ではあるが、この段階のみを変更するだけで重要な最適化を実現できることが分かっている。
  ```
  > separateLams :: CoreProgram -> CoreProgram
  ```
- 最初に、すべてのバインダと式にレベル番号を注釈として付与する。このレベル番号は0から始まる自然数で表現される：
  ```
  > type Level = Int
  > addLevels :: CoreProgram -> AnnProgram (Name, Level) Level
  ```
- 次に、すべてのMFE（最小関数式）を特定し、それらを単純な`let`式に置き換える。レベル番号はもはや全ての部分式に必要なわけではなく、バインダにのみ必要となる。
  ```
  > identifyMFEs :: AnnProgram (Name, Level) Level -> Program (Name, Level)
  ```
- 名前の再命名処理を行い、全てのバインダを一意にする。これにより、浮動処理時に名前捕捉エラーが発生するのを防ぐ。この処理は`identifyMFEs`の後で行う必要がある。`identifyMFEs`は新しいバインディングを導入するためである。残念ながら、現在の名前再命名関数をそのまま使用することはできない。なぜならこの関数は`coreProgram`を操作するのに対し、`identifyMFEs`は`program (name, level)`を生成するからである。この目的のために、新たに`renameL`関数を考案する：
  ```
  > renameL :: Program (Name, a) -> Program (Name, a)
  ```
- これで`let(rec)`定義を外側へ浮動させることができるようになる。この段階ではレベル番号はもはや不要となる。
  ```
  > float :: Program (Name,Level) -> CoreProgram
  ```
- 最後に、6.3.1節で定義した`lambdaLift`関数を用いて通常のラムダリフト処理を実行する。

  完全怠惰ラムダリフト処理は、これらの処理の合成によって定義される：

```
> fullyLazyLift = float . renameL . identifyMFEs . addLevels . separateLams
> runF          = pprint . lambdaLift . fullyLazyLift . parse
```

これまでの例と同様に、`case`式に関する方程式の大部分は演習問題として残しておく。

### 6.6.5 Separating the lambdas

We define separateLams in terms of an auxiliary function `separateLams_e`, which recursively separates variables bound in lambda abstractions in expressions:

```haskell
> separateLams_e :: CoreExpr -> CoreExpr
> separateLams_e (EVar v) = EVar v
> separateLams_e (EConstr t a) = EConstr t a
> separateLams_e (ENum n) = ENum n
> separateLams_e (EAp e1 e2) = EAp (separateLams_e e1) (separateLams_e e2)
> separateLams_e (ECase e alts)
>   = ECase (separateLams_e e) [ (tag, args, separateLams_e e)
>                              | (tag, args, e) <- alts
>                              ]
>
> separateLams_e (ELam args body) = mkSepLams args (separateLams_e body)
> 
> separateLams_e (ELet is_rec defns body)
>   = ELet is_rec [(name, separateLams_e rhs) | (name,rhs) <- defns]
>                 (separateLams_e body)
>
> mkSepLams args body = foldr mkSepLam body args
>   where mkSepLam arg body = ELam [arg] body
```

Now we return to the top-level function `separateLams`. The interesting question is what to do about supercombinator definitions. The easiest thing to do is to turn them into the equivalent lambda abstractions!

```haskell
> separateLams prog = [ (name, [], mkSepLams args (separateLams_e rhs))
>                     | (name, args, rhs) <- prog
>                     ]
```

### 6.6.5 ラムダ式の分離

補助関数 `separateLams_e` を用いて、式内のラムダ抽象で束縛された変数を再帰的に分離する `separateLams` を定義する：

```haskell
> separateLams_e :: CoreExpr -> CoreExpr
> separateLams_e (EVar v) = EVar v
> separateLams_e (EConstr t a) = EConstr t a
> separateLams_e (ENum n) = ENum n
> separateLams_e (EAp e1 e2) = EAp (separateLams_e e1) (separateLams_e e2)
> separateLams_e (ECase e alts)
>   = ECase (separateLams_e e) [ (tag, args, separateLams_e e)
>                              | (tag, args, e) <- alts
>                              ]
>
> separateLams_e (ELam args body) = mkSepLams args (separateLams_e body)
> 
> separateLams_e (ELet is_rec defns body)
>   = ELet is_rec [(name, separateLams_e rhs) | (name,rhs) <- defns]
>                 (separateLams_e body)
>
> mkSepLams args body = foldr mkSepLam body args
>   where mkSepLam arg body = ELam [arg] body
```

次に、最上位関数 `separateLams` に戻る。興味深いのはスーパーコンビネータ定義をどう扱うかという点だ。最も簡単な方法は、それらを等価なラムダ抽象に変換することである！

```haskell
> separateLams prog = [ (name, [], mkSepLams args (separateLams_e rhs))
>                     | (name, args, rhs) <- prog
>                     ]
```

### 6.6.6 Adding level numbers

There are a couple of complications concerning annotating an expression with level numbers. At first it looks as though it is sufficient to write a function which returns an expression annotated with level numbers; then for an application, for example, one simply takes the maximum of the levels of the two sub-expressions. Unfortunately, this approach loses too much information, because there is no way of mapping the level number of the **body** of a lambda abstraction to the level number of the abstraction **itself**. The easiest solution is first to annotate the expression with its free variables, and then use a mapping `freeSetToLevel` from variables to level numbers, to convert the free-variable annotations to level numbers.

```haskell
> freeSetToLevel :: ASSOC Name Level -> Set Name -> Level
> freeSetToLevel env free
>   = foldll max 0 [aLookup env n 0 | n <- setToList free]
>     -- If there are no free variables, return level zero
```

The second complication concerns `letrec` expressions. What is the correct level number to attribute to the newly introduced variables? The right thing to do is to take the maximum of the levels of the free variables of all the right-hand sides **without** the recursive variables, or equivalently map the recursive variables to level zero when taking this maximum. This level should be attributed to each of the new variables. `let` expressions are much simpler: just attribute to each new variable the level number of its right-hand side.

Now we are ready to define `addLevels`. It is the composition of two passes, the first of which annotates the expression with its free variables, while the second uses this information to generate level-number annotations.

```haskell
> addLevels = freeToLevel . freeVars
```

We have defined the `freeVars` function already, so it remains to define `freeToLevel`. The main function will need to carry around the current level, and a mapping from variables to level numbers, so as usual we define `freeToLevel` in terms of `freeToLevel_e` which does all the work.

```haskell
> freeToLevel_e :: Level                       -- Level of context
>               -> Assoc Name Level            -- Level of in-scope names
>               -> AnnExpr Name (Set Name)     -- Input expression
>               -> AnnExpr (Name, Level) Level -- Result expression
```

We represent the name-to-level mapping as an association list, with type `Assoc Name Level`. The interface of association lists is given in Appendix A, but notice that it is **not** abstract. It is so convenient to use all the standard functions on lists, and notation for lists, rather than to invent their analogues for associations, that we have compromised the abstraction.

Now we can define `freeToLevel`, using an auxiliary function to process each supercombinator definition. Remember that `separateLams` has removed all the arguments from supercombinator definitions:

```haskell
> freeToLevel prog = map freeToLevel_sc prog
>
> freeToLevel_sc (sc_name, [], rhs) = (sc_name, [], freeToLevel_e 0 [] rhs)
```

For constants, variables and applications, it is simpler and more efficient to ignore the free-variable information and calculate the level number directly.

```haskell
> freeToLevel_e level env (free, ANum k) = (0, ANum k)
> freeToLevel_e level env (free, AVar v) = (aLookup env v 0, AVar v)
> freeToLevel_e level env (free, AConstr t a) = (0, AConstr t a)
> freeToLevel_e level env (free, AAp e1 e2)
>     = (max (levelOf e1’) (levelOf e2’), AAp e1’ e2’)
>     where
>         e1’ = freeToLevel_e level env e1
>         e2’ = freeToLevel_e level env e2
```

The same cannot be done for lambda abstractions; so we must compute the level number of the abstraction using `freeSetToLevel`. We also assign a level number to each variable in the argument list. At present we expect there to be only one such variable, but we will allow there to be several and assign them all the same level number. This works correctly now, and turns out to be just what is needed to support a useful optimisation later (Section 6.7.3).

```haskell
> freeToLevel_e level env (free, ALam args body)
>     = (freeSetToLevel env free, ALam args’ body’)
>     where
>         body’ = freeToLevel_e (level + 1) (args’ ++ env) body
>         args’ = [(arg, level+1) | arg <- args]
```

`let(rec)` expressions follow the scheme outlined at the beginning of this section.

```haskell
> freeToLevel_e level env (free, ALet is_rec defns body)
>     = (levelOf new_body, ALet is_rec new_defns new_body)
>     where
>         binders = bindersOf defns
>         rhss = rhssOf defns
> 
>         new_binders = [(name,max_rhs_level) | name <- binders]
>         new_rhss = map (freeToLevel_e level rhs_env) rhss
>         new_defns = zip2 new_binders new_rhss
>         new_body = freeToLevel_e level body_env body
>
>         free_in_rhss = setUnionList [free | (free,rhs) <- rhss]
>         max_rhs_level = freeSetToLevel level_rhs_env free_in_rhss
> 
>         body_env            = new_binders ++ env
>         rhs_env | is_rec    = body_env
>                 | otherwise = env
>         level_rhs_env | is_rec    = [(name,0) | name <- binders] ++ env
>                       | otherwise = env
```

Notice that the level of the whole `let(rec)` expression is that of the body. This is valid provided the body refers to all the binders directly or indirectly. If any definition is unused, we might assign a level number to the `letrec` which would cause it to be floated outside the scope of some variable mentioned in the unused definition. This is easily fixed, but it is simpler to assume that the expression contains no redundant definitions; the dependency analysis which we look at in the next section will eliminate such definitions.

`case` expressions are deferred:

```haskell
> freeToLevel_e level env (free, ACase e alts)
>     = freeToLevel_case level env free e alts
> freeToLevel_case free e alts = error "freeToLevel_case: not yet written"
```

Lastly the auxiliary functions `levelOf` extracts the level from an expression:

```haskell
> levelOf :: AnnExpr a Level -> Level
> levelOf (level, e) = level
```

### 6.6.6 レベル番号の追加

式にレベル番号を注釈として付与する際には、いくつかの注意点がある。一見すると、レベル番号を付与した式を返す関数を定義すれば十分のように思える。例えば実際の適用処理では、2つの部分式のレベル番号のうち最大値を取ればよいと考えるかもしれない。しかし、この方法では重要な情報が失われてしまう。なぜなら、ラムダ抽象の**本体**のレベル番号を、抽象そのもののレベル番号に対応付ける方法が存在しないからだ。最も簡単な解決策は、まず式の自由変数に注釈を付与し、次に変数からレベル番号へのマッピング関数`freeSetToLevel`を用いて、自由変数の注釈をレベル番号に変換する方法である。
以下にその実装例を示す：
```haskell
> freeSetToLevel :: ASSOC Name Level -> Set Name -> Level
> freeSetToLevel env free
>   = foldll max 0 [aLookup env n 0 | n <- setToList free]
>     -- 自由変数が存在しない場合はレベル0を返す
```

二つ目の注意点は`letrec`式に関するものである。新たに導入される変数に付与すべき正しいレベル番号は何か？適切な方法は、再帰変数を除いたすべての右辺の自由変数のレベル番号の最大値を取ること、あるいは同等の方法として、この最大値を求める際に再帰変数をレベル0にマッピングすることである。このレベル番号は、新たに導入される各変数に割り当てるべきである。`let`式の場合はさらに単純で、各新規変数にはその右辺のレベル番号をそのまま割り当てればよい。
`addLevels`関数の定義準備が整った。これは二つの処理の合成であり、第一の処理では式に自由変数の注釈を付与し、第二の処理ではこの情報を用いてレベル番号の注釈を生成する。

```haskell
> addLevels = freeToLevel . freeVars
```

`freeVars`関数は既に定義済みであるため、残るは`freeToLevel`関数の定義のみである。メイン関数では現在のレベル番号と変数からレベル番号へのマッピングを保持する必要があるため、通常の手順に従い、実際の処理を行う`freeToLevel_e`関数を定義した上で`freeToLevel`関数を定義する。
以下がその定義である：
```haskell
> freeToLevel_e :: Level                       -- 文脈のレベル
>               -> Assoc Name Level            -- スコープ内の名前とそのレベルの対応関係
>               -> AnnExpr Name (Set Name)     -- 入力式
>               -> AnnExpr (Name, Level) Level -- 結果の式
```

名前とレベルの対応関係は`Assoc Name Level`型の連想リストとして表現する。連想リストのインターフェースは付録Aに記載されているが、注意すべき点としてこれは抽象化されていない仕様である。リストに対する標準的な関数やリスト記法をそのまま利用する方が、連想リスト用に類似の機能を一から実装するよりもはるかに便利であるため、我々は抽象化の原則を若干妥協している。
次に、補助関数を用いて各スーパーコンビネータ定義を処理しながら`freeToLevel`を定義する。`separateLams`関数がスーパーコンビネータ定義からすべての引数を除去していることを思い出してほしい：

```haskell
> freeToLevel prog = map freeToLevel_sc prog
>
> freeToLevel_sc (sc_name, [], rhs) = (sc_name, [], freeToLevel_e 0 [] rhs)
```

定数、変数、関数適用については、自由変数情報を考慮せず、直接レベル番号を計算する方が簡潔で効率的である。

```haskell
> freeToLevel_e level env (free, ANum k) = (0, ANum k)
> freeToLevel_e level env (free, AVar v) = (aLookup env v 0, AVar v)
> freeToLevel_e level env (free, AConstr t a) = (0, AConstr t a)
> freeToLevel_e level env (free, AAp e1 e2)
>     = (max (levelOf e1’) (levelOf e2’), AAp e1’ e2’)
>     where
>         e1’ = freeToLevel_e level env e1
>         e2’ = freeToLevel_e level env e2
```
ラムダ抽象については同様の処理は行えないため、`freeSetToLevel`を用いて抽象のレベル番号を計算する必要がある。また、引数リスト内の各変数にもレベル番号を割り当てる。現時点では変数は1つだけと想定しているが、複数存在する場合にも対応し、すべての変数に同じレベル番号を割り当てるようにしている。この実装は現在正しく動作しており、後で有用な最適化を実装する際に必要となる要件を満たしていることが判明した（第6.7.3節参照）。

```haskell
> freeToLevel_e level env (free, ALam args body)
>     = (freeSetToLevel env free, ALam args’ body’)
>     where
>         body’ = freeToLevel_e (level + 1) (args’ ++ env) body
>         args’ = [(arg, level+1) | arg <- args]
```
`let(rec)`式については、本節の冒頭で説明した方式に従って処理を行う。

```haskell
> freeToLevel_e level env (free, ALet is_rec defns body)
>     = (levelOf new_body, ALet is_rec new_defns new_body)
>     where
>         binders = bindersOf defns
>         rhss = rhssOf defns
> 
>         new_binders = [(name,max_rhs_level) | name <- binders]
>         new_rhss = map (freeToLevel_e level rhs_env) rhss
>         new_defns = zip2 new_binders new_rhss
>         new_body = freeToLevel_e level body_env body
>
>         free_in_rhss = setUnionList [free | (free,rhs) <- rhss]
>         max_rhs_level = freeSetToLevel level_rhs_env free_in_rhss
> 
>         body_env            = new_binders ++ env
>         rhs_env | is_rec    = body_env
>                 | otherwise = env
>         level_rhs_env | is_rec    = [(name,0) | name <- binders] ++ env
>                       | otherwise = env
```

注目すべきは、`let(rec)`式全体のレベルは本体のレベルと同一であるという点である。この処理方式は、本体がすべての束縛変数を直接的または間接的に参照している場合にのみ有効である。もし定義の一部が未使用である場合、`letrec`にレベル番号を割り当てることで、未使用の定義で言及されている変数のスコープ外にその定義が浮動する可能性がある。これは容易に修正可能だが、より単純なアプローチとして、式内に冗長な定義が存在しないと仮定する方法がある。次節で解説する依存関係解析は、このような定義を自動的に排除する機能を備えている。

`case`式の処理は遅延実行される：

```haskell
> freeToLevel_e level env (free, ACase e alts)
>     = freeToLevel_case level env free e alts
> freeToLevel_case free e alts = error "freeToLevel_case: 未実装"
```

最後に、補助関数`levelOf`は式からレベル情報を抽出する役割を持つ：

```haskell
> levelOf :: AnnExpr a Level -> Level
> levelOf (level, e) = level
```

### 6.6.7 Identifying MFEs

It is simple to identify MFEs, by comparing the level number of an expression with the level of its context. This requires an auxiliary parameter to give the level of the context.

```haskell
> identifyMFEs_e :: Level                       -- Level of context
>                -> AnnExpr (Name, Level) Level -- Input expression
>                -> Expr (Name, Level)          -- Result
> identifyMFEs prog = [ (sc_name, [], identifyMFEs_e 0 rhs)
>                     | (sc_name, [], rhs) <- prog
>                     ]
```

Once an MFE $e$ has been identified, our strategy is to wrap it in a trivial `let` expression of the form `let v = `$e$` in v`; but not all MFEs deserve special treatment in this way. For example, it would be a waste of time to wrap such a `let` expression around an MFE consisting of a single variable or constant. Other examples are given in Section 6.7.3. We encode this knowledge of which MFEs deserve special treatment in a function `notMFECandidate`.

```haskell
> notMFECandidate (AConstr t a) = True
> notMFECandidate (ANum k)      = True
> notMFECandidate (AVar v)      = True
> notMFECandidate ae            = False -- For now everything else
>                                       -- is a candidate
```

`identifyMFEs_e` works by comparing the level number of the expression with that of its context. If they are the same, or for some other reason the expression is not a candidate for special treatment, the expression is left unchanged, except that `identifyMFEs_e1` is used to apply `identifyMFEs_e` to its sub-expressions; otherwise we use `transformMFE` to perform the appropriate transformation.

```haskell
> identifyMFEs_e cxt (level, e)
>     | level == cxt || notMFECandidate e = e’
>     | otherwise                         = transformMFE level e’
>       where
>           e’ = identifyMFEs_e1 level e
>
> transformMFE level e = ELet nonRecursive [(("v",level), e)] (EVar "v")
```

`identifyMFEs_e1` applies `identifyMFEs_e` to the components of the expression.

```haskell
> identifyMFEs_e1 :: Level                       -- Level of context
>                 -> AnnExpr’ (Name,Level) Level -- Input expressions
>                 -> Expr (Name,Level)           -- Result expression
> 
> identifyMFEs_e1 level (AConstr t a) = EConstr t a
> identifyMFEs_e1 level (ANum n)      = ENum n
> identifyMFEs_e1 level (AVar v)      = EVar v
> identifyMFEs_e1 level (AAp e1 e2)   
>     = EAp (identifyMFEs_e level e1) (identifyMFEs_e level e2)
```

When `identifyMFEs_e1` encounters a binder it changes the ‘current’ level number carried down as its first argument, as we can see in the equations for lambda abstractions and `let(rec)` expressions:

```haskell
> identifyMFEs_e1 level (ALam args body)
>     = ELam args (identifyMFEs_e arg_level body)
>     where
>         (name, arg_level) = hd args
> 
> identifyMFEs_e1 level (ALet is_rec defns body)
>     = ELet is_rec defns’ body’
>     where
>         body’ = identifyMFEs_e level body
>         defns’ = [ ((name, rhs_level), identifyMFEs_e rhs_level rhs)
>                  | ((name, rhs_level), rhs) <- defns
>                  ]
```

`case` expressions are deferred:

```haskell
> identifyMFEs_e1 level (ACase e alts) = identifyMFEs_case1 level e alts
> identifyMFEs_case1 level e alts = error "identifyMFEs_case1: not written"
```

### 6.6.7 MFEの特定

MFEを特定する処理は単純で、式のレベル値とその文脈のレベル値を比較することで実現できる。この処理には、文脈のレベル値を指定するための補助パラメータが必要となる。

```haskell
> identifyMFEs_e :: Level                       -- 文脈のレベル値
>                -> AnnExpr (Name, Level) Level -- 入力式
>                -> Expr (Name, Level)          -- 結果
> identifyMFEs prog = [ (sc_name, [], identifyMFEs_e 0 rhs)
>                     | (sc_name, [], rhs) <- prog
>                     ]
```

MFE $e$が特定された後の処理方針として、`let v = `$e$` in v` という形式の単純な `let` 式で囲む方法を採用する。ただし、すべてのMFEがこのような特別な扱いを必要とするわけではない。例えば、単一の変数や定数からなるMFEに対してこのような `let` 式を適用するのは無駄である。このような例外的なケースについては、第6.7.3節で具体例を挙げて説明する。どのMFEが特別な扱いを必要とするかについての知識は、関数 `notMFECandidate` として実装されている。

```haskell
> notMFECandidate (AConstr t a) = True
> notMFECandidate (ANum k)      = True
> notMFECandidate (AVar v)      = True
> notMFECandidate ae            = False -- 現時点ではその他すべてのケースを候補とする
>                                       -- と定義する
```
`identifyMFEs_e` 関数は、式のレベル番号とその文脈のレベル番号を比較することで動作する。両者が一致する場合、あるいはその他の理由で式が特別な処理の候補に該当しない場合、式は変更されずにそのまま返される。ただし、`identifyMFEs_e1` 関数を用いて式の部分式に対して `identifyMFEs_e` を適用する点が異なる。それ以外の場合は、`transformMFE` 関数を使用して適切な変換処理を実行する。

```haskell
> identifyMFEs_e cxt (level, e)
>     | level == cxt || notMFECandidate e = e’
>     | otherwise                         = transformMFE level e’
>       where
>           e’ = identifyMFEs_e1 level e
>
> transformMFE level e = ELet nonRecursive [(("v",level), e)] (EVar "v")
```

`identifyMFEs_e1` 関数は、式の各構成要素に対して `identifyMFEs_e` 関数を適用する。

```haskell
> identifyMFEs_e1 :: Level                       -- 文脈のレベル
>                 -> AnnExpr’ (Name,Level) Level -- 入力式
>                 -> Expr (Name,Level)           -- 結果式
> 
> identifyMFEs_e1 level (AConstr t a) = EConstr t a
> identifyMFEs_e1 level (ANum n)      = ENum n
> identifyMFEs_e1 level (AVar v)      = EVar v
> identifyMFEs_e1 level (AAp e1 e2)   
>     = EAp (identifyMFEs_e level e1) (identifyMFEs_e level e2)
```

`identifyMFEs_e1` がバインダに遭遇すると、最初の引数として渡される「現在の」レベル番号を変更する。これはラムダ抽象や `let(rec)` 式の定義式から確認できる：

```haskell
> identifyMFEs_e1 level (ALam args body)
>     = ELam args (identifyMFEs_e arg_level body)
>     where
>         (name, arg_level) = hd args
> 
> identifyMFEs_e1 level (ALet is_rec defns body)
>     = ELet is_rec defns’ body’
>     where
>         body’ = identifyMFEs_e level body
>         defns’ = [ ((name, rhs_level), identifyMFEs_e rhs_level rhs)
>                  | ((name, rhs_level), rhs) <- defns
>                  ]
```
`case` 式については処理を遅延させている：

```haskell
> identifyMFEs_e1 level (ACase e alts) = identifyMFEs_case1 level e alts
> identifyMFEs_case1 level e alts = error "identifyMFEs_case1: not implemented"
```

### 6.6.8 Renaming variables

As we remarked above, it would be nice to use the existing `rename` function to make the binders unique, but it has the wrong type. It would be possible to write `renameL` by making a copy of rename and making some small alterations, but it would be much nicer to make a single generic renaming function, `renameGen`, which can be specialised to do either `rename` or `renameL`.

What should the type of `renameGen` be? The right question to ask is: **‘what use did we make in `rename` of the fact that each binder was a simple `name`?’** or, alternatively, ‘what operations did we perform on binders in `rename`?’.

There is actually just one such operation, which constructs new binders. In `rename_e` this function is called `newNames`; it takes a name supply and a list of names, and returns a depleted name supply, a list of new names and an association list mapping old names to new ones:

```haskell
> newNames :: nameSupply -> [name] -> (nameSupply, [name], assoc name name)
```

Since `renameGen` must be able to work over any kind of binder, not just those of type `name`, **we must pass the new-binders function into `renameGen` as an extra argument**. So the type of `renameGen` is:

```haskell
> renameGen :: (NameSupply -> [a] -> (NameSupply, [a], ASSOC Name Name))
>                        -- New-binders function
>           -> Program a -- Program to be renamed
>           -> Program a -- Resulting program
```

Notice that the type of the binders is denoted by the type variable `a`, because `renameGen` is polymorphic in this type. Using `renameGen`, we can now redefine the original `rename` function, by passing `newNames` to `renameGen` as the new-binders function.

```haskell
> rename :: CoreProgram -> CoreProgram
> rename prog = renameGen newNames prog
```

`renameL` is rather more interesting. Its binders are `(Name,Level)` pairs so we need to define a different new-binders function:

```haskell
> renameL :: Program (Name,Level) -> Program (Name,Level)
> renameL prog = renameGen newNamesL prog
```

The function `newNamesL` does just what `newNames` does, but it does it for binders whose type is a `(Name,Level)` pair:

```
> newNamesL ns old_binders
>     = (ns’, new_binders, env)
>     where
>         old_names = [name | (name,level) <- old_binders]
>         levels = [level | (name,level) <- old_binders]
>         (ns’, new_names) = getNames ns old_names
>         new_binders = zip2 new_names levels
>         env = zip2 old_names new_names
```

Now we can turn our attention to writing `renameGen`. As usual we need an auxiliary function `renameGen_e` which carries around some extra administrative information. Specifically, like `rename_e`, it needs to take a name supply and old-name to new-name mapping as arguments, and return a depleted supply as part of its result. It also needs to be passed the new-binders function:

```haskell
> renameGen_e :: (NameSupply -> [a] -> (NameSupply, [a], ASSOC Name Name))
>                                     -- New-binders function
>             -> ASSOC Name Name      -- Maps old names to new ones
>             -> NameSupply           -- Name supply
>             -> Expr a               -- Expression to be renamed
>             -> (NameSupply, Expr a) -- Depleted name supply
>                                     -- and result expression
```

Using `renameGen_e` we can now write `renameGen`. Just like `rename`, `renameGen` applies a local function `rename_sc` to each supercombinator definition.

```haskell
> renameGen new_binders prog
>     = second (mapAccuml rename_sc initialNameSupply prog)
>     where
>         rename_sc ns (sc_name, args, rhs)
>             = (ns2, (sc_name, args’, rhs’))
>             where
>                 (ns1, args’, env) = new_binders ns args
>                 (ns2, rhs’) = renameGen_e new_binders env ns1 rhs
```

#### Exercise 6.9.

Write the function `renameGen_e`. It is very like `rename_e`, except that it takes the binder-manipulation functions as extra arguments. In the equations for `ELet`, `ELam` and `ECase` (which each bind new variables), the function `newBinders` can be used in just the same way as it is in `rename_sc` above.
Test your definition by checking that the simple lambda lifter still works with the new definition of `rename`.

#### Exercise 6.10.

The type signature we wrote for `renameL` is actually slightly more restrictive than it need be. How could it be made more general (without changing the code at all)? Hint: what use does `renameL` make of the fact that the second component of a binder is of type `level`?

This section provides a good illustration of the way in which higher-order functions can help us to make programs more modular.

### 6.6.8 変数名の変更

前述したように、既存の `rename` 関数を利用してバインダを一意に命名できれば理想的だが、現在の型定義は適切ではない。`rename` 関数をコピーして若干の修正を加えれば `renameL` を作成することも可能だが、より望ましいのは、`rename` と `renameL` の両方を特殊化できる汎用的な改名関数 `renameGen` を一から定義することである。

`renameGen` の型定義はどのようにすべきだろうか？ 適切な問いは次の通りである：**「`rename` 関数において、各バインダが単純な `name` 型であるという性質をどのように活用していたか？」** あるいは別の言い方をすれば、「`rename` 関数内でバインダに対してどのような操作を行っていたか？」ということである。
実際には、この操作は1種類しかなく、それは新しいバインダを構築する機能である。`rename_e` ではこの関数は `newNames` と呼ばれ、名前供給源と名前のリストを受け取り、消費された名前供給源、新規生成された名前のリスト、および古い名前と新しい名前を対応付ける連想リストを返す：

```haskell
> newNames :: nameSupply -> [name] -> (nameSupply, [name], assoc name name)
```

`renameGen` はあらゆる種類のバインダ（`name` 型に限定されない）に対して動作可能である必要があるため、**新しいバインダ生成関数を `renameGen` の追加引数として渡す必要がある**。したがって、`renameGen` の型定義は以下のようにすべきである：
```haskell
> renameGen :: (NameSupply -> [a] -> (NameSupply, [a], ASSOC Name Name))
>                        -- 新しいバインダ生成関数
>           -> Program a -- リネーム対象のプログラム
>           -> Program a -- リネーム後のプログラム
```

バインダの型が型変数 `a` で表されていることに注意されたい。これは `renameGen` がこの型に関して多相的であるためである。`renameGen` を使用することで、`newNames` を新しいバインダ生成関数として `renameGen` に渡すことで、元の `rename` 関数を再定義することが可能となる。

```haskell
> rename :: CoreProgram -> CoreProgram
> rename prog = renameGen newNames prog
```
`renameL` はさらに興味深い実装となっている。そのバインダは `(Name,Level)` ペアであるため、異なる新しいバインダ生成関数を定義する必要がある：

```haskell
> renameL :: Program (Name,Level) -> Program (Name,Level)
> renameL prog = renameGen newNamesL prog
```

関数 `newNamesL` は `newNames` と同様の機能を持つが、こちらは型が `(Name,Level)` ペアであるバインダに対して適用されるものである：

```
> newNamesL ns old_binders
>     = (ns’, new_binders, env)
>     where
>         old_names = [name | (name,level) <- old_binders]
>         levels = [level | (name,level) <- old_binders]
>         (ns’, new_names) = getNames ns old_names
>         new_binders = zip2 new_names levels
>         env = zip2 old_names new_names
```
ここで `renameGen` の実装に移る。通常と同様に、補助関数 `renameGen_e` が必要となる。この関数は追加の管理情報を保持する役割を持つ。具体的には、`rename_e` と同様に名前供給と古い名前から新しい名前へのマッピングを引数として受け取り、結果の一部として消費された名前供給を返す必要がある。また、新しいバインダ生成関数も引数として渡す必要がある：

```haskell
> renameGen_e :: (NameSupply -> [a] -> (NameSupply, [a], ASSOC Name Name))
>                                     -- 新しいバインダ生成関数
>             -> ASSOC Name Name      -- 古い名前から新しい名前へのマッピング
>             -> NameSupply           -- 名前供給
>             -> Expr a               -- リネーム対象の式
>             -> (NameSupply, Expr a) -- 消費された名前供給と結果の式
```
`renameGen_e` を使用することで、`renameGen` を実装できる。`rename` と同様に、`renameGen` は各スーパーコンビネータ定義に対してローカル関数 `rename_sc` を適用する。

```haskell
> renameGen new_binders prog
>     = second (mapAccuml rename_sc initialNameSupply prog)
>     where
>         rename_sc ns (sc_name, args, rhs)
>             = (ns2, (sc_name, args', rhs_prime))
>             where
>                 (ns1, args', env) = new_binders ns args
>                 (ns2, rhs_prime) = renameGen_e new_binders env ns1 rhs
```
#### 演習 6.9.

関数 `renameGen_e` を実装せよ。この関数は `rename_e` と非常に似ているが、追加の引数としてバインダ操作関数を受け取る点が異なる。`ELet`、`ELam`、`ECase` の定義式（いずれも新しい変数を束縛する場合）では、`newBinders` 関数を上記の `rename_sc` と同様に使用できる。
定義の動作確認として、単純なラムダリフト関数が新しい `rename` 定義でも正しく動作することを確認せよ。

#### 演習 6.10.

`renameL` に対して記述した型シグネチャは、実際には必要以上に制約が強い。コードを一切変更せずにより汎用的にするにはどうすればよいか？ヒント：`renameL` がバインダの第二要素が型 `level` であるという事実をどのように利用しているかを考察せよ。
本節は、高階関数を用いることでプログラムのモジュール性をどのように向上させられるかを示す好例である。

### 6.6.9 Floating `let(rec)` expressions

The final pass floats `let(rec)` expressions out to the appropriate level. The auxiliary function, which works over expressions, has to return an expression together with the collection of definitions which should be ffoated outside the expression.

```haskell
> float_e :: Expr (Name, Level) -> (FloatedDefns, Expr Name)
```

There are many possible representations for the `floatedDefns` type, and we will choose a simple one, by representing the definitions being floated as a list, each element of which represents a group of definitions, identified by its level, and together with its `isRec` flag.

```haskell
> type FloatedDefns = [(Level, IsRec, [(Name, Expr Name)])]
```

Since the definitions in the list may depend on one another, we add the following constraint:

> a definition group may depend only on definition groups appearing earlier in the `floatedDefns` list.

We can now proceed to a definition of `float_e`. The cases for variables, constants and applications are straightforward.

```haskell
> float_e (EVar v) = ([], EVar v)
> float_e (EConstr t a) = ([], EConstr t a)
> float_e (ENum n) = ([], ENum n)
> float_e (EAp e1 e2) = (fd1 ++ fd2, EAp e1’ e2’)
>     where
>         (fd1, e1’) = float_e e1
>         (fd2, e2’) = float_e e2
```

How far out should a definition be floated? There is more than one possible choice, but here we choose to install a definition just inside the innermost lambda which binds one its free variables (recall from Section 6.6.6 that all variables bound by a single `ELam` construct are given the same level):

```haskell
> float_e (ELam args body)
>     = (fd_outer, ELam args’ (install fd_this_level body’))
>     where
>         args’ = [arg | (arg,level) <- args]
>         (first_arg,this_level) = hd args
>         (fd_body, body’) = float_e body
>         (fd_outer, fd_this_level) = partitionFloats this_level fd_body
```

The equation for a `let(rec)` expression adds its definition group to those floated out from its body, and from its right-hand sides. The latter must come first, since the new definition group may depend on them.

```haskell
> float_e (ELet is_rec defns body)
>     = (rhsFloatDefns ++ [thisGroup] ++ bodyFloatDefns, body’)
>     where
>         (bodyFloatDefns, body’) = float_e body
>         (rhsFloatDefns, defns’) = mapAccuml float_defn [] defns
>         thisGroup = (thisLevel, is_rec, defns’)
>         (name,thisLevel) = hd (bindersOf defns)
>
>         float_defn floatedDefns ((name,level), rhs)
>             = (rhsFloatDefns ++ floatedDefns, (name, rhs’))
>             where
>                 (rhsFloatDefns, rhs’) = float_e rhs
```

We defer `case` expressions:

```haskell
> float_e (ECase e alts) = float_case e alts
> float_case e alts = error "float_case: not yet written"
```

The auxiliary function `partitionFloats` takes a `floatedDefns` and a level number, and separates it into two: those belonging to an outer level and those belonging to the specified level (or an inner one):

```haskell
> partitionFloats :: Level -> FloatedDefns -> (FloatedDefns, FloatedDefns)
> partitionFloats this_level fds
>     = (filter is_outer_level fds, filter is_this_level fds)
>     where
>         is_this_level (level,is_rec,defns) = level >= this_level
>         is_outer_level (level,is_rec,defns) = level < this_level
```

The function `install` wraps an expression in a nested set of `let(rec)`s containing the specified definitions:

```haskell
> install :: FloatedDefns -> Expr Name -> Expr Name
> install defnGroups e
>     = foldr installGroup e defnGroups
>     where
>         installGroup (level, is_rec, defns) e = ELet is_rec defns e
```

Finally, we can define the top-level function, `float`. It uses `float_sc` to apply `float_e` to each supercombinator, yielding a list of supercombinators, in just the same way as `collectSCs` above.

```haskell
> float prog = concat (map float_sc prog)
```

The function `float_sc` takes a supercombinator definition to a list of supercombinator definitions, consisting of the transformed version of the original definition together with the level-zero definitions floated out from its body:

```haskell
> float_sc (name, [], rhs)
>     = [(name, [], rhs’)] ++ concat (map to_scs fds)
>     where
>         (fds, rhs’) = float_e rhs
>         to_scs (level, is_rec, defns) = map make_sc defns
>         make_sc (name, rhs) = (name, [], rhs)
```

The top level of a program is implicitly mutually recursive, so we can drop the `isRec` flags. We also have to give each floated definition an empty argument list, since it is now a supercombinator definition.

### 6.6.9 `let(rec)` 式の浮動処理

最終処理段階では、`let(rec)` 式を適切なレベルまで浮動させる。式を処理する補助関数は、式そのものと、式の外部に浮動させるべき定義群の両方を返す必要がある。

```haskell
> float_e :: Expr (Name, Level) -> (FloatedDefns, Expr Name)
```
`floatedDefns` 型には複数の表現方法が可能だが、ここでは単純なリスト表現を採用する。各要素はレベルによって識別される定義群を表し、`isRec` フラグと共に保持される。

```haskell
> type FloatedDefns = [(Level, IsRec, [(Name, Expr Name)])]
```

リスト内の定義は相互に依存する可能性があるため、以下の制約を追加する：

> 定義群は、`floatedDefns` リスト内でそれ以前に現れる定義群のみに依存できるものとする。
これで `float_e` の定義に進むことができる。変数、定数、適用の各ケースは自明である。

```haskell
> float_e (EVar v) = ([], EVar v)
> float_e (EConstr t a) = ([], EConstr t a)
> float_e (ENum n) = ([], ENum n)
> float_e (EAp e1 e2) = (fd1 ++ fd2, EAp e1’ e2’)
>     where
>         (fd1, e1’) = float_e e1
>         (fd2, e2’) = float_e e2
```

定義をどこまで外側に移動させるべきか？複数の選択肢があるが、ここでは最も内側のラムダ式で、その自由変数を束縛する最も内側のラムダ式のすぐ内側に定義を配置することとする（第6.6.6節で述べたように、単一の `ELam` 構造で束縛されるすべての変数は同じレベルが与えられる）。
```haskell
> float_e (ELam args body)
>     = (fd_outer, ELam args’ (install fd_this_level body’))
>     where
>         args’ = [arg | (arg,level) <- args]
>         (first_arg,this_level) = hd args
>         (fd_body, body’) = float_e body
>         (fd_outer, fd_this_level) = partitionFloats this_level fd_body
```

`let(rec)` 式の方程式では、その定義グループを本体から移動させた定義群と、右辺から移動させた定義群に追加する。後者は先に配置しなければならない。なぜなら、新しい定義グループがこれらに依存する可能性があるからだ。
```haskell
> float_e (ELet is_rec defns body)
>     = (rhsFloatDefns ++ [thisGroup] ++ bodyFloatDefns, body’)
>     where
>         (bodyFloatDefns, body’) = float_e body
>         (rhsFloatDefns, defns’) = mapAccuml float_defn [] defns
>         thisGroup = (thisLevel, is_rec, defns’)
>         (name,thisLevel) = hd (bindersOf defns)
>
>         float_defn floatedDefns ((name,level), rhs)
>             = (rhsFloatDefns ++ floatedDefns, (name, rhs’))
>             where
>                 (rhsFloatDefns, rhs’) = float_e rhs
```

`case`式の処理は後回しにする：

```haskell
> float_e (ECase e alts) = float_case e alts
> float_case e alts = error "float_case: 未実装"
```

補助関数 `partitionFloats` は `floatedDefns` とレベル番号を受け取り、それらを2つに分割する：外側のレベルに属するものと、指定されたレベル（または内側のレベル）に属するものに分ける：

```haskell
> partitionFloats :: Level -> FloatedDefns -> (FloatedDefns, FloatedDefns)
> partitionFloats this_level fds
>     = (filter is_outer_level fds, filter is_this_level fds)
>     where
>         is_this_level (level,is_rec,defns) = level >= this_level
>         is_outer_level (level,is_rec,defns) = level < this_level
```
関数 `install` は指定された定義を含む入れ子の `let(rec)` 構造で式をラップする：

```haskell
> install :: FloatedDefns -> Expr Name -> Expr Name
> install defnGroups e
>     = foldr installGroup e defnGroups
>     where
>         installGroup (level, is_rec, defns) e = ELet is_rec defns e
```

最終的に、最上位レベルの関数 `float` を定義する。この関数は `float_sc` を使って各スーパーコンビネータに `float_e` を適用し、前述の `collectSCs` と同様に、スーパーコンビネータのリストを生成する。
以下がその実装である：
```haskell
> float prog = concat (map float_sc prog)
```

関数 `float_sc` はスーパーコンビネータの定義を受け取り、元の定義を変換したものと、その本体から抽出されたレベル0の定義からなるスーパーコンビネータ定義のリストを返す：

```haskell
> float_sc (name, [], rhs)
>     = [(name, [], rhs’)] ++ concat (map to_scs fds)
>     where
>         (fds, rhs’) = float_e rhs
>         to_scs (level, is_rec, defns) = map make_sc defns
>         make_sc (name, rhs) = (name, [], rhs)
```
プログラムの最上位レベルは暗黙的に相互再帰的であるため、`isRec` フラグは不要である。また、浮動させた各定義には空の引数リストを指定しなければならない。これは今やスーパーコンビネータ定義として扱われるためである。
