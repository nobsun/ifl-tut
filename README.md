# 『関数型言語を実装する：チュートリアル』を読む

[Simon L. Payton Jones, David R. Lester *Implementing Functional Languages: a tutorial*, 1991](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/)

> 本書は、遅延グラフ簡約を用いた非正格な関数型言語の実装を理解するための実践的なアプローチを提供します。
この本は、読者が自明ではないコンパイラを開発、修正、実験することで、関数型言語の実装を「生き生きと」させるための実践的な実験材料を提供することを目的としています。

この本にある実装は、元々 Miranda[^1]で書かれていましたが、現在、公開されているものは、Haskellで示されています[^2]。
30年も前に出版されたものなので、説明されている実装は、最新の言語実装技術によるものではなく、現在では素朴にみえるものです。
しかし、基本的なアイデアは興味深く、実装としてもまとまっているので、入門をおえたプログラマ向けのHaskellプログラミングの教材として楽しいものになっています。
おまけに遅延評価を行う関数型言語系の実装が学べます。
他にも、現在では当たり前になり、標準的なライブラリとして提供されている（それゆえに、利用はするが、どのようなアイデアでデザインされているのかあまり知らない）プリティプリンタやパーザコンビネータのアイデアを楽しめます。

[^1]: Miranda は Research Software Ltd. の登録商標です。

[^2]: 地の文の説明は、Miranda を前提としています。



## この本の構成

1. Core言語の抽象構文木、プリティプリンタ、パーザ
2. テンプレート具体化を利用した（仮想）マシン
3. G-machine（グラフ簡約マシン）
4. TIM（Three Instruction Machine）
5. 並列G-machine
6. λ リフティング

## コードについて

この main ブランチのコードは ghc-9.2.1 で導入された言語拡張 'OverloadedDotSyntax' を使用しています。

## 起動

プロジェクトルートにおいて、コマンド `cabal run ti5b prog/prog17.ifl` で雛形具体化機械 `ti5b` にサンプルプログラム `prog17.ifl` ロードして起動すると、

```
% cabal run ti5b prog/prog17.ifl
   0) Heap [  40: NAP #21 #1
              39: NPrim print
              ...
              ヒープのダンプ
              ...
      Stack [  40: NAp   21    1 (NSupercomb main) ]
      Depth 1
      Dump []
      Output []
      no description


|
```
のように初期状態（の一部）が表示された状態で停止する。

- Enterキー（空文字列の入力）で、次の状態に遷移
- Cキー、Enterキーの順で押下（文字列`C`の入力）で、最後の状態まで遷移
- 数字入力で、指定したかずぶんだけ状態が遷移

