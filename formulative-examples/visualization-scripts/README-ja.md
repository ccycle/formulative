# クエリと可視化

[English](README.md)/Japanese

`formulative`で作成されたbinaryは、デフォルトでは`output/`以下に対し設定ファイルに応じて個別に親ディレクトリを生成する。

`formulative-examples/visualization-scripts/`以下にあるスクリプト群は、設定値から出力ディレクトリを検索したり、検索結果を可視化することができる。以降ではスクリプトの解説と使用例を載せる。

## データベースのクエリ

`create_database.py`は、特定のディレクトリ(デフォルトでは`output/`)以下にある`setting.dhall`の値を読み取って`_database.csv`に書き込むスクリプトである。

`view_database.py`は、コマンドライン引数を使って`_database.csv`の中身を一部表示するためのスクリプトである。抽出された結果の一覧は`_query_result.csv`に保存される。

## 使用例

以下に挙げるスクリプトの例はすべて[Harmonic Oscillator](../equations/harmonic-oscillator/)のディレクトリ中で実行したものである(数値計算は一通り済んでいる状態を想定する)。

例:

```sh
$ python ../../visualization-scripts/view_database.py -H equation_x0 equation_p0 equation_gamma dynamics_stepSize
```

実行結果(ハッシュ値はパッケージのバージョンによって異なる可能性がある):

```
Header prefixes are removed for displaying on terminal by default. To show prefixes, use "--showPrefix".
Result:
                                outputDirectory  x0  p0  gamma  stepSize
output/8522bf29ad128ae3965d9405afa0cde1eb1e2bce 0.0 1.0    0.0      0.01
output/bf24f7c0d25b882be92b1efa06b1b1e38e87fec3 0.0 1.0    1.0      0.01
output/8603526498e22c848441a590625372ad62a86248 0.0 1.0    2.0      0.01
output/c23670ed4d78ca211324d3b0194bc86784fe092c 0.0 1.0    3.0      0.01
output/1944314149639206e60f063d7e8a15346903fdba 1.0 0.0    0.0      0.01
output/6ed95255177ec6764d325f3b0026c4dbefa37461 1.0 0.0    1.0      0.01
output/41a418deb71a2dc522ae9f2c43c00becdea26f11 1.0 0.0    2.0      0.01
output/6bd77382de415f5657aafac22b557ac65095f69a 1.0 0.0    3.0      0.01
output/44bc371bf63db266e538fd36bf2523ffb56d56d6 1.0 1.0    0.0      0.01
output/b6c234da0a5de92e2faf5b3c2a7b7fd8bbed76b1 1.0 1.0    1.0      0.01
output/20b610a502b6a07b6ee18469c8bc2e3fe2a8a8d0 1.0 1.0    2.0      0.01
output/1708ed594716f2a51b2438ec4d1909a46d162445 1.0 1.0    3.0      0.01
Exporting output/_query_result.csv ..
````

`-q` オプションを使うことで、結果を検索することができる(構文はpandasの[query](https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.query.html)を参照)。

例:

```
python ../../visualization-scripts/view_database.py -H equation_x0 equation_p0 equation_gamma dynamics_stepSize -q "equation_x0 == 1 & equation_p0 == 0 & equation_gamma <= 1"
```

実行結果:

```sh
Header prefixes are removed for displaying on terminal by default. To show prefixes, use "--showPrefix".
Result:
                                outputDirectory  x0  p0  gamma  stepSize
output/1944314149639206e60f063d7e8a15346903fdba 1.0 0.0    0.0      0.01
output/6ed95255177ec6764d325f3b0026c4dbefa37461 1.0 0.0    1.0      0.01
Exporting output/_query_result.csv ..
```

## 可視化

`plot2d.py`などの可視化スクリプトはデフォルトでは`_query_result.csv`中にあるすべての`outputDirectory`に対し実行される。

前述した`-q`オプションを使うことにより、結果を部分的に可視化することができる。

例:

```sh
python ../../visualization-scripts/view_database.py -H equation_x0 equation_p0 equation_gamma dynamics_stepSize -q "equation_x0 == 1 & equation_p0 == 0 & equation_gamma <= 1"
python ../../visualization-scripts/plot_time_evolution.py -t time --data position -o t-x.png
```

実行結果:

```sh
Exporting output/1944314149639206e60f063d7e8a15346903fdba/t-x.png ..
Exporting output/6ed95255177ec6764d325f3b0026c4dbefa37461/t-x.png ..
```

## 画像のクエリ

`-f`オプションを使うと、クエリの結果に対しファイルパスを付け足すことができる。

例:

```
python ../../visualization-scripts/view_database.py -H equation_x0 equation_p0 equation_gamma dynamics_stepSize -q "equation_x0 == 1 & equation_p0 == 0 & equation_gamma <= 1" -f t-x.png
```

実行結果:

```
Header prefixes are removed for displaying on terminal by default. To show prefixes, use "--showPrefix".
Result:
                                        outputDirectory  x0  p0  gamma  stepSize
output/1944314149639206e60f063d7e8a15346903fdba/t-x.png 1.0 0.0    0.0      0.01
output/6ed95255177ec6764d325f3b0026c4dbefa37461/t-x.png 1.0 0.0    1.0      0.01
Exporting output/_query_result.csv ..
```

表示したファイルパスは、コピーアンドペーストなどでターミナルからファイルを直接開いたりするのに役立つ。
