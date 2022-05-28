# Hénon Map

equation:

$$
\begin{align*}
x_{n+1} & =1-ax_{n}{}^{2}+y_{n}\\
y_{n+1} & =bx_{n}
\end{align*}
$$

## Execution

Build:

```sh
cabal build henon-map
```

Execute:

1. single setting file

   ```sh
   cabal exec -- henon-map -s setting.dhall
   ```

1. multiple setting files

   generate multiple setting files:

   ```sh
   cabal repl henon-map
   ```

   in REPL:

   ```sh
   :source equations/henon-map/writeSettingFiles.ghci
   ```

   quit REPL:

   ```sh
   :q
   ```

   _NOTE_: `:source` is a command defined in `formulative-examples/.ghci` . To use this command outside of `formulative-examples`, add `:def source readFile` in your `.ghci` file.

   execute for multiple setting files:

   ```sh
   find ./settingFiles -name "*.dhall" | xargs -I {} -P 4 cabal exec -- henon-map -s {}
   ```

   Recalculate dependent variables from exported independent variable data:

   ```sh
   find ./settingFiles -name "*.dhall" | xargs -I {} cabal exec -- henon-map --recalculation Continue -s {}
   ```

   Multiprocessing (3 process):

   ```sh
   find ./settingFiles -name "*.dhall" | xargs -P 4 -I {} cabal exec -- henon-map --recalculation Continue -s {}
   ```

## Visualization

Create Database:

```sh
python ../../visualization-scripts/create_database.py
```

View and query database (the result is exported in `output/_query_result.csv`):

```sh
python ../../visualization-scripts/view_database.py -H equation_a equation_b -S equation_a equation_b
```

Visualization command is executed on all directories contained in `_query_result.csv` .

Phase space (scatter plot):

```sh
python ../../visualization-scripts/plot2d.py --data x y -S -o phase-space.png
```

View list of image files:

```sh
python ../../visualization-scripts/view_database.py -H equation_a equation_b -S equation_a equation_b -f phase-space.png
```

## Examples

$a = 1.4, b = 0.3, x0 = 0.0, y0 = 0.0$

![](media/phase-space1.png)

$a = 0.2, b = 0.9991, x0 = 0.0, y0 = 0.0$

![](media/phase-space2.png)

$a = 0.2, b = -0.9999, x0 = 0.0, y0 = 0.0$

![](media/phase-space3.png)


## References

- https://en.wikipedia.org/wiki/H%C3%A9non_map
- https://mathworld.wolfram.com/HenonMap.html
