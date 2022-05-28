# Van der Pol Equation

Equation:

$$
\begin{align*}
\frac{\mathrm{d}x}{\mathrm{d}t} & =\mu\left(x-\frac{1}{3}x^{3}-y\right)\\
\frac{\mathrm{d}y}{\mathrm{d}t} & =\frac{1}{\mu}x
\end{align*}
$$

Scheme:

$$
\begin{align*}
 & \frac{x^{(i+1)}-x^{(i)}}{\Delta t}-\mu\left(\frac{x^{(i+1)}+x^{(i)}}{2}-\frac{1}{3}f\left(x^{(i+1)},x^{(i)}\right)-\frac{y^{(i+1)}+y^{(i)}}{2}\right)=0\\
 & \frac{y^{(i+1)}-y^{(i)}}{\Delta t}-\frac{1}{\mu}\left(\frac{x^{(i+1)}+x^{(i)}}{2}\right)=0
\end{align*}
$$

where $f\left(x^{(i+1)},x^{(i)}\right)=\frac{1}{4}\left(\left(x^{(i+1)}\right)^{3}+\left(x^{(i+1)}\right)^{2}x^{(i)}+x^{(i+1)}\left(x^{(i+1)}\right)^{2}+\left(x^{(i)}\right)^{3}\right)$

## Execution

Build:

```sh
cabal build lorenz
```

Execute:

1. For single setting file

   ```sh
   cabal exec -- lorenz -s setting.dhall
   ```

1. For multiple setting files

   Generate multiple setting files:

   ```sh
   cabal repl lorenz
   ```

   in REPL:

   ```sh
   :source equations/lorenz/writeSettingFiles.ghci
   ```

   quit REPL:

   ```sh
   :q
   ```

   _NOTE_: `:source` is a command defined in `formulative-examples/.ghci` . To use this command outside of `formulative-examples`, add `:def source readFile` in your `.ghci` file.

   Execute for multiple setting files (Multiprocessing):

   ```sh
   find ./settingFiles -name "*.dhall" | xargs -I {} -P 4 cabal exec -- lorenz -s {}
   ```

## Visualization

### Query

Create database:

```sh
python ../../visualization-scripts/create_database.py
```

View and query database (the results are exported to `output/_query_result.csv`):

```sh
python ../../visualization-scripts/view_database.py -H equation_rho -S equation_rho
```

The following visualization scripts are executed on all directories contained in `output/_query_result.csv` .

### Plot

Plot x-y-z space:

```sh
python plot3d.py --data variable -o variable.png
```

Plot x-xdot space:

```sh
python ../../visualization-scripts/plot2d.py  --data x dependentVariable/xdot -o x-xdot.png
```

Plot animation (interval=20, framerate=10):

```sh
python ../../visualization-scripts/plot2d_animation.py --data x y -o variable.mp4 -i 20 -f 10
```

## References:

- https://en.wikipedia.org/wiki/Van_der_Pol_oscillator
