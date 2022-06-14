# Lorenz Equation

Equation:

$$
\begin{align*}
\frac{\mathrm{d}x}{\mathrm{d}t} & =\sigma\left(y-x\right)\\
\frac{\mathrm{d}y}{\mathrm{d}t} & =x\left(\rho-z\right)-y\\
\frac{\mathrm{d}z}{\mathrm{d}t} & =xy-\beta z
\end{align*}
$$

Scheme:

$$
\begin{align*}
 & \frac{x^{(i+1)}-x^{(i)}}{\Delta t}-\sigma\left(\frac{y^{(i+1)}+y^{(i)}}{2}-\frac{x^{(i+1)}+x^{(i)}}{2}\right)=0\\
 & \frac{y^{(i+1)}-y^{(i)}}{\Delta t}-\left(\frac{x^{(i+1)}+x^{(i)}}{2}\right)\left(\rho-\frac{z^{(i+1)}+z^{(i)}}{2}\right)-\frac{y^{(i+1)}+y^{(i)}}{2}=0\\
 & \frac{z^{(i+1)}-z^{(i)}}{\Delta t}-\sigma\left(\frac{x^{(i+1)}+x^{(i)}}{2}\right)\left(\frac{y^{(i+1)}+y^{(i)}}{2}\right)-\beta\left(\frac{z^{(i+1)}+z^{(i)}}{2}\right)=0
\end{align*}
$$

## Execution

Build:

```
cabal build lorenz
```

Execute:

1. Single setting file

   ```
   cabal exec -- lorenz -s setting.dhall
   ```

1. Multiple setting files

   Generate multiple setting files:

   ```
   cabal repl lorenz
   ```

   in REPL:

   ```
   :source equations/lorenz/writeSettingFiles.ghci
   ```

   Quit REPL:

   ```
   :q
   ```

   _NOTE_: `:source` is a command defined in `formulative-examples/.ghci` . To use this command outside of `formulative-examples`, add `:def source readFile` in your `.ghci` file.

   Execute for multiple setting files (Multiprocessing):

   ```
   find ./settingFiles -name "*.dhall" | xargs -I {} -P 4 cabal exec -- lorenz -s {}
   ```

## Visualization

### Query

Create database:

```
python ../../visualization-scripts/create_database.py
```

View and query database (the results are exported to `output/_query_result.csv`):

```
python ../../visualization-scripts/view_database.py -H equation_rho -S equation_rho
```

### Plot

The following visualization scripts are executed on all directories contained in `output/_query_result.csv` .

Plot x-y-z space:

```
python plot3d.py --data variable -o variable.png
```

Plot x-z space:

```
python ../../visualization-scripts/plot2d.py --data x z -o x-z.png
```

Plot animation (interval=20, framerate=10):

```
python ../../visualization-scripts/plot3d_animation.py --data x y z -o variable.mp4 -i 20 -f 10
```

## Examples

Initial condition: $(x_0,y_0,z_0)=(1,0,0)$

$\sigma = 10,\beta=8/3$

$\rho=15$: [video link](media/variable_rho_15.mp4)

<video src="media/variable_rho_15.mp4" controls="controls" width="70%">
</video>

$\rho=24$: [video link](media/variable_rho_24.mp4)

<video src="media/variable_rho_24.mp4" controls="controls" width="70%">
</video>

$\rho=25$: [video link](media/variable_rho_25.mp4)

<video src="media/variable_rho_25.mp4" controls="controls" width="70%">
</video>

$\rho=28$: [video link](media/variable_rho_28.mp4)

<video src="media/variable_rho_28.mp4" controls="controls" width="70%">
</video>

## References:

- https://en.wikipedia.org/wiki/Lorenz_system
