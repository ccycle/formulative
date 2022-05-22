# Lorenz equation

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

   Execute for multiple setting files:

   ```sh
   find ./settingFiles -name "*.dhall" | xargs -I {} -P 4 cabal exec -- lorenz -s {}
   ```

   Recalculate dependent variables from exported data for independent variable:

   ```sh
   find ./settingFiles -name "*.dhall" | xargs -I {} cabal exec -- lorenz --recalculation Continue -s {}
   ```

   Multiprocessing (3 process):

   ```sh
   find ./settingFiles -name "*.dhall" | xargs -P 3 -I {} cabal exec -- lorenz --recalculation Continue -s {}
   ```

## Visualization

### Query

Create database:

```sh
python ../../visualization-scripts/create_database.py
```

View and query database (the results are exported to `output/_query_result.csv`):

```sh
python ../../visualization-scripts/view_database.py
```

Show header:

```sh
python ../../visualization-scripts/view_database.py -H equation_rho
```

Sort:

```sh
python ../../visualization-scripts/view_database.py -H equation_rho -S equation_rho
```

Query:

```sh
python ../../visualization-scripts/view_database.py -H equation_rho -S equation_rho -q "equation_rho <= 24"
```

Append file name for displaying:

```sh
python ../../visualization-scripts/view_database.py -H equation_rho -S equation_rho -q "equation_rho <= 24" -f variable.png
```

- example 1: "equation_rho >= 25"

  ```sh
  python ../../visualization-scripts/view_database.py -H equation_rho -q "equation_rho >= 25"
  ```

- example 2: extract specific directory

  ```sh
  python ../../visualization-scripts/view_database.py -q "export_outputDirectory == \"output/9352458ed15815db770b0d6cece7e30dff1f7b7b\""
  ```

The following visualization scripts are executed on all directories contained in `output/_query_result.csv` .

### Plot

Plot x-y-z space:

```sh
python plot3d_lorenz.py --data variable.csv -o variable.png
```

Plot animation (interval=20, framerate=10):

```sh
python plot3d_animation_lorenz.py --data variable.csv -o variable.mp4 -i 20 -f 10
```

## Examples

Initial condition: $(x_0,y_0,z_0)=(1,0,0)$

$\sigma = 10,\beta=8/3,\rho=15$:

<video src="media/variable_rho_15.mp4" controls="controls" width="70%">
</video>

$\sigma = 10,\beta=8/3,\rho=24$:

<video src="media/variable_rho_24.mp4" controls="controls" width="70%">
</video>

$\sigma = 10,\beta=8/3,\rho=25$:

<video src="media/variable_rho_25.mp4" controls="controls" width="70%">
</video>

$\sigma = 10,\beta=8/3,\rho=28$:

<video src="media/variable_rho_28.mp4" controls="controls" width="70%">
</video>

## References:

- https://en.wikipedia.org/wiki/Lorenz_system
