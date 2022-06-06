# Particle on Paraboloid

Equation:

$$
\begin{align*}
 & \frac{\mathrm{d}\mathbf{x}}{\mathrm{d}t}=\mathbf{v}\\
 & \frac{\mathrm{d}\mathbf{\mathbf{v}}}{\mathrm{d}t}=-gz\mathbf{e}_{z}\\
 & \frac{x^{2}}{a^{2}}+\frac{y^{2}}{b^{2}}-2z=0
\end{align*}
$$

where $\mathbf{x}=\left(x,y,z\right)$.

Scheme:

$$
\begin{align*}
 & \frac{\mathbf{x}^{(i+1)}-x^{(i)}}{\Delta t}-\frac{\mathbf{v}^{(i+1)}+\mathbf{v}^{(i)}}{2}=\mathbf{0}\\
 & \frac{\mathbf{v}^{(i+1)}-\mathbf{v}^{(i)}}{\Delta t}+g\frac{z^{(i+1)}+z^{(i)}}{2}\mathbf{e}_{z}\\
 & \quad+2\lambda\left[\left(\frac{x^{(i+1)}+x^{(i)}}{a^{2}}\right)\mathbf{e}_{x}+\left(\frac{y^{(i+1)}+y^{(i)}}{b^{2}}\right)\mathbf{e}_{y}-\mathbf{e}_{z}\right]=\mathbf{0}
\end{align*}
$$

where $\lambda$ is Lagrange multiplier.

## Execution

Build:

```sh
cabal build particle-on-paraboloid
```

Execute:

1. Single setting file

  ```sh
  cabal exec -- particle-on-paraboloid -s setting.dhall
  ```

1. Multiple setting files

  Generate multiple setting files:

  ```sh
  cabal repl particle-on-paraboloid
  ```

  in REPL:

  ```sh
  :source equations/particle-on-paraboloid/writeSettingFiles.ghci
  ```

  Quit REPL:

  ```sh
  :q
  ```

  _NOTE_: `:source` is a command defined in `formulative-examples/.ghci` . To use this command outside of `formulative-examples`, add `:def source readFile` in your `.ghci` file.

  Execute for multiple setting files (Multiprocessing):

  ```sh
  find ./settingFiles -name "*.dhall" | xargs -P 4 -I {} cabal exec -- particle-on-paraboloid -s {}
  ```

## Visualization

Create Database:

```sh
python ../../visualization-scripts/create_database.py
```

View and query database (for more details, see [Queries and Visualizations](../../visualization-scripts/README.md)):

```sh
python ../../visualization-scripts/view_database.py -H equation_a equation_b equation_xInit equation_vxInit equation_vyInit
```

Visualization command is executed on all directories contained in `_query_result.csv` .

Plot orbit:

```sh
python plot3d_particle-on-paraboloid.py --data position -o position.png
```

Plot animation (interval:5):

```sh
python plot3d_animation_particle-on-paraboloid.py --data position -o position.mp4 -i 5
```

Plot all global quantities:

```sh
python ../../visualization-scripts/plot_global_quantity.py --parameter time
```

Plot global quantities for selected labels (in this case `kineticEnergy`,`potentialEnergy`,`hamiltonian`):

```sh
python ../../visualization-scripts/plot_global_quantity.py -H kineticEnergy potentialEnergy hamiltonian
```

## Examples

(g,a,b,xInit,yInit,vxInit)=(1,1,1,1,0,0)

vyInit=0.5: [movie link](media/position1.mp4)

<video src="media/position1.mp4" controls="controls" width="70%">
</video>

vyInit=1.0: [movie link](media/position2.mp4)

<video src="media/position2.mp4" controls="controls" width="70%">
</video>

vyInit=1.5: [movie link](media/position3.mp4)

<video src="media/position3.mp4" controls="controls" width="70%">
</video>

## Reference

- A. Gray, A. Jones, and R. Rimmer, “Motion under gravity on a paraboloid,” Journal of Differential Equations, vol. 45, no. 2, pp. 168–181, Aug. 1982, doi: 10.1016/0022-0396(82)90063-8.
