# Particle on Paraboloid

Equation:

$$
\begin{align*}
\frac{\mathrm{d}\mathbf{x}}{dt} & =\mathbf{v}\\
\frac{\mathrm{d}\mathbf{\mathbf{v}}}{dt} & =-gz\mathbf{e}_{z}\\
f\left(\mathbf{x}\right) & =\frac{x^{2}}{a^{2}}+\frac{y^{2}}{b^{2}}-2z=0
\end{align*}
$$

where $\mathbf{x}=\left(x,y,z\right)$.

Scheme:

$$
\begin{align*}
\frac{\mathbf{x}^{(i+1)}-x^{(i)}}{\Delta t} & -\frac{\mathbf{v}^{(i+1)}+\mathbf{v}^{(i)}}{2}=\mathbf{0}\\
\frac{\mathbf{v}^{(i+1)}+\mathbf{v}^{(i)}}{\Delta t} & +g\frac{z^{(i+1)}+z^{(i)}}{2}\mathbf{e}_{z}+\lambda\left(\frac{2x}{a^{2}}\mathbf{e}_{x}+\frac{2y}{b^{2}}\mathbf{e}_{y}-2\mathbf{e}_{z}\right)=\mathbf{0}
\end{align*}
$$

where $\lambda$ is Lagrangian multiplier.

## Execution

Build:

```sh
cabal build particle-on-paraboloid
```

Execute:

- (case 1) Single setting file

  ```sh
  cabal exec -- particle-on-paraboloid -s setting.dhall
  ```

- (case 2) Multiple setting files

  Generate multiple setting files:

  ```sh
  cabal repl particle-on-paraboloid
  ```

  in REPL:

  ```sh
  :source equations/particle-on-paraboloid/writeSettingFiles.ghci
  ```

  quit REPL:

  ```sh
  :q
  ```

  _NOTE_: `:source` is a command defined in `formulative-examples/.ghci` . To use this command outside of `formulative-examples`, add `:def source readFile` in your `.ghci` file.

  Execute for multiple setting files:

  - Single process:

    ```sh
    find ./settingFiles -name "*.dhall" | xargs -I {} cabal exec -- particle-on-paraboloid -s {}
    ```

    Using option `--recalculation` (Recalculate dependent variables from exported variable data if the directory exists):

    ```sh
    find ./settingFiles -name "*.dhall" | xargs -I {} cabal exec -- particle-on-paraboloid --recalculation Continue -s {}
    ```

  - Multi process (Run 3 processes):

    ```sh
    find ./settingFiles -name "*.dhall" | xargs -P 3 -I {} cabal exec -- particle-on-paraboloid --recalculation Continue -s {}
    ```

## Visualization

Make database:

```sh
python ../../visualization-scripts/make_database.py
```

View and query database (the result is exported in `output/_query_result.csv`):

- example 1: "equation_dampingRatio <= 1"

  ```sh
  python ../../visualization-scripts/view_database.py -H equation_a equation_b equation_xInit equation_vxInit equation_vyInit -q "equation_a==1 & equation_b == 1"
  ```

- example 2: extract specific directory

  ```sh
  python ../../visualization-scripts/view_database.py -q "export_outputDirectory == \"output/eeca6053077485a19e88dbeb2424390f1c6b37b7\""
  ```

Visualization command is executed on all directories contained in `_query_result.csv` .

Plot orbit:

```sh
python plot3d_particle-on-paraboloid.py --data position.csv -o position.png
```

Plot animation (interval:5):

```sh
python plot3d_animation_particle-on-paraboloid.py --data position.csv -o position.mp4 -i 5
```

Plot all global quantities:

```sh
python ../../visualization-scripts/plot_global_quantity.py --parameter time.csv --data dependentVariable/_global.csv
```

Global quantities for selected labels (in this case `kineticEnergy`,`potentialEnergy`,`hamiltonian`):

```sh
python ../../visualization-scripts/plot_global_quantity.py --parameter time.csv --data dependentVariable/_global.csv -H kineticEnergy  potentialEnergy hamiltonian
```

## Reference

- A. Gray, A. Jones, and R. Rimmer, “Motion under gravity on a paraboloid,” Journal of Differential Equations, vol. 45, no. 2, pp. 168–181, Aug. 1982, doi: 10.1016/0022-0396(82)90063-8.
