# Harmonic Oscillator

Equation:

$$
\begin{align*}
\frac{\mathrm{d}x}{\mathrm{d}t} & =\frac{p}{m}\\
\frac{\mathrm{d}p}{\mathrm{d}t} & =-\frac{\gamma}{m}p-kx
\end{align*}
$$

Scheme:

$$
\begin{align*}
\frac{x_{i+1}-x_{i}}{\Delta t} & -\frac{p_{i+1}+p_{i}}{m}=0\\
\frac{p_{i+1}-p_{i}}{\Delta t} & +\frac{\gamma}{m}\frac{p_{i+1}+p_{i}}{2}+k\frac{x_{i+1}+x_{i}}{2}=0
\end{align*}
$$

## Execution

Build:

```
cabal build harmonic-oscillator
```

Execute:

1. Single setting file

   ```
   cabal exec -- harmonic-oscillator -s setting.dhall
   ```

1. Multiple setting files

   Generate multiple setting files:

   ```
   cabal repl harmonic-oscillator
   ```

   in REPL:

   ```
   :source equations/harmonic-oscillator/writeSettingFiles.ghci
   ```

   Quit REPL:

   ```
   :q
   ```

   _NOTE_: `:source` is a command defined in `formulative-examples/.ghci` . To use this command outside of `formulative-examples`, add `:def source readFile` in your `.ghci` file.

   Execute for multiple setting files:

   ```
   find ./settingFiles -name "*.dhall" | xargs -I {} -P 4 cabal exec -- harmonic-oscillator -s {}
   ```

   Recalculate dependent variables from exported independent variable data:

   ```
   find ./settingFiles -name "*.dhall" | xargs -I {} cabal exec -- harmonic-oscillator --recalculation Continue -s {}
   ```

   Multiprocessing (3 process):

   ```
   find ./settingFiles -name "*.dhall" | xargs -P 4 -I {} cabal exec -- harmonic-oscillator --recalculation Continue -s {}
   ```

## Visualization

Create Database:

```
python ../../visualization-scripts/create_database.py
```

View and query database (for more details, see [Queries and Visualizations](../../visualization-scripts/README.md)):

```
python ../../visualization-scripts/view_database.py -H equation_dampingRatio equation_x0 equation_p0
```

Plot time evolution:

```
python ../../visualization-scripts/plot_time_evolution.py -t time --data position -o t-x.png
```

Plot phase space:

```
python ../../visualization-scripts/plot2d.py --data position momentum
```

Plot all global quantities:

```
python ../../visualization-scripts/plot_global_quantity.py --parameter time
```

Plot global quantities for selected labels (in this case `hamiltonian`, `dHdt`, `power`):

```
python ../../visualization-scripts/plot_global_quantity.py --parameter time --header hamiltonian dHdt power
```

## Examples

$m = 1.0, k = 1.0, x_0 = 1.0, p_0 = 0.0$

damping ratio: $\zeta=\frac{\gamma}{2\sqrt{mk}}$

$\gamma = 0.0$ ($\zeta=0.0$)

![](media/t-x_dampingRatio_0.png)

$\gamma = 1.0$ ($\zeta=0.5$)

![](media/t-x_dampingRatio_0.5.png)

$\gamma = 2.0$ ($\zeta=1.0$)

![](media/t-x_dampingRatio_1.png)

$\gamma = 3.0$ ($\zeta=1.5$)

![](media/t-x_dampingRatio_1.5.png)

## References

- https://en.wikipedia.org/wiki/Harmonic_oscillator
