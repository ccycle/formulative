# Harmonic Oscillator

equation:

$$
\global\long\def\d{\mathrm{d}}%
\global\long\def\D#1#2{\frac{\d#1}{\d#2}}%
\global\long\def\vari{x}%
\global\long\def\varii{p}%
\global\long\def\parami{t}%
\global\long\def\mass{m}%
\global\long\def\viscousDampingCoefficient{\gamma}%
\global\long\def\springConstant{k}%
\begin{align*}
\D x{\parami} & =\frac{\varii}{\mass}\\
\D{\varii}{\parami} & =-\frac{\viscousDampingCoefficient}{\mass}\varii-\springConstant\vari
\end{align*}
$$

scheme:

$$
\global\long\def\vari{x}%
\global\long\def\varii{p}%
\global\long\def\indexI{i}%
\begin{align*}
\frac{x_{\indexI+1}-x_{\indexI}}{\Delta t} & -\frac{p_{\indexI+1}+p_{\indexI}}{m}=0\\
\frac{p_{\indexI+1}-p_{\indexI}}{\Delta t} & +\frac{\gamma}{m}\frac{p_{\indexI+1}+p_{\indexI}}{2}+k\frac{\vari_{\indexI+1}+\vari_{\indexI}}{2}=0
\end{align*}
$$

## scripts

execute:

```sh
cabal exec -- harmonic-oscillator -s setting.dhall
```

execute multiple setting files:

```sh
find ./settingFiles -name "*.dhall" -exec cabal exec -- harmonic-oscillator -s {} \;
```

### visualize

Time evolution:

```sh
python ../../visualization-scripts/plot_time_evolution.py --outputDirRegExp "output/*" --x position.csv --y momentum.csv
```

Phase space:

```sh
python ../../visualization-scripts/plot2d.py --outputDirRegExp "output/*" --x position.csv --y momentum.csv
```

Global quantities:

```sh
python ../../visualization-scripts/plot_global_quantity.py --outputDirRegExp "output/*" --parameter time.csv --data dependentVariableGlobal.csv
```

Global quantities for selected labels (`hamiltonian`, `dHdt`, `power`):

```sh
python ../../visualization-scripts/plot_global_quantity.py --outputDirRegExp "output/*" --parameter time.csv --data dependentVariableGlobal.csv --labels hamiltonian dHdt power
```
