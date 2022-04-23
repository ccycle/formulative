# Particle on Paraboloid

equation:

$$
\global\long\def\d{\mathrm{d}}%

\global\long\def\v#1{\mathbf{#1}}%

\global\long\def\cd{\cdot}%

\global\long\def\D#1#2{\frac{\d#1}{\d#2}}%
\global\long\def\vari{\v x}%
\global\long\def\varii{\mathbf{p}}%

\global\long\def\xVec{\mathbf{e}_{x}}%

\global\long\def\yVec{\mathbf{e}_{y}}%

\global\long\def\zVec{\mathbf{e}_{z}}%

\global\long\def\xComponentI{x}%

\global\long\def\xComponentII{y}%

\global\long\def\xComponentIII{z}%

\global\long\def\LagrangianMultiplier{\lambda}%

\global\long\def\constantI{a}%

\global\long\def\constantII{b}%

\global\long\def\equalityConstraint{f}%
\global\long\def\parami{t}%
\global\long\def\mass{m}%
\global\long\def\gravitationalConstant{g}%
\global\long\def\indexI{i}%
\begin{align*}
\D{\vari}{\parami} & =\frac{\varii}{\mass}\\
\D{\varii}{\parami} & =-\gravitationalConstant\xComponentIII\zVec\\
\equalityConstraint\left(\vari\right) & =\frac{\xComponentI^{2}}{\constantI^{2}}+\frac{\xComponentII^{2}}{\constantII^{2}}-\xComponentIII
\end{align*}
$$

scheme:

$$
\global\long\def\d{\mathrm{d}}%

\global\long\def\v#1{\mathbf{#1}}%

\global\long\def\cd{\cdot}%

\global\long\def\D#1#2{\frac{\d#1}{\d#2}}%
\global\long\def\vari{\v x}%
\global\long\def\varii{\mathbf{p}}%

\global\long\def\xVec{\mathbf{e}_{x}}%

\global\long\def\yVec{\mathbf{e}_{y}}%

\global\long\def\zVec{\mathbf{e}_{z}}%

\global\long\def\xComponentI{x}%

\global\long\def\xComponentII{y}%

\global\long\def\xComponentIII{z}%

\global\long\def\LagrangianMultiplier{\lambda}%

\global\long\def\constantI{a}%

\global\long\def\constantII{b}%

\global\long\def\equalityConstraint{f}%
\global\long\def\parami{t}%
\global\long\def\mass{m}%
\global\long\def\gravitationalConstant{g}%
\global\long\def\indexI{i}%
\begin{align*}
\frac{\vari_{\indexI+1}-\vari_{\indexI}}{\Delta t} & -\frac{\varii_{\indexI+1}+\varii_{\indexI}}{m}=\v 0\\
\frac{\varii_{\indexI+1}-\varii_{\indexI}}{\Delta t} & +\gravitationalConstant\frac{\xComponentIII_{\indexI+1}+\xComponentIII_{\indexI}}{2}\zVec+\LagrangianMultiplier\left(\frac{2\xComponentI}{\constantI^{2}}\xVec+\frac{2\xComponentII}{\constantII^{2}}\yVec-\zVec\right)=\v 0
\end{align*}
$$

## scripts

### execute

```sh
cabal exec -- harmonic-oscillator -s setting.dhall
```

### visualize

plot phase space:

```sh
python ../../visualization-scripts/plot2d.py --outputDirRegExp "output/*" --x position.csv --y momentum.csv
```

plot all global quantities:

```sh
python ../../visualization-scripts/plot_global_quantity.py --outputDirRegExp "output/*" --parameter time.csv --data dependentVariableGlobal.csv
```

plot global quantities for selected labels:

```sh
python ../../visualization-scripts/plot_global_quantity.py --outputDirRegExp "output/*" --parameter time.csv --data dependentVariableGlobal.csv --labels hamiltonian dHdt power
```
