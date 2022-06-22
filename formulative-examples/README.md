# formulative-examples

## List of Executables

- Continuous Dynamical Systems
  - Unconstrained system
    - [Harmonic Oscillator](equations/harmonic-oscillator/)
    - [Van der Pol Oscillator](equations/van-der-pol-oscillator/)
    - [Lorenz Equation](equations/lorenz/)
  - Constrained system
    - [Particle on Paraboloid](equations/particle-on-paraboloid/)
- Discrete Dynamical Systems
  - [Hénon Map](equations/henon-map/)
  - [Logistic Map](equations/logistic-map/)
  - [Gumowski-Mira Map](equations/gumowski-mira-map/)

## Build and Execution

- Get a list of the available binaries:

  ```
  cabal list-bin all:exes
  ```

- Build single binary:

  ```
  cabal build <exec_name>
  ```

  To reduce binary size, use `--enable-executable-dynamic` (build may fail if shared libraries has been changed):

  ```
  cabal build <exec_name> --enable-executable-dynamic
  ```

- Build all binary:

  ```
  cabal build all
  ```

- Execute:

  ```
  cabal exec <exec_name>
  ```

  To use command line arguments, put `--` after `exec`:

  ```
  cabal exec -- <exec_name> <args>
  ```

For data visualizations, see [Queries and Visualizations](visualization-scripts/README.md).