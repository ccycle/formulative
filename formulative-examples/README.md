# formulative-examples

## List of Executables

- Continuous Dynamical Systems
  - Unconstrained system
    - [Harmonic Oscillator](equations/harmonic-oscillator/)
    - [Van der Pol Oscillator](equations/van-der-pol-oscillator/)
  - Constrained system
    - [Particle on Paraboloid](equations/particle-on-paraboloid/)
- Discrete Dynamical Systems
  - [Hénon Map](equations/henon-map/)
  - [Logistic Map](equations/logistic-map/)
  - [Gumowski Mira Map](equations/gumowski-mira-map/)
## Build and Execute

- Get a list of the available binaries:

  ```
  cabal list-bin all:exes
  ```

- Build single binary:

  ```
  cabal build <exec_name>
  ```

- Build all binary:

  ```sh
  cabal build all
  ```

- execute:

  ```sh
  cabal exec <exec_name>
  ```

  To use commandline arguments, put `--` after `exec`:

  ```sh
  cabal exec -- <exec_name> <args>
  ```
