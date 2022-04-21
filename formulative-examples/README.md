# formulative-examples

## Executables

- Unconstrained system
  - [harmonic-oscillator](equations/harmonic-oscillator/)
  - [van-der-Pol-oscillator](equations/van-der-Pol-oscillator/)
- Constrained system
  - [particle-on-paraboloid](equations/particle-on-paraboloid/)

## Build and Execute

- get a list of the available binaries:

  ```
  stack ide targets
  ```
  or
  ```
  cabal list-bin .
  ```

- build single binary:

  ```
  cabal build <exec_name>
  ```

- build all binary:

  ```
  cabal build
  ```

- execute:

  ```
  cabal exec -- <exec_name>
  ```
