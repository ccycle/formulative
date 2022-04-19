# formulative-examples

## Executables

- Unconstrained system
  - [harmonic-oscillator](equations/harmonic-oscillator/app/Main.hs)
  - [van-der-Pol-oscillator](equations/van-der-Pol-oscillator/app/Main.hs)
- Constrained system
  - [particle-on-paraboloid](equations/particle-on-paraboloid/app/Main.hs)

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
