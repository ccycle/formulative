# formulative-examples

## List of Executables

- Unconstrained system
  - [harmonic-oscillator](equations/harmonic-oscillator/)
  - [van-der-Pol-oscillator](equations/van-der-Pol-oscillator/)
- Constrained system
  - [particle-on-paraboloid](equations/particle-on-paraboloid/)

## Build and Execute

- get a list of the available binaries:

  ```
  cabal list-bin .
  ```

- build single binary:

  ```
  cabal build <exec_name>
  ```

  - if you want to reduce the binary size, use cabal options `--enable-executable-dynamic`.

    ```
    cabal build <exec_name> --enable-executable-dynamic
    ```

- build all binary:

  ```
  cabal build --enable-executable-dynamic
  ```

- execute:

  ```
  cabal exec -- <exec_name>
  ```

## Data organization

```sh
python ../../visualization-scripts/query.py -q "equation_dampingRatio >= 0.5" -H equation_dampingRatio --fileName "position.svg" -S equation_dampingRatio
```
