# formulative-examples

## List of Executables

- Unconstrained system
  - [harmonic-oscillator](equations/harmonic-oscillator/)
  - [van-der-Pol-oscillator](equations/van-der-Pol-oscillator/)
- Constrained system
  - [particle-on-paraboloid](equations/particle-on-paraboloid/)

## Build and Execute

- Get a list of the available binaries:

  ```
  cabal list-bin all:exes
  ```

- Build single binary:

  ```
  cabal build <exec_name>
  ```

  - To reduce the binary size, use cabal options `--enable-executable-dynamic` :

    ```
    cabal build <exec_name> --enable-executable-dynamic
    ```

- Build all binary:

  ```
  cabal build all:exes --enable-executable-dynamic
  ```

- execute:

  ```
  cabal exec -- <exec_name>
  ```

## Data organization

- Make database:

  ```sh
  python ../../visualization-scripts/make_database.py
  ```

- View database using query:

  ```sh
  python ../../visualization-scripts/make_database.py -q "equation_dampingRatio >= 0.5" -H equation_dampingRatio -S equation_dampingRatio --appendFileName "position.svg"
  ```
