# Lorenz equation

<!-- TODO: equation -->

## Execution

Build:

```sh
cabal build lorenz
```

Execute:

1. single setting file

   ```sh
   cabal exec -- lorenz -s setting.dhall
   ```

1. multiple setting files

   generate multiple setting files:

   ```sh
   cabal repl lorenz
   ```

   in REPL:

   ```sh
   :source equations/lorenz/writeSettingFiles.ghci
   ```

   quit REPL:

   ```sh
   :q
   ```

   _NOTE_: `:source` is a command defined in `formulative-examples/.ghci` . To use this command outside of `formulative-examples`, add `:def source readFile` in your `.ghci` file.

   execute for multiple setting files:

   ```sh
   find ./settingFiles -name "*.dhall" | xargs -I {} -P 4 cabal exec -- lorenz -s {}
   ```

   Recalculate dependent variables from exported independent variable data:

   ```sh
   find ./settingFiles -name "*.dhall" | xargs -I {} cabal exec -- lorenz --recalculation Continue -s {}
   ```

   Multiprocessing (3 process):

   ```sh
   find ./settingFiles -name "*.dhall" | xargs -P 3 -I {} cabal exec -- lorenz --recalculation Continue -s {}
   ```

## Visualization

Make database:

```sh
python ../../visualization-scripts/make_database.py
```

View and query database (the result is exported in `output/_query_result.csv`):

<!-- TODO: query parameter -->

- example 1: "equation_dampingRatio <= 1"

  ```sh
  python ../../visualization-scripts/view_database.py -H equation_rho
  ```

- example 2: extract specific directory

  ```sh
  python ../../visualization-scripts/view_database.py -q "export_outputDirectory == \"output/eeca6053077485a19e88dbeb2424390f1c6b37b7\""
  ```

Visualization command is executed on all directories contained in `_query_result.csv` .

Time evolution:

```sh
python ../../visualization-scripts/plot_time_evolution.py -t time.csv -x position.csv
```

Phase space:

```sh
python ../../visualization-scripts/plot2d.py --x position.csv --y momentum.csv
```

All global quantities:

```sh
python ../../visualization-scripts/plot_global_quantity.py --parameter time.csv --data dependentVariable/_global.csv
```

Global quantities for selected labels (in this case `hamiltonian`, `dHdt`, `power`):

```sh
python ../../visualization-scripts/plot_global_quantity.py --parameter time.csv --data dependentVariable/_global.csv --header hamiltonian dHdt power
```

References:

- https://www2.physics.ox.ac.uk/sites/default/files/profiles/read/lect6-43147.pdf
- https://en.wikipedia.org/wiki/Lorenz_system
