# Queries and Visualizations

English/[Japanese](README-ja.md)

Binaries created with `formulative` generate individual parent directories under `output/` by default, depending on the configuration file. By using scripts under `formulative-examples/visualization-scripts/`, you can search output directories by configuration values and visualize the search results. The following sections describe usage of these scripts and provide examples of their use.

## Querying Database

The `create_database.py` is a script that reads the values in `setting.dhall` under a specific directory (`output/` by default) and writes them to `_database.csv` after the numerical calculation is completed once.

`view_database.py` is a script that uses command line arguments to display some of the contents of `_database.csv`. The list of extracted results is stored in `_query_result.csv`.

## Examples

All of the following scripts run in the [Harmonic Oscillator](.../equations/harmonic-oscillator/) directory (assuming that all numerical calculations have been done).

Example:

```sh
$ python ../../visualization-scripts/view_database.py -H equation_x0 equation_p0 equation_gamma dynamics_stepSize
```

Result(hash values may vary depending on package version):

```
Header prefixes are removed for displaying on terminal by default. To show prefixes, use "--showPrefix".
Result:
                                outputDirectory  x0  p0  gamma  stepSize
output/8522bf29ad128ae3965d9405afa0cde1eb1e2bce 0.0 1.0    0.0      0.01
output/bf24f7c0d25b882be92b1efa06b1b1e38e87fec3 0.0 1.0    1.0      0.01
output/8603526498e22c848441a590625372ad62a86248 0.0 1.0    2.0      0.01
output/c23670ed4d78ca211324d3b0194bc86784fe092c 0.0 1.0    3.0      0.01
output/1944314149639206e60f063d7e8a15346903fdba 1.0 0.0    0.0      0.01
output/6ed95255177ec6764d325f3b0026c4dbefa37461 1.0 0.0    1.0      0.01
output/41a418deb71a2dc522ae9f2c43c00becdea26f11 1.0 0.0    2.0      0.01
output/6bd77382de415f5657aafac22b557ac65095f69a 1.0 0.0    3.0      0.01
output/44bc371bf63db266e538fd36bf2523ffb56d56d6 1.0 1.0    0.0      0.01
output/b6c234da0a5de92e2faf5b3c2a7b7fd8bbed76b1 1.0 1.0    1.0      0.01
output/20b610a502b6a07b6ee18469c8bc2e3fe2a8a8d0 1.0 1.0    2.0      0.01
output/1708ed594716f2a51b2438ec4d1909a46d162445 1.0 1.0    3.0      0.01
Exporting output/_query_result.csv ..
````

The `-q` option can be used to search for results (see [pandas.DataFrame.query](https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.query.html) for syntax).

Example:

```
python ../../visualization-scripts/view_database.py -H equation_x0 equation_p0 equation_gamma dynamics_stepSize -q "equation_x0 == 1 & equation_p0 == 0 & equation_gamma <= 1"
```

Result:

```console
Header prefixes are removed for displaying on terminal by default. To show prefixes, use "--showPrefix".
Result:
                                outputDirectory  x0  p0  gamma  stepSize
output/1944314149639206e60f063d7e8a15346903fdba 1.0 0.0    0.0      0.01
output/6ed95255177ec6764d325f3b0026c4dbefa37461 1.0 0.0    1.0      0.01
Exporting output/_query_result.csv ..
```

## Visualization

Visualization scripts such as `plot2d.py` are executed by default for all `outputDirectory` in `_query_result.csv`.

By using the `-q` option mentioned above you can partially visualize the results.

Example:

```sh
python ../../visualization-scripts/view_database.py -H equation_x0 equation_p0 equation_gamma dynamics_stepSize -q "equation_x0 == 1 & equation_p0 == 0 & equation_gamma <= 1"
python ../../visualization-scripts/plot_time_evolution.py -t time --data position -o t-x.png
```

Result:

```console
Exporting output/1944314149639206e60f063d7e8a15346903fdba/t-x.png ..
Exporting output/6ed95255177ec6764d325f3b0026c4dbefa37461/t-x.png ..
```

## Querying image paths

The option `-f` can be used to append a file path to the query results.

Example:

```
python ../../visualization-scripts/view_database.py -H equation_x0 equation_p0 equation_gamma dynamics_stepSize -q "equation_x0 == 1 & equation_p0 == 0 & equation_gamma <= 1" -f t-x.png
```

Result:

```console
Header prefixes are removed for displaying on terminal by default. To show prefixes, use "--showPrefix".
Result:
                                        outputDirectory  x0  p0  gamma  stepSize
output/1944314149639206e60f063d7e8a15346903fdba/t-x.png 1.0 0.0    0.0      0.01
output/6ed95255177ec6764d325f3b0026c4dbefa37461/t-x.png 1.0 0.0    1.0      0.01
Exporting output/_query_result.csv ..
```

The displayed file paths are useful for opening files directly from the terminal by copying and pasting, etc.
