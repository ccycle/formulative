find ./settingFiles -name "*.dhall" -exec cabal exec -- harmonic-oscillator -s {} \;
python ../../visualization-scripts/make_database.py
python ../../visualization-scripts/plot_time_evolution.py --t time.csv --x position.csv --fileName t-x.svg
python ../../visualization-scripts/view_database.py -H equation_dampingRatio equation_x0 equation_p0 -S equation_dampingRatio equation_x0 -f "t-x.svg"
# python ../../visualization-scripts/plot_phase_space --x position.csv --y momentum.csv