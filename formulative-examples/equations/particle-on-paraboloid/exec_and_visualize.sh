find ./settingFiles -name "*.dhall" -exec cabal exec -- particle-on-paraboloid -s {} \;
python ../../visualization-scripts/make_database.py
python ../../visualization-scripts/plot3d.py --data position.csv
python ../../visualization-scripts/view_database.py -H  equation_xInit equation_pxInit -S equation_xInit -f "position.png"
# python ../../visualization-scripts/plot_phase_space --x position.csv --y momentum.csv