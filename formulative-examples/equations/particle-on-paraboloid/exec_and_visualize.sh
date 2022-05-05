find ./settingFiles -name "*.dhall" -exec cabal exec -- particle-on-paraboloid -s {} \;
python ../../visualization-scripts/make_database.py
python ../../visualization-scripts/plot3d.py --data position.csv
python ../../visualization-scripts/view_database.py -H  equation_xInit equation_pxInit -S equation_xInit -o "position.png"
# python ../../visualization-scripts/view_database.py -H equation_a equation_xInit equation_pyInit -S equation_pyInit -q " export_outputDirectory.str.contains(\"76f3aa7e81855a911a627f715ac0f23c7e836063\") "