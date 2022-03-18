import pandas as pd
import argparse
import os
import glob
import re

parser = argparse.ArgumentParser()
parser.add_argument("-d","--databaseDir", help="relative path of database.csv")
args = parser.parse_args()
if args.databaseDir:
    print("databaseDir")
    dirname = os.path.dirname(args.databaseDir)
# string (condition function) -> path
# df.query('equation.a <= 1',engine='python')