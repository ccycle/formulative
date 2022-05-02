# coding: UTF-8
import argparse
import pandas as pd
import numpy as np
import pathlib
import json
from mergedeep import merge
import subprocess
import os
import dhall
from collections import OrderedDict
import glob

def getParentDirStr(pathStr):
    p_rel = pathlib.Path(pathStr)
    p_parent = p_rel.parent
    return str(p_parent)
def relPathToAbsPath(path):
    p_rel = pathlib.Path(path)
    p_abs = p_rel.resolve()
    return str(p_abs)

def dhallPathToDataFrame(path):
    with open(path) as f:
        s = f.read()
    dhallToDict = dhall.loads(s)
    parentPath = str(pathlib.Path(path).parent)
    dhallToDict["export"]["outputDirectory"] = parentPath

    # add dependent parameter to dictionary
    parentDir = getParentDirStr(path)
    dependentParameterPath = os.path.join(parentDir,"dependentParamater.dhall")
    if os.path.exists(dependentParameterPath):
        with open(dependentParameterPath) as f1:
            s1 = f1.read()
        dhallToDict1 = dhall.loads(s1)
        dhallToDict2 = {"equation":dhallToDict1}
        d1 = merge(dhallToDict,dhallToDict2)
        return pd.json_normalize(d1,sep="_")
    else:
        return pd.json_normalize(dhallToDict,sep="_")


def dhallPathListToDataFrame(all_paths):
    df = pd.concat((dhallPathToDataFrame(f) for f in all_paths))
    return df


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="an example program")
    parser.add_argument(
        "--settingFilePath",
        help='Relative path for setting file. example: --settingFilePath="output/*/setting.dhall"',
        default = "output/*/setting.dhall"
    )
    parser.add_argument(
        "--output",
        help='output file. example: --output="output/_database.csv"',
        default = "output/_database.csv"
    )

    args = parser.parse_args()
    settingFilePath_ = args.settingFilePath
    settingFilePathList = glob.glob(settingFilePath_)
    data = dhallPathListToDataFrame(settingFilePathList).sort_index(axis=1)
    outputPath_ = args.output
    data.to_csv(outputPath_, index=False)
    print("Exporting "+relPathToAbsPath(outputPath_)+" ..")
