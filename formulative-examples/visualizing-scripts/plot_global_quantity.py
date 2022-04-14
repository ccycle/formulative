# coding: UTF-8
import matplotlib.pyplot as plt
import pandas as pd
import matplotlib
import os
import argparse
import glob

matplotlib.use("Agg")


def plot_global_quantity(outputDirList, paramPathArgv, dataPathArgv, labelList):
    for outputDir in outputDirList:
        paramPath = os.path.join(outputDir, paramPathArgv)
        dataPath = os.path.join(outputDir, dataPathArgv)
        params_ = pd.read_csv(paramPath)
        labelName = os.path.splitext(os.path.basename(paramPath))[0]
        params = params_[[labelName]]
        data = pd.read_csv(dataPath)

        df = pd.concat([params, data], axis=1)

        # plt.figure()
        if labelList == []:
            df.plot(x=labelName, y=None)
        else:
            df.plot(x=labelName, y=labelList)
        imgOutputPath = os.path.splitext(dataPath)[0] + ".png"
        plt.savefig(imgOutputPath)
        plt.close("all")


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="an example program")
    parser.add_argument(
        "--outputDirRegExp",
        help='parent directory of data. example: --outputDirRegExp="output/*/"',
    )
    parser.add_argument(
        "--parameter",
        help="paramter data example: --parameterData=time.csv",
        default="time.csv",
    )
    parser.add_argument(
        "--data",
        help="global quantity data. example: --dependentVariableGlobalData=dependentVariableGlobal.csv",
        default="dependentVariableGlobal.csv",
    )
    parser.add_argument(
        "--labels",
        nargs="*",
        type=str,
        help="a list of label for plotting data",
        default=[],
    )

    args = parser.parse_args()
    outputDirRegExp_ = args.outputDirRegExp
    outputDirList = glob.glob(outputDirRegExp_)
    paramPathArgv = args.parameter
    dataPathArgv = args.data
    labelListArgv = args.labels

    plot_global_quantity(outputDirList, paramPathArgv, dataPathArgv, labelListArgv)
