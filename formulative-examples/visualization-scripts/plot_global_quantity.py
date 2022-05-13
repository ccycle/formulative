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
        print("Exporting " + imgOutputPath + " ..")
        plt.savefig(imgOutputPath)
        plt.close("all")


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="an example program")
    parser.add_argument(
        "--queryResult",
        help="paths of database for plotting.",
        default="output/_query_result.csv",
    )
    parser.add_argument(
        "--parameter",
        help="paramter data example: --parameterData=time.csv",
        default="time.csv",
    )
    parser.add_argument(
        "--data",
        help="global quantity data. example: --data=dependentVariable/_global.csv",
        default="dependentVariable/_global.csv",
    )
    parser.add_argument(
        "-H",
        "--header",
        nargs="*",
        type=str,
        help="a list of header for plotting data",
        default=[],
    )

    args = parser.parse_args()
    # outputDirRegExp_ = args.outputDirRegExp
    # outputDirList = glob.glob(outputDirRegExp_)
    queryResultArgv = args.queryResult
    queryResultDF = pd.read_csv(queryResultArgv)
    outputDirList = queryResultDF["export_outputDirectory"]
    paramPathArgv = args.parameter
    dataPathArgv = args.data
    labelListArgv = args.header

    plot_global_quantity(outputDirList, paramPathArgv, dataPathArgv, labelListArgv)
