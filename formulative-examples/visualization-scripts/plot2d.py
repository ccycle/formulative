# coding: UTF-8
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os
import matplotlib
import argparse
import glob

matplotlib.use("Agg")

# TODO: ラベルからパスを取得してプロットできるようにする
# TODO: Haskell側でplotのみ行うようにする


def plot2d(outputDirList, xPathArgv, yPathArgv):
    for outputDirRegExp in outputDirList:
        xPath = os.path.join(outputDirRegExp, xPathArgv)
        yPath = os.path.join(outputDirRegExp, yPathArgv)

        xLabel = os.path.splitext(os.path.basename(xPath))[0]
        yLabel = os.path.splitext(os.path.basename(yPath))[0]

        df_x = pd.read_csv(xPath, names=[xLabel])
        df_y = pd.read_csv(yPath, names=[yLabel])

        df = pd.concat([df_x, df_y], axis=1)

        plt.figure()
        df.plot(x=xLabel, y=yLabel)
        # fig, ax = plt.subplots( nrows=1, ncols=1 )
        # plt.ion()
        # ax.plot(df_x,df_y)
        # plt.ioff()
        imgOutputPath = os.path.join(outputDirRegExp, "phase_space.png")
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
        "-x", help="1D data for x axis. example: -x x.csv", required=True
    )
    parser.add_argument(
        "-y", help="1D data for y axis. example: -y y.csv", required=True
    )
    # parser.add_argument('--labels', nargs="*", type=str, help='a list of label for plotting data',default=[])

    args = parser.parse_args()
    queryResultArgv = args.queryResult
    queryResultDF = pd.read_csv(queryResultArgv)
    outputDirList = queryResultDF["export_outputDirectory"]
    xPathArgv = args.x
    yPathArgv = args.y

    plot2d(outputDirList, xPathArgv, yPathArgv)
