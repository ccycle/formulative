# coding: UTF-8
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os
import matplotlib
import argparse
import glob

matplotlib.use("Agg")


def concatFromPaths(all_files, names):
    return pd.concat((pd.read_csv(f, header=None) for f in all_files), axis=1).set_axis(
        names, axis=1
    )


def plot2d(
    outputDirList, tPathArgv, xPathsArgv, fileNameArgv, idxStartArgv, idxEndArgv
):
    for outputDirRegExp in outputDirList:

        tPath = os.path.join(outputDirRegExp, tPathArgv) + ".csv"
        # xPath = os.path.join(outputDirRegExp, xPathsArgv)

        xPaths = map(lambda x: os.path.join(outputDirRegExp, x) + ".csv", xPathsArgv)

        # tName = os.path.splitext(os.path.basename(tPath))[0]
        tName = tPathArgv
        # xName = os.path.splitext(os.path.basename(xPath))[0]

        # xPaths = map(lambda x: os.path.join(outputDirRegExp, x) + ".csv", xPathsArgv)

        t_ = pd.read_csv(tPath)
        t = t_[tPathArgv]
        # x = pd.read_csv(xPath, header=None, names=[xName])
        # dataCSV = concatFromPaths(xPaths)

        xPaths = map(lambda x: os.path.join(outputDirRegExp, x) + ".csv", xPathsArgv)

        x = concatFromPaths(xPaths, xPathsArgv)

        if idxEndArgv == 0:
            idxEnd = x.shape[0]
        else:
            idxEnd = idxEndArgv

        df = pd.concat([t, x], axis=1).iloc[idxStartArgv:idxEnd]

        plt.figure()
        df.plot(x=tName, y=xPathsArgv)
        # fig, ax = plt.subplots( nrows=1, ncols=1 )
        # plt.ion()
        # ax.plot(t,t)
        # plt.ioff()
        imgOutputPath = os.path.join(outputDirRegExp, fileNameArgv)
        print("Exporting " + imgOutputPath + " ..")
        plt.savefig(imgOutputPath)
        plt.close("all")


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="plot time-dependent value. example: ")

    parser.add_argument(
        "--queryResult",
        help="paths of database for plotting.",
        default="output/_query_result.csv",
    )

    parser.add_argument(
        "--idxStart", help="Index to start plotting.", default=0, type=int
    )
    parser.add_argument("--idxEnd", help="Index to end plotting.", default=0, type=int)

    parser.add_argument("-t", help="t data. example: -t time.csv", required=True)
    parser.add_argument(
        "--data",
        help='3d data. example: "--data position", "--data x y z"',
        required=True,
        nargs="*",
        type=str,
        default=[],
    )
    parser.add_argument(
        "-o",
        "--output",
        help="file name of output image. example: -o t-x.png",
        required=True,
    )
    # parser.add_argument('--labels', nargs="*", type=str, help='a list of label for plotting data',default=[])

    args = parser.parse_args()
    # outputDirRegExp_ = args.outputDirRegExp
    # outputDirList = glob.glob(outputDirRegExp_)
    queryResultArgv = args.queryResult
    queryResultDF = pd.read_csv(queryResultArgv)
    outputDirList = queryResultDF["export_outputDirectory"]
    tPathArgv = args.t
    xPathsArgv = args.data
    fileNameArgv = args.output
    idxEndArgv = args.idxEnd
    idxStartArgv = args.idxStart

    plot2d(outputDirList, tPathArgv, xPathsArgv, fileNameArgv, idxStartArgv, idxEndArgv)
