# coding: UTF-8
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os
import matplotlib
import argparse
import numpy as np
import dhall


def getRowNumber(df):
    return df.shape[0]


def getColumnNumber(df):
    return df.shape[1]


def concatFromPaths(all_files):
    return pd.concat((pd.read_csv(f, header=None) for f in all_files), axis=1).set_axis(
        ["x", "y"], axis=1
    )


def dhallPathToDict(path):
    with open(path) as f:
        s = f.read()
    dhallToDict = dhall.loads(s)
    return dhallToDict


def plot3d(
    outputDirList, xPathsArgv, fileName, scatterplotArgv, idxStartArgv, idxEndArgv
):

    for outputDirRegExp in outputDirList:
        xPaths = map(lambda x: os.path.join(outputDirRegExp, x) + ".csv", xPathsArgv)

        dataCSV = concatFromPaths(xPaths)

        if idxEndArgv == 0:
            idxEnd = dataCSV.shape[0]
        else:
            idxEnd = idxEndArgv

        ax = plt.axes()

        df = dataCSV.iloc[idxStartArgv:idxEnd]

        # Data for a three-dimensional line
        xline = df["x"]
        yline = df["y"]
        # zline = dataCSV["z"]

        if scatterplotArgv:
            # plot line
            ax.plot(xline, yline)
        else:
            ax.scatter(xline, yline, s=0.25)

        # fix x and y axis
        # max_axis_val = ((np.abs(dataCSV[["x", "y"]])).max()).max()
        # ax.scatter(-max_axis_val, -max_axis_val, 0, s=0)
        # ax.scatter(max_axis_val, max_axis_val, 0, s=0)

        # xmin, xmax = ax.get_xlim()
        # ymin, ymax = ax.get_ylim()
        # zmin, zmax = ax.get_zlim()

        # projection
        # ax.plot(
        #     xs=xline,
        #     ys=yline,
        #     zs=zmin,
        #     zdir="z",
        #     c="gray",
        #     alpha=0.5,
        # )
        # ax.plot(
        #     xs=yline,
        #     ys=zline,
        #     zs=xmin,
        #     zdir="x",
        #     c="gray",
        #     alpha=0.5,
        # )
        # ax.plot(
        #     xs=xline,
        #     ys=zline,
        #     zs=ymax,
        #     zdir="y",
        #     c="gray",
        #     alpha=0.5,
        # )

        imgOutputPath = os.path.join(outputDirRegExp, fileName)
        print("Exporting " + imgOutputPath + " ..")
        plt.savefig(imgOutputPath)
        plt.close("all")


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="an example program")

    parser.add_argument(
        "--queryResult",
        help="path for database.",
        default="output/_query_result.csv",
    )

    parser.add_argument(
        "--data",
        help='3d data. example: "--data position", "--data x y z"',
        required=True,
        nargs="*",
        type=str,
        default=[],
    )

    parser.add_argument(
        "-I",
        "--interactive",
        help="show interactive view",
        action="store_true",
    )
    parser.add_argument(
        "-S",
        "--scatterplot",
        help="plot points",
        action="store_false",
    )
    parser.add_argument(
        "-o",
        "--output",
        help="file name of output image. example: -o t-x.png",
        required=True,
    )
    parser.add_argument(
        "--idxStart", help="Index to start plotting.", default=0, type=int
    )
    parser.add_argument("--idxEnd", help="Index to end plotting.", default=0, type=int)

    args = parser.parse_args()
    queryResultArgv = args.queryResult
    queryResultDF = pd.read_csv(queryResultArgv)
    outputDirList = queryResultDF["export_outputDirectory"]
    xPathsArgv = args.data
    fileNameArgv = args.output
    scatterplotArgv = args.scatterplot
    idxStartArgv = args.idxStart
    idxEndArgv = args.idxEnd

    if args.interactive:
        plot3d(
            outputDirList,
            xPathsArgv,
            fileNameArgv,
            scatterplotArgv,
            idxStartArgv,
            idxEndArgv,
        )
        plt.show()
    else:
        matplotlib.use("Agg")
        plot3d(
            outputDirList,
            xPathsArgv,
            fileNameArgv,
            scatterplotArgv,
            idxStartArgv,
            idxEndArgv,
        )
