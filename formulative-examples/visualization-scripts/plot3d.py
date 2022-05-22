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
        ["x", "y", "z"], axis=1
    )


def dhallPathToDict(path):
    with open(path) as f:
        s = f.read()
    dhallToDict = dhall.loads(s)
    return dhallToDict


def plot3d(outputDirList, xPathsArgv, fileName):
    for outputDirRegExp in outputDirList:
        xPaths = map(lambda x: os.path.join(outputDirRegExp, x) + ".csv", xPathsArgv)

        dataCSV = concatFromPaths(xPaths)

        ax = plt.axes(projection="3d")

        # Data for a three-dimensional line
        xline = dataCSV["x"]
        yline = dataCSV["y"]
        zline = dataCSV["z"]

        # plot line
        ax.plot3D(xline, yline, zline)

        # fix x and y axis
        max_axis_val = ((np.abs(dataCSV[["x", "y"]])).max()).max()
        ax.scatter(-max_axis_val, -max_axis_val, 0, s=0)
        ax.scatter(max_axis_val, max_axis_val, 0, s=0)

        xmin, xmax = ax.get_xlim()
        ymin, ymax = ax.get_ylim()
        zmin, zmax = ax.get_zlim()

        # projection
        ax.plot(
            xs=xline,
            ys=yline,
            zs=zmin,
            zdir="z",
            c="gray",
            alpha=0.5,
        )
        ax.plot(
            xs=yline,
            ys=zline,
            zs=xmin,
            zdir="x",
            c="gray",
            alpha=0.5,
        )
        ax.plot(
            xs=xline,
            ys=zline,
            zs=ymax,
            zdir="y",
            c="gray",
            alpha=0.5,
        )

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
        "-o",
        "--output",
        help="file name of output image. example: -o t-x.png",
        required=True,
    )

    args = parser.parse_args()
    queryResultArgv = args.queryResult
    queryResultDF = pd.read_csv(queryResultArgv)
    outputDirList = queryResultDF["export_outputDirectory"]
    xPathsArgv = args.data
    fileNameArgv = args.output

    if args.interactive:
        plot3d(outputDirList, xPathsArgv, fileNameArgv)
        plt.show()
    else:
        matplotlib.use("Agg")
        plot3d(outputDirList, xPathsArgv, fileNameArgv)
