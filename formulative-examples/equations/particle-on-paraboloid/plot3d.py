# coding: UTF-8
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os
import matplotlib
import argparse
import glob
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import dhall


def dhallPathToDict(path):
    with open(path) as f:
        s = f.read()
    dhallToDict = dhall.loads(s)
    return dhallToDict


def paraboloid_meshgrid(relPath, xmin, xmax, ymin, ymax):
    path = os.path.join(relPath, "setting.dhall")
    dhallToDict = dhallPathToDict(path)
    a = dhallToDict["equation"]["a"]
    b = dhallToDict["equation"]["b"]
    x = np.arange(xmin, xmax, 0.1)
    y = np.arange(ymin, ymax, 0.1)
    X, Y = np.meshgrid(x, y)
    Z = (X / a) ** 2 + (Y / b) ** 2
    return (X, Y, Z)


def plot2d(outputDirList, xPathArgv):
    for outputDirRegExp in outputDirList:
        xPath = os.path.join(outputDirRegExp, xPathArgv)

        dataCSV = pd.read_csv(xPath, names=["x", "y", "z"])

        ax = plt.axes(projection="3d")

        # Data for a three-dimensional line
        xline = dataCSV["x"]
        yline = dataCSV["y"]
        zline = dataCSV["z"]

        # plot line
        ax.plot3D(xline, yline, zline, "gray")

        # fix x and y axis
        max_axis_val = ((np.abs(dataCSV[["x", "y"]])).max()).max()
        ax.scatter(-max_axis_val, -max_axis_val, 0, s=0)
        ax.scatter(max_axis_val, max_axis_val, 0, s=0)

        xmin, xmax = ax.get_xlim()
        ymin, ymax = ax.get_ylim()
        zmin, zmax = ax.get_zlim()

        # ax.set_xlim(xmin, xmax)
        # ax.set_ylim(ymin, ymax)
        # ax.set_zlim(zmin, zmax)

        # plot surface
        (X, Y, Z) = paraboloid_meshgrid(
            outputDirRegExp, -max_axis_val, max_axis_val, -max_axis_val, max_axis_val
        )
        ax.plot_surface(X, Y, Z, alpha=0.2)

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

        imgOutputPath = os.path.join(outputDirRegExp, "position.png")
        print(imgOutputPath)
        plt.savefig(imgOutputPath)
        plt.close("all")


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="an example program")
    parser.add_argument(
        "-O",
        "--outputDirRegExp",
        help="parent directory of data. example: --outputDirRegExp=output/*/",
        default="output/*/",
    )

    parser.add_argument(
        "--data", help="3d data. example: --data position.csv", required=True
    )

    parser.add_argument(
        "-I",
        "--interactive",
        help="show interactive view",
        action="store_true",
    )

    args = parser.parse_args()
    outputDirRegExp_ = args.outputDirRegExp
    outputDirList = glob.glob(outputDirRegExp_)
    xPathArgv = args.data

    if args.interactive:
        plot2d(outputDirList, xPathArgv)
        plt.show()
    else:
        matplotlib.use("Agg")
        plot2d(outputDirList, xPathArgv)
