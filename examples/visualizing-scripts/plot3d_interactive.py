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

# example:
# python ../scripts/plot2d.py --outputDirRegExp output/7ced1919b958b83d/ --x position.csv --y momentum.csv
# matplotlib.use("Agg")

# TODO: ラベルからパスを取得してプロットできるようにする
# TODO: Haskell側でplotのみ行うようにする


def paraboloid_plot():
    x = np.arange(-1.5, 1.5, 0.1)
    y = np.arange(-1.5, 1.5, 0.1)
    X, Y = np.meshgrid(x, y)
    Z = X**2 + Y**2
    return (X, Y, Z)

    # fig = plt.figure(figsize=(6,6))
    # ax = fig.add_subplot(111, projection='3d')

    # Plot a 3D surface
    ax.plot_surface(X, Y, Z)


def plot2d(outputDirList, xPathArgv):
    for outputDirRegExp in outputDirList:
        xPath = os.path.join(outputDirRegExp, xPathArgv)

        # yPath = os.path.join(outputDirRegExp, yPathArgv)

        # xName = os.path.splitext(os.path.basename(xPath))[0]
        # yName = os.path.splitext(os.path.basename(yPath))[0]

        dataCSV = pd.read_csv(xPath, names=["x", "y", "z"])

        # df = pd.concat([position, momentum], axis=1)
        ax = plt.axes(projection="3d")

        # Data for a three-dimensional line
        xline = dataCSV["x"]
        yline = dataCSV["y"]
        zline = dataCSV["z"]
        ax.plot3D(xline, yline, zline, "gray")

        # plot surface
        (X, Y, Z) = paraboloid_plot()
        ax.plot_surface(X, Y, Z, alpha=0.2)
        # plt.figure()
        # df.plot(x=xName, y=yName)
        # fig, ax = plt.subplots( nrows=1, ncols=1 )
        # plt.ion()
        # ax.plot(position,momentum)
        # plt.ioff()
        # imgOutputPath = os.path.join(outputDirRegExp, "position.png")
        plt.show()
        # plt.savefig(imgOutputPath)
        # plt.close("all")


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="an example program")
    parser.add_argument(
        "--outputDirRegExp",
        help="parent directory of data. example: --outputDirRegExp=output/*/",
        required=True,
    )

    parser.add_argument(
        "--data", help="3d data. example: --data position.csv", required=True
    )
    # parser.add_argument("--y", help="y data. example: --y momentum.csv", required=True)
    # parser.add_argument('--labels', nargs="*", type=str, help='a list of label for plotting data',default=[])

    args = parser.parse_args()
    outputDirRegExp_ = args.outputDirRegExp
    outputDirList = glob.glob(outputDirRegExp_)
    xPathArgv = args.data
    # yPathArgv = args.y
    # labelListArgv = args.labels

    plot2d(outputDirList, xPathArgv)
