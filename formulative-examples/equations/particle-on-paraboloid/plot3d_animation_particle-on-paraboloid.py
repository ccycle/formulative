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
import subprocess
import shutil
import dhall

matplotlib.use("Agg")


def dhallPathToDict(path):
    with open(path) as f:
        s = f.read()
    dhallToDict = dhall.loads(s)
    return dhallToDict


def paraboloid_meshgrid(relPath, xmin, xmax, ymin, ymax, zmax):
    path = os.path.join(relPath, "setting.dhall")
    dhallToDict = dhallPathToDict(path)
    a = dhallToDict["equation"]["a"]
    b = dhallToDict["equation"]["b"]
    x = np.arange(xmin, xmax, 0.01)
    y = np.arange(ymin, ymax, 0.01)
    X, Y = np.meshgrid(x, y)
    Z = (X / a) ** 2 + (Y / b) ** 2
    return (X, Y, Z)


def plot3d(outputDirList, xPathArgv, interval_, frameRate_, fileName):
    for outputDirRegExp in outputDirList:

        xPath = os.path.join(outputDirRegExp, xPathArgv)

        df = pd.read_csv(xPath, names=["x", "y", "z"])
        cols = len(df)
        colsDigits = len(str(cols))
        imgDir = os.path.join(outputDirRegExp, "img")
        if os.path.exists(imgDir):
            shutil.rmtree(imgDir)
        os.makedirs(imgDir, exist_ok=True)

        j = 0
        for i in range(cols):
            if i % interval_ == 0:

                ax = plt.axes(projection="3d")
                # ax.use_sticky_edges = False

                xLabel = "x"
                yLabel = "y"
                zLabel = "z"
                xline = df["x"]
                yline = df["y"]
                zline = df["z"]
                ax.plot3D(xline[:i], yline[:i], zline[:i])
                ax.plot3D(xline, yline, zline, lw=0)
                ax.plot(df[:i][xLabel], df[:i][yLabel], lw=0)
                ax.scatter(
                    df[i : i + 1][xLabel],
                    df[i : i + 1][yLabel],
                    df[i : i + 1][zLabel],
                    s=20,
                    c="black",
                )

                # fix x and y axis
                max_axis_val = ((np.abs(df[["x", "y"]])).max()).max()
                ax.scatter(-max_axis_val, -max_axis_val, 0, s=0)
                ax.scatter(max_axis_val, max_axis_val, 0, s=0)

                xmin, xmax = ax.get_xlim()
                ymin, ymax = ax.get_ylim()
                zmin, zmax = ax.get_zlim()

                # projection
                ax.plot(
                    xs=xline[:i], ys=yline[:i], zs=zmin, zdir="z", c="gray", alpha=0.5
                )
                ax.plot(
                    xs=yline[:i], ys=zline[:i], zs=xmin, zdir="x", c="gray", alpha=0.5
                )
                ax.plot(
                    xs=xline[:i], ys=zline[:i], zs=ymax, zdir="y", c="gray", alpha=0.5
                )

                ax.scatter(
                    df[i : i + 1][xLabel],
                    df[i : i + 1][yLabel],
                    zmin,
                    c="black",
                    s=5,
                    alpha=0.5,
                )
                ax.scatter(
                    df[i : i + 1][xLabel],
                    ymax,
                    df[i : i + 1][zLabel],
                    c="black",
                    s=5,
                    alpha=0.5,
                )
                ax.scatter(
                    xmin,
                    df[i : i + 1][yLabel],
                    df[i : i + 1][zLabel],
                    c="black",
                    s=5,
                    alpha=0.5,
                )

                xmin, xmax = ax.get_xlim()
                ymin, ymax = ax.get_ylim()
                zmin, zmax = ax.get_zlim()

                ax.set_xlim(xmin, xmax)
                ax.set_ylim(ymin, ymax)
                # ax.set_zlim(zmin, zmax)

                # plot surface
                (X, Y, Z) = paraboloid_meshgrid(
                    outputDirRegExp,
                    -max_axis_val,
                    max_axis_val,
                    -max_axis_val,
                    max_axis_val,
                    zmax,
                )
                ax.plot_surface(X, Y, Z, alpha=0.2, color="gray")

                fileName_base = os.path.splitext(os.path.basename(fileName))[0]
                imgOutputPath = os.path.join(
                    outputDirRegExp,
                    "img",
                    "{}_{}.png".format(fileName_base, str(j).zfill(colsDigits)),
                )
                plt.savefig(imgOutputPath)
                print(imgOutputPath)
                plt.close("all")
                j += 1

        fileName_base = os.path.splitext(os.path.basename(fileName))[0]
        imgRegExp = os.path.join(
            imgDir, ("{}_%0".format(fileName_base) + str(colsDigits) + "d.png")
        )
        movRegExp = os.path.join(outputDirRegExp, ("{}").format(fileName))
        cmd = (
            "ffmpeg -y -r "
            + str(frameRate_)
            + " -i "
            + imgRegExp
            + " -vcodec libx264 -pix_fmt yuv420p -r "
            + str(frameRate_)
            + " "
            + movRegExp
        )
        subprocess.run(cmd, shell=True)
        print(movRegExp)
        # ax = plt.axes(projection="3d")

        # Data for a three-dimensional line
        # xline = df["x"]
        # yline = df["y"]
        # zline = df["z"]
        # ax.plot3D(xline, yline, zline, "gray")

        # plot surface
        # (X, Y, Z) = paraboloid_meshgrid()
        # ax.plot_surface(X, Y, Z, alpha=0.2)
        # plt.figure()
        # df.plot(x=xName, y=yName)
        # fig, ax = plt.subplots( nrows=1, ncols=1 )
        # plt.ion()
        # ax.plot(position,momentum)
        # plt.ioff()
        # imgOutputPath = os.path.join(outputDirRegExp, "position.png")
        # plt.savefig(imgOutputPath)
        # plt.close("all")


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="an example program")
    # parser.add_argument(
    #     "-O",
    #     "--outputDirRegExp",
    #     help="parent directory of data. example: --outputDirRegExp=output/*/",
    #     default="output/*/",
    # )
    parser.add_argument(
        "--queryResult",
        help="path for database. example: --outputDirRegExp=output/*/",
        default="query_result.csv",
    )

    parser.add_argument(
        "--data", help="3d data. example: --data position.csv", required=True
    )
    parser.add_argument(
        "-i",
        "--interval",
        help="intervals for exporting images. example: --interval 10",
        default=10,
        type=int,
    )
    parser.add_argument(
        "-f",
        "--framerate",
        help="frame rates for exporting images. example: --framerate 10",
        default=10,
        type=int,
    )
    parser.add_argument(
        "-o",
        "--output",
        help="file name of output movie (without extension). example: -o position.mp4",
        required=True,
    )

    args = parser.parse_args()
    queryResultArgv = args.queryResult
    # outputDirList = glob.glob(queryResultArgv)
    queryResultDF = pd.read_csv(queryResultArgv)
    outputDirList = queryResultDF["export_outputDirectory"]
    xPathArgv = args.data
    intervalArgv = args.interval
    frameRateArgv = args.framerate
    fileNameArgv = args.output

    plot3d(outputDirList, xPathArgv, intervalArgv, frameRateArgv, fileNameArgv)
