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

matplotlib.use("Agg")
# TODO: particle-on-paraboloid以外にも対応させる
# TODO: dhallからa,bの値を取得できるように変更


def paraboloid_plot():
    x = np.arange(-1.5, 1.5, 0.1)
    y = np.arange(-1.5, 1.5, 0.1)
    X, Y = np.meshgrid(x, y)
    Z = X**2 + Y**2
    return (X, Y, Z)

    # fig = plt.figure(figsize=(6,6))
    # ax = fig.add_subplot(111, projection='3d')

    # Plot a 3D surface
    # ax.plot_surface(X, Y, Z)


def plot2d(outputDirList, xPathArgv, interval_, frameRate_):
    for outputDirRegExp in outputDirList:
        xPath = os.path.join(outputDirRegExp, xPathArgv)

        # yPath = os.path.join(outputDirRegExp, yPathArgv)

        # xName = os.path.splitext(os.path.basename(xPath))[0]
        # yName = os.path.splitext(os.path.basename(yPath))[0]

        df = pd.read_csv(xPath, names=["x", "y", "z"])
        cols = len(df)
        colsDigits = len(str(cols))
        imgDir = os.path.join(outputDirRegExp, "img")
        if os.path.exists(imgDir):
            shutil.rmtree(imgDir)
        os.makedirs(imgDir, exist_ok=True)
        # plt.figure()
        # fig, ax = plt.subplots()
        #     # set xlim and ylim
        # x_min = min(df["x"])
        # x_max = max(df["x"])
        # y_min = min(df["y"])
        # y_max = max(df["y"])
        # z_min = min(df["z"])
        # z_max = max(df["z"])
        # xaxisRange = x_max - x_min
        # yaxisRange = y_max - y_min
        # zaxisRange = z_max - z_min
        # df = pd.concat([position, momentum], axis=1)
        j = 0
        for i in range(cols):
            if i % interval_ == 0:
                # fig, ax = plt.subplots()
                ax = plt.axes(projection="3d")
                ax.use_sticky_edges = False
                # ax.set_xlim(x_min - xaxisRange * (0.05), x_max + xaxisRange * (0.05))
                # ax.set_ylim(y_min - yaxisRange * (0.05), y_max + yaxisRange * (0.05))
                # ax.set_zlim(z_min - zaxisRange * (0.05), z_max + zaxisRange * (0.05))
                # ax.plot(df[xLabel][:i], df[yLabel][:i])
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
                )

                (X, Y, Z) = paraboloid_plot()
                ax.plot_surface(X, Y, Z, alpha=0.2)
                # fig, ax = plt.subplots( nrows=1, ncols=1 )
                # plt.ion()
                # ax.plot(df_x,df_y)
                # plt.ioff()

                imgOutputPath = os.path.join(
                    outputDirRegExp,
                    "img",
                    "position_{}.png".format(str(j).zfill(colsDigits)),
                )
                plt.savefig(imgOutputPath)
                print(imgOutputPath)
                plt.close("all")
                j += 1
        imgRegExp = os.path.join(imgDir, ("position_%0" + str(colsDigits) + "d.png"))
        movRegExp = os.path.join(outputDirRegExp, ("position_movie.mp4"))
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
        # (X, Y, Z) = paraboloid_plot()
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
    parser.add_argument(
        "--outputDirRegExp",
        help="parent directory of data. example: --outputDirRegExp=output/*/",
        required=True,
    )

    parser.add_argument(
        "--data", help="3d data. example: --data position.csv", required=True
    )
    parser.add_argument(
        "--interval",
        help="intervals for exporting images. example: --interval 10",
        default=10,
        type=int,
    )
    parser.add_argument(
        "--framerate",
        help="frame rates for exporting images. example: --framerate 10",
        default=10,
        type=int,
    )
    # parser.add_argument("--y", help="y data. example: --y momentum.csv", required=True)
    # parser.add_argument('--labels', nargs="*", type=str, help='a list of label for plotting data',default=[])

    args = parser.parse_args()
    outputDirRegExp_ = args.outputDirRegExp
    outputDirList = glob.glob(outputDirRegExp_)
    xPathArgv = args.data
    intervalArgv = args.interval
    frameRateArgv = args.framerate
    # yPathArgv = args.y
    # labelListArgv = args.labels

    plot2d(outputDirList, xPathArgv, intervalArgv, frameRateArgv)
