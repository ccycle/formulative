# coding: UTF-8
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os
import matplotlib
import argparse
import glob
import shutil
import subprocess

# example:
# python ../scripts/plot2d.py --outputDirRegExp output/7ced1919b958b83d/ --x df_x.csv --y df_y.csv
matplotlib.use("Agg")
# matplotlib.rcParams["axes.xmargin"] = 0.05
# matplotlib.rcParams["axes.ymargin"] = 0.05

# TODO: ラベルからパスを取得してプロットできるようにする
# TODO: Haskell側でplotのみ行うようにする


def plot2d(outputDirList, xPathArgv, yPathArgv, interval_, frameRate_):
    for outputDirRegExp in outputDirList:
        xPath = os.path.join(outputDirRegExp, xPathArgv)
        yPath = os.path.join(outputDirRegExp, yPathArgv)

        xLabel = os.path.splitext(os.path.basename(xPath))[0]
        yLabel = os.path.splitext(os.path.basename(yPath))[0]

        df_x = pd.read_csv(xPath, names=[xLabel])
        df_y = pd.read_csv(yPath, names=[yLabel])

        df = pd.concat([df_x, df_y], axis=1)
        cols = len(df)
        colsDigits = len(str(cols))
        imgDir = os.path.join(outputDirRegExp, "img")
        if os.path.exists(imgDir):
            shutil.rmtree(imgDir)
        os.makedirs(imgDir, exist_ok=True)
        # plt.figure()
        # fig, ax = plt.subplots()
        #     # set xlim and ylim
        x_max = max(df_x[xLabel])
        y_min = min(df_y[yLabel])
        y_max = max(df_y[yLabel])
        xaxisRange = 2 * x_max
        yaxisRange = y_max - y_min

        # plt.xlim(-x_max, x_max)
        # plt.ylim(y_min, y_max)
        j = 0
        for i in range(cols):
            if i % interval_ == 0:
                fig, ax = plt.subplots()
                # ax.use_sticky_edges = False
                # ax.set_xlim(-x_max - xaxisRange * (0.05), x_max + xaxisRange * (0.05))
                # ax.set_ylim(y_min - yaxisRange * (0.05), y_max + yaxisRange * (0.05))
                # df[:i].plot(x=xLabel, y=yLabel)
                ax.plot(df[:i][xLabel], df[:i][yLabel])
                ax.scatter(x=df[i : i + 1][xLabel], y=df[i : i + 1][yLabel], s=20)
                ax.plot(df[xLabel], df[yLabel], lw=0)  # dummy plot
                # fig, ax = plt.subplots( nrows=1, ncols=1 )
                # plt.ion()
                # ax.plot(df_x,df_y)
                # plt.ioff()

                imgOutputPath = os.path.join(
                    outputDirRegExp,
                    "img",
                    "phase_space_{}.png".format(str(j).zfill(colsDigits)),
                )
                plt.savefig(imgOutputPath)
                print(imgOutputPath)
                plt.close("all")
                j += 1
        imgRegExp = os.path.join(imgDir, ("phase_space_%0" + str(colsDigits) + "d.png"))
        movRegExp = os.path.join(outputDirRegExp, ("phase_space_movie.mp4"))
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


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="an example program")
    parser.add_argument(
        "--outputDirRegExp",
        help="parent directory of data. example: --outputDirRegExp=output/*/",
        required=True,
    )

    parser.add_argument(
        "--x", help="1D data for x axis. example: --x x.csv", required=True
    )
    parser.add_argument(
        "--y", help="1D data for y axis. example: --y y.csv", required=True
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
    # parser.add_argument('--labels', nargs="*", type=str, help='a list of label for plotting data',default=[])

    args = parser.parse_args()
    outputDirRegExp_ = args.outputDirRegExp
    outputDirList = glob.glob(outputDirRegExp_)
    xPathArgv = args.x
    yPathArgv = args.y
    intervalArgv = args.interval
    frameRateArgv = args.framerate
    # labelListArgv = args.labels

    plot2d(outputDirList, xPathArgv, yPathArgv, intervalArgv, frameRateArgv)
