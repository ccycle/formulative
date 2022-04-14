import matplotlib.pyplot as plt
from matplotlib import animation
import pandas as pd
import functions as func
import numpy as np
import glob
import argparse
import matplotlib
import os

matplotlib.use("Agg")

fig, ax = plt.subplots()
title = ax.set_title(None)
# line, = ax.plot([], [], lw=2)


# plotlays, plotcols = [2], ["black","red"]

# initialize lines
# def linesFunc(n):
#     lines = []
#     for index in range(n):
#         lobj = ax.plot([], [], lw=2)[0]
#         lines.append(lobj)


# init
# def initFunc(xs, ys):

#     # # initialize
#     # for line in lines_:
#     #     line.set_data([], [])

#     # set xlim and ylim
#     x_max = func.df_max(xs)
#     y_min = func.df_min(ys)
#     y_max = func.df_max(ys)

#     ax.set_xlim(-x_max, x_max)
#     ax.set_ylim(-y_min, y_max)

#     # initialize title
#     title.set_text(None)

#     return ()


# function for initialize xs, ys
# def list_init(n):
#     empty_list = [[] for _ in range(n)]
#     return empty_list


# x_init_list
# x1,y1 = [],[]
# x2,y2 = [],[]

# frame_num = len(gps_data[0])

# animation function.  This is called sequentially
def animateFunc(i, xs, ys):
    ax.clear()
    for k, (x, y) in enumerate(zip(xs, ys)):
        ax.plot(x[:i], y[:i])
    # x_max = func.df_max(xs)
    # y_min = func.df_min(ys)
    # y_max = func.df_max(ys)

    # ax.set_xlim(-x_max, x_max)
    # ax.set_ylim(y_min, y_max)


def anim_func(xs, ys):
    def animate(i):
        return animateFunc(i, xs, ys)

    # def init():
    #     return initFunc(lines, xs, ys)

    animation.FuncAnimation(fig, animate, frames=10, interval=1, blit=True)
    # return anim


# def getDataFromPath (outputDir, dataPath):
#     paramPath = os.path.join(outputDir, paramPathArgv)
#     dataPath = os.path.join(outputDir, dataPathArgv)


def parseArgandReturnData():

    parser = argparse.ArgumentParser(description="an example program")
    parser.add_argument(
        "--outputDirRegExp",
        help='parent directory of data. example: --outputDirRegExp="output/*/"',
    )
    parser.add_argument(
        "--xs",
        help="x data example: --xs x.csv X.csv",
        type=str,
        nargs="+"
        # default="time.csv",
    )
    parser.add_argument(
        "--ys",
        help="x data example: --ys y.csv Y.csv",
        type=str,
        nargs="+"
        # default="time.csv",
    )

    args = parser.parse_args()
    outputDirRegExp_ = args.outputDirRegExp
    outputDirList = glob.glob(outputDirRegExp_)

    xsPathArgv = args.xs
    ysPathArgv = args.ys

    try:
        if len(xsPathArgv) != len(ysPathArgv):
            raise ValueError("error: the number of x and y data does not match")
    except ValueError as e:
        print(e)
        # print("error: the number of x and y data does not match")

    # xsData = func.concat_df(xsPathArgv)
    # ysData = func.concat_df(ysPathArgv)
    n = len(xsPathArgv)

    return (outputDirList, xsPathArgv, ysPathArgv, n)

    # labelListArgv = args.labels


def concatPath(outputDir, xsPathArgv, ysPathArgv):
    # for outputDir in outputDirList:
    xsRelPath = map(lambda xs: os.path.join(outputDir, xs), xsPathArgv)
    ysRelPath = map(lambda ys: os.path.join(outputDir, ys), ysPathArgv)
    return (xsRelPath, ysRelPath)


def getData(xsRelPath, ysRelPath):
    xsData = map(lambda path: pd.read_csv(path), xsRelPath)
    ysData = map(lambda path: pd.read_csv(path), ysRelPath)
    return (xsData, ysData)


if __name__ == "__main__":
    outputDirList, xsPathArgv, ysPathArgv, n = parseArgandReturnData()
    for outputDir in outputDirList:
        (xsRelPath, ysRelPath) = concatPath(outputDir, xsPathArgv, ysPathArgv)
        for x in xsRelPath:
            print(x)
        (xsData, ysData) = getData(xsRelPath, ysRelPath)
        print("xsData")
        for x in xsData:
            print(x)
        print("generate animation")
        anim = anim_func(xsData, ysData)
        filePath = os.path.join(outputDir, "movie.gif")
        print("save")
        anim.save(filePath)
