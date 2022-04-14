# coding: UTF-8
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os
import matplotlib
import argparse
import glob

# example:
# python ../scripts/plot2d.py --outputDirRegExp output/7ced1919b958b83d/ --t t.csv --t t.csv
matplotlib.use("Agg")

# TODO: ラベルからパスを取得してプロットできるようにする
# TODO: Haskell側でplotのみ行うようにする


def plot2d(outputDirList, xPathArgv, yPathArgv, fileNameArgv):
    for outputDirRegExp in outputDirList:
        xPath = os.path.join(outputDirRegExp, xPathArgv)
        yPath = os.path.join(outputDirRegExp, yPathArgv)

        tName = os.path.splitext(os.path.basename(xPath))[0]
        xName = os.path.splitext(os.path.basename(yPath))[0]

        t_ = pd.read_csv(xPath)
        t = t_[tName]
        x = pd.read_csv(yPath, names=[xName])

        df = pd.concat([t, x], axis=1)

        plt.figure()
        df.plot(x=tName, y=xName)
        # fig, ax = plt.subplots( nrows=1, ncols=1 )
        # plt.ion()
        # ax.plot(t,t)
        # plt.ioff()
        imgOutputPath = os.path.join(outputDirRegExp, fileNameArgv)
        plt.savefig(imgOutputPath)
        plt.close("all")


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="plot time-dependent value. example: ")
    parser.add_argument(
        "--outputDirRegExp",
        help="parent directory of data. example: --outputDirRegExp=output/*/",
        required=True,
    )

    parser.add_argument("--t", help="t data. example: --t time.csv", required=True)
    parser.add_argument("--x", help="x data. example: --x position.csv", required=True)
    parser.add_argument(
        "--fileName",
        help="file name of output image. example: --fileName t-x.png",
        required=True,
    )
    # parser.add_argument('--labels', nargs="*", type=str, help='a list of label for plotting data',default=[])

    args = parser.parse_args()
    outputDirRegExp_ = args.outputDirRegExp
    outputDirList = glob.glob(outputDirRegExp_)
    xPathArgv = args.t
    yPathArgv = args.x
    fileNameArgv = args.fileName
    # labelListArgv = args.labels

    plot2d(outputDirList, xPathArgv, yPathArgv, fileNameArgv)
