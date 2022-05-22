# coding: UTF-8
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os
import matplotlib
import argparse
import numpy as np
import subprocess
import shutil
import dhall

matplotlib.use("Agg")


def concatFromPaths(all_files):
    return pd.concat((pd.read_csv(f, header=None) for f in all_files), axis=1).set_axis(
        ["x", "y", "z"], axis=1
    )


def dhallPathToDict(path):
    with open(path) as f:
        s = f.read()
    dhallToDict = dhall.loads(s)
    return dhallToDict


def plot3d(outputDirList, xPathsArgv, interval_, frameRate_, fileName):
    for outputDirRegExp in outputDirList:
        xPaths = map(lambda x: os.path.join(outputDirRegExp, x) + ".csv", xPathsArgv)

        df = concatFromPaths(xPaths)
        cols = len(df)
        colsDigits = len(str(cols))
        imgDir = os.path.join(outputDirRegExp, "img")
        if os.path.exists(imgDir):
            shutil.rmtree(imgDir)
        os.makedirs(imgDir, exist_ok=True)

        # ax = plt.axes(projection="3d")
        max_axis_val = ((np.abs(df[["x", "y"]])).max()).max()
        j = 0

        ax = plt.axes(projection="3d")

        # read data
        xLabel = "x"
        yLabel = "y"
        zLabel = "z"
        xline = df[xLabel]
        yline = df[yLabel]
        zline = df[zLabel]

        for i in range(0, cols):
            if i % interval_ == 0:

                fig = plt.figure()
                ax = fig.add_subplot(111, projection="3d")

                # plot lines
                ax.plot3D(xline[: (i + 1)], yline[: (i + 1)], zline[: (i + 1)])
                ax.plot3D(xline, yline, zline, c="b", alpha=0.2)

                ax.scatter(
                    xline[i],
                    yline[i],
                    zline[i],
                    s=20,
                    c="black",
                )

                # fix x and y axis
                ax.scatter(-max_axis_val, -max_axis_val, 0, s=0)
                ax.scatter(max_axis_val, max_axis_val, 0, s=0)

                # projection
                xmin, xmax = ax.get_xlim()
                ymin, ymax = ax.get_ylim()
                zmin, zmax = ax.get_zlim()

                ax.plot3D(
                    xs=xline[: (i + 1)],
                    ys=yline[: (i + 1)],
                    zs=zmin,
                    zdir="z",
                    c="gray",
                    alpha=0.5,
                )
                ax.plot3D(
                    xs=yline[: (i + 1)],
                    ys=zline[: (i + 1)],
                    zs=xmin,
                    zdir="x",
                    c="gray",
                    alpha=0.5,
                )
                ax.plot3D(
                    xs=xline[: (i + 1)],
                    ys=zline[: (i + 1)],
                    zs=ymax,
                    zdir="y",
                    c="gray",
                    alpha=0.5,
                )

                ax.scatter(
                    xline[i],
                    yline[i],
                    zmin,
                    c="black",
                    s=5,
                    alpha=0.5,
                )
                ax.scatter(
                    xline[i],
                    ymax,
                    zline[i],
                    c="black",
                    s=5,
                    alpha=0.5,
                )
                ax.scatter(
                    xmin,
                    yline[i],
                    zline[i],
                    c="black",
                    s=5,
                    alpha=0.5,
                )

                # save figure
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
        print(cmd)
        subprocess.run(cmd, shell=True)
        print(movRegExp)


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="an example program")

    parser.add_argument(
        "--queryResult",
        help="paths of database for plotting. default:%(default)s",
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
        "-i",
        "--interval",
        help="intervals for exporting images. " + "default:%(default)s",
        default=10,
        type=int,
    )
    parser.add_argument(
        "-f",
        "--framerate",
        help="frame rates for exporting images. " + "default:%(default)s",
        default=10,
        type=int,
    )
    parser.add_argument(
        "-o",
        "--output",
        help="file name of output movie. example: -o position.mp4",
        required=True,
    )

    args = parser.parse_args()
    queryResultArgv = args.queryResult
    queryResultDF = pd.read_csv(queryResultArgv)
    outputDirList = queryResultDF["export_outputDirectory"]
    xPathsArgv = args.data
    intervalArgv = args.interval
    frameRateArgv = args.framerate
    fileNameArgv = args.output

    plot3d(outputDirList, xPathsArgv, intervalArgv, frameRateArgv, fileNameArgv)
