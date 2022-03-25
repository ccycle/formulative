import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os
import matplotlib
import argparse

# example:
# python ../scripts/plot2d.py --parentDir output/7ced1919b958b83d/ --x position.csv --y momentum.csv
matplotlib.use('Agg')

# TODO: ラベルからパスを取得してプロットできるようにする
# TODO: Haskell側でplotのみ行うようにする

def plot2d (outputDir,xPathArgv,yPathArgv):

    # outputDir = 'output/'
    xPath = os.path.join(outputDir,xPathArgv)
    yPath = os.path.join(outputDir,yPathArgv)

    xName = os.path.splitext(os.path.basename(xPath))[0]
    yName = os.path.splitext(os.path.basename(yPath))[0]

    position = pd.read_csv(xPath,names = [xName])
    momentum = pd.read_csv(yPath,names = [yName])

    df = pd.concat([position,momentum],axis=1)

    plt.figure()
    df.plot(x=xName,y=yName)
    # fig, ax = plt.subplots( nrows=1, ncols=1 )
    # plt.ion()
    # ax.plot(position,momentum)
    # plt.ioff()
    imgOutputPath = os.path.join(outputDir,'phase_space.png')
    plt.savefig(imgOutputPath)
    plt.close('all')

if __name__ == '__main__':

    parser = argparse.ArgumentParser(description='an example program')
    parser.add_argument('--parentDir', help='parent directory of data. example: --parentDir=output/',required=True)
    parser.add_argument('--x', help='x data. example: --x position.csv',required=True)
    parser.add_argument('--y', help='y data. example: --y momentum.csv',required=True)
    # parser.add_argument('--labels', nargs="*", type=str, help='a list of label for plotting data',default=[])

    args = parser.parse_args()
    outputDir = args.parentDir
    xPathArgv = args.x
    yPathArgv = args.y
    # labelListArgv = args.labels

    plot2d(outputDir,xPathArgv,yPathArgv)