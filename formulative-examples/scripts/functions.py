import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import pandas as pd


def concat_df(all_file_paths):
    df = pd.concat((pd.read_csv(f) for f in all_file_paths))
    return df


def df_max(xs):
    print("max xs")
    print(xs)
    # list = []
    # for x in xs:

    list = map(lambda x: x.iloc[1].max(), xs)
    # print(x)
    # print(list)
    val = max(list)
    print(val)
    return val


def df_min(df):
    minVal = df.min().min()
    return minVal
