# coding: UTF-8
import pandas as pd
import numpy as np
import json
import subprocess

# TODO: 設定ファイルのdatabaseを作る
# パラメータを検索してそれに対応するURLを出力するようにしたい
# queryは別に作る？

# subprocess.run(["dhall-to-json"])

json_open = open("./scripts/test/paramTest.json", "r")
json_load = json.load(json_open)
data = pd.json_normalize(json_load)
data.to_csv("_database.csv", mode="a")
