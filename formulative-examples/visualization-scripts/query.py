import pandas as pd
import argparse
import os
import glob
import re
import pathlib

def getParentDirStr(pathStr):
    p_rel = pathlib.Path(pathStr)
    p_parent = p_rel.parent
    return str(p_parent)

def relPathToAbsPath(path):
    p_rel = pathlib.Path(path)
    p_abs = p_rel.resolve()
    return str(p_abs)

def str_to_df_query(df,x):
    return df.query(x)

if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument("-d","--databaseDir",
     help="relative path of database",
     default = "output/_database.csv"
     )
    parser.add_argument("-q","--queryExpr",
     help="expression of query",
     )
    parser.add_argument("-f","--fileName",
     help="target file",
     default=""
     )
    parser.add_argument("-o","--output",
     help="Whether to export the results of the query execution",
     default="query_result.csv"
     )
    parser.add_argument("-H","--header",
     help="header",
     nargs="*",
     type=str,
     default="export_outputDirectory"
     )
    args = parser.parse_args()
    dbPath =  args.databaseDir
    queryStr =  args.queryExpr
    df = pd.read_csv(dbPath)
    df1 = str_to_df_query(df,queryStr)
    # target
    fileName1 = args.fileName
    df2 = df1.copy()
    df2["export_outputDirectory"] = df2["export_outputDirectory"].map(lambda x: (x+"/"+fileName1))
    headerArg = args.header
    outputPath = args.output
    print(df2[["export_outputDirectory"]+headerArg].to_string(index=False))
    if outputPath != "":
        df1.to_csv(outputPath)
        print("Successfully exported to "+relPathToAbsPath(outputPath))
