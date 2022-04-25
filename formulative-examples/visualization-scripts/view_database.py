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
    if x=="":
        return df
    else:
        return df.query(x)

def splitHeader(flag,s):
    if flag:
        return s.split("_")[-1]
    else:
        return s

if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument("-d","--databaseDir",
     help="relative path of database",
     metavar='PATH',
     default = "output/_database.csv"
     )
    parser.add_argument("-q","--queryExpr",
     help="expression of query",
     default=""
     )
    parser.add_argument("-f","--fileName",
     help="target file",
     default=""
     )
    parser.add_argument("-o","--output",
     help="Whether to export the results of the query execution",
     metavar='PATH',
     default="query_result.csv"
     )
    parser.add_argument("-S","--headerToSort",
     help="header to sort",
     metavar='HEADER',
     type=str,
     default = ""
     )
    parser.add_argument("-A","--isAscending",
     help="sort type",
     metavar='FLAG',
     default = True
     )
    parser.add_argument("-H","--header",
     help="header",
     nargs="*",
     metavar='HEADER',
     type=str,
     default="export_outputDirectory"
     )
    parser.add_argument("-P","--showPrefix",
     help="show header prefix for printing",
     action='store_false'
     )
    args = parser.parse_args()
    dbPath =  args.databaseDir
    queryStr =  args.queryExpr
    df = pd.read_csv(dbPath)
    df1 = str_to_df_query(df,queryStr)
    fileName1 = args.fileName
    df2 = df1.copy()
    df2["export_outputDirectory"] = df2["export_outputDirectory"].map(lambda x: (x+"/"+fileName1))
    headerArg = args.header
    headerToSortArg = args.headerToSort
    isAscendingArg = args.isAscending
    df3 = df2[["export_outputDirectory"]+headerArg]
    if headerToSortArg != "":
        df3 = df3.sort_values(headerToSortArg, ascending = isAscendingArg)
    outputPath = args.output

    # print
    flag = args.showPrefix
    if flag:
        print("Flag \"--showPrefix\" is disabled; Header prefixes are removed.")
        print("result:")
    print(df3.rename(columns=lambda s: splitHeader(flag, s)).to_string(index=False))

    # export
    if outputPath != "":
        df3.to_csv(outputPath)
        print("Successfully exported to "+relPathToAbsPath(outputPath))
