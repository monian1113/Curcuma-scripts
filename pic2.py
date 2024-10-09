#!/usr/bin/python
# Author: liaoxuezhu,1113monian@live.cn
# Created Time: Wed Jan 17 13:20:08 2024
# Example pic.py   

import pandas as pd
import sys

def calculate_average(bed1, bed2, output_file):
    df1 = pd.read_csv(bed1, sep='\t', header=None)
    df2 = pd.read_csv(bed2, sep='\t', header=None)

    df1.columns = ['chr', 'start', 'end']
    df2.columns = ['chr', 'start', 'end', 'PIC']

    merged = pd.merge(df1, df2, how='inner', on=['chr'], suffixes=('_1', '_2'))

    merged = merged[(merged['start_2'] >= merged['start_1']) & (merged['end_2'] <= merged['end_1'])]

    result = merged.groupby(['chr', 'start_1', 'end_1'])['PIC'].mean().reset_index()

    result.to_csv(output_file, sep='\t', header=False)

if __name__ == "__main__":
    bed1 = sys.argv[1]
    bed2 = sys.argv[2]
    output_file = sys.argv[3]
    calculate_average(bed1, bed2, output_file)
