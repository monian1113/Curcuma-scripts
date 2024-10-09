#!/usr/bin/python
# Author: liaoxuezhu,1113monian@live.cn
# Created Time: Wed Jan 17 11:37:56 2024
# Example pic.ok.py   

import pandas as pd
import sys

def calculate_pic(allele_frequencies):
    pic = 1 - sum([freq**2 for freq in allele_frequencies])
    return pic

def calculate_pic_from_file(file_path, output_file_path):
    df = pd.read_csv(file_path, sep="\t", header=None)

    pic_dict = {}

    for index, row in df.iterrows():
        if '{ALLELE:FREQ}' in row[4]:
            continue

        allele_frequencies = [float(x.split(":")[1]) for x in row[4].split(",")]

        pic = calculate_pic(allele_frequencies)

        pic_dict[row[1]] = pic

    with open(output_file_path, 'w') as file:
        for pos, pic in pic_dict.items():
            file.write(f"{pos}\t{pic}\n")

input_file_path = sys.argv[1]
output_file_path = sys.argv[2]

calculate_pic_from_file(input_file_path, output_file_path)
