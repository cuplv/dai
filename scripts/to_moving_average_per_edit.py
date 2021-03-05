#!/usr/local/bin/python3

import csv
import sys
import statistics

qpe = int(sys.argv[1])

with sys.stdin as csvfile:
    data = csv.reader(csvfile, delimiter='\t')

    raw  = []

    for (idx,row) in enumerate(data):
        raw.append(float(row[0]))
        if len(raw) % qpe == 0:
            print( statistics.mean(raw[-qpe:]))
