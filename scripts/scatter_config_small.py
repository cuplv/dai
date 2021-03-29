#!/usr/bin/python3

# Expects to run as ./scatter_config.py <output-file> <config> <columns>
# where:
#   - output-file is a .png to output the scatter plot to
#   - config is in {batch, dd, incr, dd_incr} and controls the color/label of the plot
#   - columns is the number of columns in the input data
#   - input data (on stdin) is a csv where each column is the log output by one run of the experimental harness (../run_d1a_experiment)

import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import csv
import sys
import statistics

blue   = '#1f77b4'
orange = '#ff7f0e'
green  = '#2ca02c'
red    = '#d62728'

output_file = sys.argv[1]
if sys.argv[2] == "batch":
    color = blue
    label = "Batch"
elif sys.argv[2] == "dd":
    color = green
    label = "Demand-Driven"
elif sys.argv[2] == "incr":
    color = orange
    label = "Incremental"
elif sys.argv[2] == "dd_incr":
    color = red
    label = "Incremental \& Demand-Driven"
else:
    raise Exception("Unknown configuration: " + sys.argv[2])
runs = int(sys.argv[3])

with sys.stdin as csvfile:
    data = csv.reader(csvfile, delimiter=',')
    x_coords = []
    
    raw = [[] for i in range(runs)]
    
    for idx,row in enumerate(data):
        x_coords.append(int(idx))
        for i in range(runs):
            try:
                raw[i].append(float(row[i]))
            except:
                raw[i].append(0.0)

plt.rc('text')
plt.rc('font', family='serif',size=16.0)
plt.rc('legend', edgecolor='white',fontsize="x-large",handlelength=0,framealpha=0)

plt.rc('axes',labelsize='x-large',linewidth=1.5,labelpad=-15.0)
plt.rc('xtick.major',width=1.5)
plt.rc('ytick.major',width=1.5)

plt.rc('xtick',labelsize='large')
plt.rc('ytick',labelsize='large')

#linear axes
#plt.axis([0,3000,0,30000])
plt.axis([0,500,0,10000])

plt.ylabel(r"Analysis Time (ms)")
plt.xlabel(r"Cumulative Program Edits")

plt.xticks([0,250,500],label=True)
plt.yticks([0,5000,10000],label=True)

for i in range(runs):
    plt.scatter(x_coords, raw[i], s=0.08, alpha=0.3, color=color,marker=",")

plt.plot([1], [1],color=color, label=label)

plt.savefig(output_file, dpi=400)
    
