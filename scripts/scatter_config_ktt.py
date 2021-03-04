#!/usr/local/bin/python3

# Expects to run as ./scatter_config.py <output-file> <config> <columns>
# where:
#   - output-file is a .png to output the scatter plot to
#   - config is in {batch, dd, incr, dd_incr} and controls the color/label of the plot
#   - columns is the number of columns in the input data
#   - input data (on stdin) is a csv where each column is the log output by one run of the experimental harness (../run_d1a_experiment)

import matplotlib as mpl
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
            raw[i].append(float(row[i]))

plt.rc('text', usetex=True)
plt.rc('font', family='serif',size=16.0)
plt.rc('legend', edgecolor='white',fontsize="x-large",handlelength=0,framealpha=0)

plt.rc('axes',labelsize='x-large',linewidth=1.5,labelpad=-15.0)
plt.rc('xtick.major',width=1.5)
plt.rc('ytick.major',width=1.5)

plt.rc('xtick',labelsize='large')
plt.rc('ytick',labelsize='large')

#linear axes
#plt.axis([0,3000,0,30000])
plt.axis([0,100,0,1000])

plt.ylabel(r"Analysis Time (sec)")
plt.xlabel(r"Cumulative Program Edits")

#plt.xticks([0,1000,2000,3000],labels=['0','','','3000'])
#plt.yticks(ticks=[0,5000,10000,15000,20000,25000,30000],labels=['0','','','','','','30'])
plt.xticks([0,50,100],labels=['0','','100'])
plt.yticks(ticks=[0,500,1000],labels=['0','','1'])

for i in range(runs):
    plt.scatter(x_coords, raw[i], s=0.08, alpha=0.3, color=color,marker=",")

plt.plot([1], [1],color=color, label=label)

#leg = plt.legend(ncol=2,loc='upper left', bbox_to_anchor = (-0.05,1))
#
#for legobj in leg.legendHandles:
#    legobj.set_linewidth(2.0)

plt.savefig(output_file, dpi=400, bbox_inches='tight')
    
