#!/usr/bin/env python3

# expect arguments: <output-file> <batch> <batch_observations> <incr> <incr_observations> <dd> <dd_observations> <dd+incr> <dd+incr_observations>
# where each <config> is a file with one analysis latency observation per line, <observations> lines

import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
import csv
import sys
import statistics

blue   = '#1f77b4'
orange = '#ff7f0e'
green  = '#2ca02c'
red    = '#d62728'


batch_observations = int(sys.argv[3])
batch_ys = np.array(range(batch_observations))/float(batch_observations)
incr_observations = int(sys.argv[5])
incr_ys = np.array(range(incr_observations))/float(incr_observations)
dd_observations = int(sys.argv[7])
dd_ys = np.array(range(dd_observations))/float(dd_observations)
dd_incr_observations = int(sys.argv[9])
dd_incr_ys = np.array(range(dd_incr_observations))/float(dd_incr_observations)

batch = np.sort([float(line) for line in open(sys.argv[2])])/float(1000)
incr = np.sort([float(line) for line in open(sys.argv[4])])/float(1000)
dd = np.sort([float(line) for line in open(sys.argv[6])])/float(1000)
dd_incr = np.sort([float(line) for line in open(sys.argv[8])])/float(1000)

plt.rc('text')
plt.rc('font', family='serif',size=16.0)
plt.rc('legend', edgecolor='white',fontsize="x-large",handlelength=0,framealpha=0)

plt.rc('axes',labelsize='x-large',linewidth=1.5,labelpad=-15)
plt.rc('xtick.major',width=1.5)
plt.rc('ytick.major',width=1.5)

plt.rc('xtick',labelsize='large')
plt.rc('ytick',labelsize='large')

plt.yticks([0.6,0.7,0.8,0.9,1.0])
plt.xlabel(r"Analysis Latency (sec)")

## LINEAR X AXIS
plt.axis([0,60,0.6,1])
plt.xticks([0,15,30,45,60])
plt.plot(batch,batch_ys,color=blue)
plt.plot(incr,incr_ys,color=orange)
plt.plot(dd,dd_ys,color=green)
plt.plot(dd_incr,dd_incr_ys,color=red)

## LOG X AXIS
#plt.axis([1.0,100,0.6,1])
#plt.semilogx(batch,batch_ys,color=blue)
#plt.semilogx(incr,incr_ys,color=orange)
#plt.semilogx(dd,dd_ys,color=green)
#plt.semilogx(dd_incr,dd_incr_ys,color=red)

plt.savefig(sys.argv[1],dpi=400, bbox_inches='tight')
    
