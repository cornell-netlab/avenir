# -*- coding: utf-8 -*-
import csv
import locale
import time
import matplotlib as mpl
import matplotlib.pyplot as plt
from cycler import cycler
from collections import defaultdict
import numpy as np
import math

locale.setlocale(locale.LC_ALL, 'en_US.UTF8')
from IPython.display import set_matplotlib_formats
set_matplotlib_formats('pdf')

onecolsize = (4, 1.5)   # Tweak based on figure's appearance in the paper
seaborn_colorblind = cycler('color', ['#0072B2', '#D55E00', '#009E73', '#CC79A7', '#F0E442', '#56B4E9'])
seaborn_muted = cycler('color', ['#4878CF', '#6ACC65', '#D65F5F', '#B47CC7', '#C4AD66', '#77BEDB'])

def setrcparams():
  # setup matplotlib rcparams
  plt.style.use(['seaborn-paper', 'seaborn-colorblind'])
  # color cyclers
  seaborn_colorblind = cycler('color', ['#0072B2', '#D55E00', '#009E73', '#CC79A7', '#F0E442', '#56B4E9'])
  seaborn_muted = cycler('color', ['#4878CF', '#6ACC65', '#D65F5F', '#B47CC7', '#C4AD66', '#77BEDB'])

  plt.rcParams['axes.prop_cycle'] =  seaborn_colorblind + cycler(linestyle=['-', '--', '-.', '--', '-.','-'])

  plt.rcParams['axes.axisbelow'] = True
  plt.rcParams['axes.edgecolor'] = 'lightgray'
  plt.rcParams['axes.facecolor'] = 'white'
  plt.rcParams['axes.spines.left'] = False
  plt.rcParams['axes.spines.bottom'] = False
  plt.rcParams["axes.spines.right"] = False
  plt.rcParams["axes.spines.top"] = False
  plt.rcParams['axes.grid'] = True
  plt.rcParams['axes.linewidth'] = 0.1


  plt.rcParams['grid.alpha'] = 0.4
  plt.rcParams['grid.color'] = 'gray'
  plt.rcParams['grid.linestyle'] = ':'
  plt.rcParams['grid.linewidth'] = 1.0

  plt.rcParams['hatch.linewidth'] = 1.0

  plt.rcParams['xtick.bottom'] = False
  plt.rcParams['ytick.left'] = False
  plt.rcParams['xtick.direction'] = 'in'
  plt.rcParams['ytick.direction'] = 'in'


  plt.rcParams['legend.edgecolor'] = 'none'
  plt.rcParams['legend.framealpha'] = 0.4
  plt.rcParams["legend.columnspacing"] = 0.4
  plt.rcParams["legend.handletextpad"] = 0.2

  plt.rcParams['savefig.bbox'] = 'tight'
  plt.rcParams['savefig.format'] = 'pdf'
  plt.rcParams['savefig.pad_inches'] = 0

  plt.rcParams['figure.figsize'] = onecolsize

  plt.rcParams['pdf.fonttype'] = 42
  plt.rcParams['ps.fonttype'] = 42
  plt.rcParams['pdf.compression'] = 9
  # plt.rcParams['text.usetex'] = True
  # plt.rcParams['pgf.texsystem']= "pdflatex"
  # plt.rcParams["font.sans-serif"] = "Linux Libertine"
  # plt.rcParams["text.latex.preamble"] = "\usepackage{libertine},\usepackage[libertine]{newtxmath},\usepackage[T1]{fontenc}"
  # plt.rcParams["pgf.preamble"] = "\usepackage{libertine},\usepackage[libertine]{newtxmath},\usepackage[T1]{fontenc}"
  # plt.rcParams["font.family"] = "sans-serif"

# Load the base config
setrcparams()

# Override some the common rcParams as needed
plt.rcParams['axes.spines.left'] = True
plt.rcParams['axes.spines.bottom'] = True
plt.rcParams["legend.columnspacing"] = 0.8
plt.rcParams["legend.handletextpad"] = 0.4

# Example plot

def read_data():
  # Parse and extract data
  data0 = dict()
  data1 = dict()
  for key in range(1, 100):
      data0[key] = math.sqrt(key)
      data1[key] = math.log(key)

  return (data0, data1)

def intercalate(data,x,i):
  print "intercalating", x, "the %dth key" % i
  if x in data:
    return data[x]
  else :
    xprev = None
    yprev = None
    for xnext, ynext in data.items():
      if xprev is None or yprev is None:
        xprev = xnext
        yprev = ynext
      else:
        if x > xnext:
          xprev = xnext
          yprev = ynext
        else :
          return ynext + ((x / xnext - xprev) * (ynext - yprev))

    return 100.0


def hi_rows(lo_data,hi_data):
  return [intercalate(hi_data,x,i) for (i,(x,y)) in enumerate(lo_data.items())]

def plot_series(data0, data1 = None):
  fig = plt.figure(figsize=(3.7,1.0)) # Override fig size based on trial
  xs0 = sorted(data0.keys())
  ys0 = [data0[x] for x in xs0]
  plt.plot(xs0, ys0, label='Avenir', ls='-', zorder=2)

  if data1:
    xs1 = sorted(data1.keys())
    ys1 = [data1[x] for x in xs1]
    # print (sorted(data1.items()))
    plt.plot(xs1, ys1, label='Avenir bcm adds', ls='-', zorder=3)
    # plt.annotate("annotation", xy=(25, 2.5), xytext=(40, 1), arrowprops=dict(arrowstyle="->"))

  # print "filling"
  # plt.fill_between(xs1, y1 = ys0, y2 = hi_rows(data1,data0), alpha = 0.2)
  # print "filled"
  plt.axvline(x=15, c='#FFCCCB', ls='-', label='ONOS completion', zorder=1)
  # plt.text(4.2, 90, "Avenir Runtime", size="xx-small", rotation=24)


  plt.xlim(left=0,right=16)
  plt.ylim(bottom=0,top=101)
  plt.xlabel("time (mins)", labelpad=0)
  plt.ylabel("% completed")
  plt.legend(loc='lower right', ncol=1, fontsize='x-small', bbox_to_anchor=(0.9,0))
  # plt.xticks(np.arange(0, 101, 25))
  # plt.yticks(np.arange(0, 11, 2))
  fig.savefig("plot.pdf")
  plt.close(fig)


def plot():
  (data0, data1) = read_data()
  plot_series(data0, data1)
