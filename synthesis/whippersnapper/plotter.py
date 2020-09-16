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

def plot_series(data0, data1=None, data2=None, data3=None, name = "plot", xlabel = "LABELME", ylabel = "LABELME"):
  print("adding data0")
  fig = plt.figure(figsize=(3.7,1.0)) # Override fig size based on trial
  xs = sorted(data0.keys())
  ys = [data0[x] for x in xs]
  plt.plot(xs, ys, c='#DB4437', label='Cold cache', ls='-', zorder=2)
  # plt.axhline(y=5, c='#4285F4', ls=':', label='y=5', zorder=1)

  if data1:
    print("adding data1")
    xs = sorted(data1.keys())
    ys = [data1[x] for x in xs]
    plt.plot(xs, ys, label='Hot cache', ls='-', zorder=3)
    # plt.text(75, 1, "text", size="smaller")

  if data2:
    print("adding data2")
    xs = sorted(data2.keys())
    ys = [data2[x] for x in xs]
    plt.plot(xs, ys, label='No cache', ls='-', zorder=3)
    # plt.text(75, 1, "text", size="smaller")

  if data3:
    print("adding data3")
    xs = sorted(data3.keys())
    ys = [data3[x] for x in xs]
    plt.plot(xs, ys, label='E', ls='-', zorder=3)
    plt.text(75, 1, "text", size="smaller")

  # plt.annotate("annotation", xy=(25, 2.5), xytext=(40, 1), arrowprops=dict(arrowstyle="->"))

  # plt.xlim(0, 10000)
  # plt.ylim(0, 100)
  print("labelling")
  plt.xlabel(xlabel, labelpad=0)
  plt.ylabel(ylabel)
  plt.legend(loc='upper left', ncol=2, bbox_to_anchor=(0,1.1))
  # plt.xticks(np.arange(0, 101, 25))
  # plt.yticks(np.arange(0, 11, 2))
  print("saving")
  fig.savefig("%s.pdf" % name)
  print("closing")
  plt.close(fig)
  print("done")


def plot():
  (data0, data1) = read_data()
  plot_series(data0, data1)
