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


def scatter (outdir,xs,ys, label, xlabel, ylabel, ylim=None,xlim=None,colors=None):
  plt.rc("font", size=9)
  plt.rc("ytick", labelsize=9)
  plt.rc("xtick", labelsize=9)
  fig = plt.figure(figsize=(3.7,1.0))
  # fig.suptitle(label)
  plt.xlabel(xlabel, fontsize=9)
  plt.ylabel(ylabel, fontsize=9)
  if ylim is not None: plt.ylim(ylim[0],ylim[1])
  if xlim is not None: plt.xlim(xlim[0],xlim[1])
  print "generating"
  if colors is None:
    c = None
    s = None
  else:
    c = [-1*c for c in colors]
    s = [c * 1000 for c in colors]
  plt.scatter(xs,ys,c=c,s=s)
  f =  "%s/%s.pdf" % (outdir,label)
  f = f.replace("//","/")
  print "saving to", f
  fig.savefig(f)
  print "closing"
  plt.close(fig)
  print "done"

def base() :
  return 10

def exp(x):
  return base()**x

def log(x):
  math.log(x,base())

def violins (outdir,xs, ydistribs, label, xlabel, ylabel, ylim=None, widths=0.9, xlines=[]):
  plt.rc("font", size=60)
  plt.rc("ytick", labelsize=60)
  plt.rc("xtick", labelsize=60)
  fig,ax = plt.subplots(figsize=(37,10))
  # fig.suptitle(label)
  print "generating"
  plt.xlabel(xlabel, fontsize=60)
  plt.ylabel(ylabel, fontsize=60)
  if ylim is not None: plt.ylim(ylim[0],ylim[1])
  ax.violinplot(ydistribs,xs, widths=widths, showmeans=False, showmedians=True, showextrema=False)
  for (ls, x, xl, yl, off, rotation) in xlines:
    ax.axvline(x)
    for i,l in enumerate(ls):
      print "plotting", l, "at x=", (xl + (i * off)), "y=", yl
      ax.text(xl, yl - (i * off), l, rotation=rotation, size=40)
  # ax.set_yscale('log')
  f =  "%s/%s.pdf" % (outdir,label)
  f = f.replace("//","/")
  print "saving to", f
  fig.savefig(f)
  print "closing"
  plt.close(fig)
  print "done"
