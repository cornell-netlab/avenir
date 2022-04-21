#! /usr/bin/env python

# -*- coding: utf-8 -*-


# This simple example on how to do animations using graph-tool. Here we do a
# simple simulation of an S->I->R->S epidemic model, where each vertex can be in
# one of the following states: Susceptible (S), infected (I), recovered (R). A
# vertex in the S state becomes infected either spontaneously with a probability
# 'x' or because a neighbor is infected. An infected node becomes recovered
# with probability 'r', and a recovered vertex becomes again susceptible with
# probability 's'.

# DISCLAIMER: The following code is definitely not the most efficient approach
# if you want to simulate this dynamics for very large networks, and/or for very
# long times. The main purpose is simply to highlight the animation capabilities
# of graph-tool.


from graph_tool.all import *
from numpy.random import *
import math
import sys, os, os.path
import itertools
import cairo

# We need some Gtk and gobject functions
import gi
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk, Gdk, GdkPixbuf, GObject, GLib

# Generate the graph
g = Graph()
n = int(sys.argv[1])
for i in range(n):
    g.add_vertex()

# layout positions
pos = g.new_vertex_property("vector<double>")
tick = (2.0*math.pi) / float(n)
for i,v in enumerate(g.vertices()):
    theta = tick*i
    pos[v] = (10 * math.cos(theta), 10 * math.sin(theta))
    print(pos[v])

# set nodes to server shapes
vertex_sfcs = g.new_vertex_property("object")
server = cairo.ImageSurface.create_from_png("host.png")
for v in g.vertices():
    vertex_sfcs[v] = server

# We will filter out edges which are in the "non-reachable" state, by masking
# them using a property map.
# reachable = g.new_edge_property("bool")
# for v in g.vertices():
#     for u in g.vertices():
#         e = g.add_edge(v,u)
#         reachable[e] = False
# g.set_edge_filter(reachable)

# Place Graph in Window
win = Gtk.Window()
win.set_default_size(1000, 800)
win.graph = GraphWidget(g, pos,
                        edge_color=[0.6, 0.6, 0.6, 1],
                        vertex_fill_color=[0.0, 0.0, 0.0, 0.0],
                        vertex_surface=vertex_sfcs)
win.add(win.graph)

def vertex(idx):
    return g.vertex(idx)

def has_edge(u,v):
    return u in g.get_in_neighbors(v)

def add_edge(g, src_idx, tgt_idx):
    src = vertex(src_idx)
    tgt = vertex(tgt_idx)
    if not has_edge(src,tgt):
        g.add_edge(src,tgt)

def update():
    count = 0
    edges = list(g.edges())
    for e in edges:
        g.remove_edge(e)
    for filename in os.listdir("."):
        if filename.split(".")[-1] == "reachable":
            src_idx = int(filename.split("_")[0][1:]) - 1
            fn = os.path.join(".", filename)
            with open(fn, 'r') as f:
                hosts = f.readlines()
            for tgt in hosts:
                tgt_idx = int(tgt[1:])
                add_edge(g, src_idx, tgt_idx)
                count = count + 1
    return count


# This function will be called repeatedly by the GTK+ main loop, and we use it
# to update the state according to the SIRS dynamics.
def update_state():

    num_edges = update()

    # # iterate through potential u -- v edges
    # # where u != v and u -/- v, randomly adding the edge
    # for u,v in itertools.product(g.vertices(), g.vertices()):
    #     if u != v \
    #        and u not in g.get_all_neighbors(v) \
    #        and state.is_reachable(u,v):
    #         g.add_edge(u,v)
    #         count = count + 1

    # The following will force the re-drawing of the graph, and issue a
    # re-drawing of the GTK window.
    win.graph.regenerate_surface()
    win.graph.queue_draw()

    # Stop updating when we've added all the edges
    print("UPDATED:", num_edges, "edges")
    # if num_edges < n*(n-1):
    #     return True
    # else:
    #     return False
    return True

# Bind the function above as an 'idle' callback.
cid = GLib.idle_add(update_state)

# We will give the user the ability to stop the program by closing the window.
win.connect("delete_event", Gtk.main_quit)


# Actually show the window, and start the main loop.
win.show_all()
Gtk.main()
