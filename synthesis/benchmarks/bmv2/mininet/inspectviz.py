#!/usr/bin/env python3

import cairo
import gi
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk, Gdk, GdkPixbuf, GObject, GLib
import math


num_abs=0
num_tgt=0

# COLORs
BLACK = (0,0,0)
WHITE = (1,1,1)
BLUEGRAY = (.4,.6,.8)
LIGHTBLUE = (.6,.8,1)
BURNTORANGE = (.8,.3,0)
LIGHTORANGE = (1, .651, .298)
#(.99,.84,.69)
DEEPPURPLE = (.22, .14, .23)

def rectangle(ctx, w, h):
    ctx.move_to(0, 0)
    ctx.rel_line_to(w,0)
    ctx.rel_line_to(0,h)
    ctx.rel_line_to(-1 * w,0)
    ctx.close_path()

# Creating function for make roundrect shape
def roundrect(ctx, x, y, w, h):
    r = 30
    ctx.arc(x+r, y+r, r, math.pi, 3*math.pi/2)
    ctx.arc(x+w-r, y+r, r, 3*math.pi/2, 0)
    ctx.arc(x+w-r, y+h-r, r, 0, math.pi/2)
    ctx.arc(x+r, y+h-r, r, math.pi/2, math.pi)
    ctx.close_path()

def draw_rectangle(ctx, x, y, w, h):
    ctx.save()
    ctx.new_path()
    ctx.translate(x,y)
    rectangle(ctx, w, h)
    ctx.fill()
    ctx.restore()

def draw_roundrect(ctx,x,y,w,h):
    ctx.save()
    ctx.new_path()
    ctx.translate(x,y)
    roundrect(ctx, 0, 0, w, h)
    ctx.fill()
    ctx.restore()

def draw_single_table(ctx, num, num_valid, x, y, back_color, front_color):
    w=300*1.2
    h=240*1.2
    ctx.set_source_rgb(*back_color)
    ctx.set_line_width(7.5)
    ctx.set_tolerance(0.1)
    ctx.set_line_join(cairo.LINE_JOIN_ROUND)
    ctx.set_dash([7.5, 7.5], 0)
    draw_roundrect(ctx, x, y, w, h)

    step=10
    xi = x + 2*step
    wi = w - 4*step
    hi = float(h - (num + 3)*step)/float(num)
    yi = y + 2*step
    for i in range(num_valid):
        ctx.set_source_rgb(*front_color)
        draw_rectangle(ctx, xi, yi, wi, hi)
        ctx.set_source_rgb(*BLACK)
        ctx.move_to(xi + (wi / 2.0), yi + 16)
        ctx.set_font_size(20)
        ctx.select_font_face(str(i), cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
        ctx.show_text(str(i))
        yi = yi + hi + step

def draw_double_table(ctx, num, num_valid, x, y, back_color, front_color):
    w=300*1.2
    h=240*1.2
    ctx.set_source_rgb(*back_color)
    ctx.set_line_width(7.5)
    ctx.set_tolerance(0.1)
    ctx.set_line_join(cairo.LINE_JOIN_ROUND)
    ctx.set_dash([7.5, 7.5], 0)
    draw_roundrect(ctx, x, y, w, h)

    ctx.set_source_rgb(*front_color)
    step=10
    xi = x + 2*step
    wi = w - 4*step
    hi = float(h - (num + 3)*step)/float(num)
    yi = y + 2*step
    for i in range(num_valid):
        # first rect
        ctx.set_source_rgb(*front_color)
        draw_rectangle(ctx, xi, yi, (wi - step)/2.0, hi)
        # first label
        ctx.set_source_rgb(*BLACK)
        ctx.move_to(xi + (wi / 4.0), yi + 16)
        ctx.set_font_size(20)
        ctx.select_font_face(str(i), cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
        ctx.show_text(str(i))

        # second rect
        ctx.set_source_rgb(*front_color)
        draw_rectangle(ctx, xi + ((wi + step)/2.0), yi, (wi - step)/2.0, hi)
        # second label
        ctx.set_source_rgb(*BLACK)
        ctx.move_to(xi + 3.0*(wi / 4.0), yi + 16)
        ctx.set_font_size(20)
        ctx.select_font_face(str(i), cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
        ctx.show_text(str(i))
        yi = yi + hi + step

def draw_avenir(ctx, x, y):
    w=300*1.2
    h=180*1.2
    ctx.set_source_rgb(*DEEPPURPLE)
    ctx.set_line_width(7.5)
    ctx.set_tolerance(0.1)
    ctx.set_line_join(cairo.LINE_JOIN_ROUND)
    ctx.set_dash([7.5, 7.5], 0)
    draw_roundrect(ctx, x, y, w, h)

    ctx.set_source_rgb(*WHITE)
    ctx.move_to(x + 80, y + 130)
    ctx.set_font_size(60)
    ctx.select_font_face("Avenir", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
    ctx.show_text("Avenir")

def update_counts(abs_queue, tgt_queue):
    global num_abs
    global num_tgt
    num_abs = abs_queue.qsize()
    num_tgt = tgt_queue.qsize()
    # print "THERE SHOULD BE", num_abs, "ABSTRACT ROWS"
    # print "THERE SHOULD BE", num_tgt, "TARGET ROWS"

def draw(abs_queue, tgt_queue, da, ctx):
    # print("draw")
    xpos = 250
    abs_ypos = 50
    tgt_ypos = 700
    avr_ypos = 400
    num = 8
    draw_single_table(ctx, num, num_abs, xpos, abs_ypos, BLUEGRAY, LIGHTBLUE)
    draw_avenir(ctx, xpos, avr_ypos)
    draw_double_table(ctx, num, num_tgt, xpos, tgt_ypos, BURNTORANGE, LIGHTORANGE)



def run(abs_queue, tgt_queue):
    print("starting GTK with", abs_queue, tgt_queue)
    window = Gtk.Window()
    window.set_default_size(800,1200)

    drawingarea = Gtk.DrawingArea()
    window.add(drawingarea)
    drawingarea.connect('draw', lambda da, ctx: draw(abs_queue, tgt_queue, da, ctx))
    window.show_all()

    window.connect('destroy', Gtk.main_quit)

    def update_state():
        update_counts(abs_queue,tgt_queue)
        window.queue_draw()
        return True

    # Bind the function above as an 'timeout' callback
    # this should trigger an update approximately every 10ms
    cid = GLib.timeout_add(10, update_state)

    # start the main loop
    Gtk.main()
