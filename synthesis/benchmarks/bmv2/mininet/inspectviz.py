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
    # ctx.rel_line_to(2 * w, 0)
    # ctx.rel_line_to(0, 2 * h)
    # ctx.rel_line_to(-2 * w, 0)
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


def draw_table(ctx, num, num_valid, x, y, back_color, front_color):
    w=300
    h=240
    ctx.set_source_rgb(*back_color)
    ctx.set_line_width(7.5)
    ctx.set_tolerance(0.1)
    ctx.set_line_join(cairo.LINE_JOIN_ROUND)
    ctx.set_dash([7.5, 7.5], 0)
    draw_roundrect(ctx, x, y, w, h)

    ctx.set_source_rgb(*front_color)
    step=10
    print ("step", step)
    xi = x + 2*step
    wi = w - 4*step
    hi = float(h - (num + 3)*step)/float(num)
    print("height", hi)
    yi = y + 2*step
    for i in range(num_valid):
        draw_rectangle(ctx, xi, yi, wi, hi)
        yi = yi + hi + step

def draw_avenir(ctx, x, y):
    w=300
    h=180
    ctx.set_source_rgb(*DEEPPURPLE)
    ctx.set_line_width(7.5)
    ctx.set_tolerance(0.1)
    ctx.set_line_join(cairo.LINE_JOIN_ROUND)
    ctx.set_dash([7.5, 7.5], 0)
    draw_roundrect(ctx, x, y, w, h)

    ctx.set_source_rgb(*WHITE)
    ctx.move_to(x + 55, y + 110)
    ctx.set_font_size(60)
    ctx.select_font_face("Avenir", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
    ctx.show_text("Avenir")


def draw(da, ctx):
    print("draw")
    xpos = 250
    abs_ypos = 50
    tgt_ypos = 700
    avr_ypos = 400
    num = 8
    draw_table(ctx, num, num_abs, xpos, abs_ypos, BLUEGRAY, LIGHTBLUE)
    draw_avenir(ctx, xpos, avr_ypos)
    draw_table(ctx, num, num_tgt, xpos, tgt_ypos, BURNTORANGE, LIGHTORANGE)


def main():
    window = Gtk.Window()
    window.set_default_size(800,1200)
    window.add_events(Gdk.EventMask.KEY_PRESS_MASK)


    def update_counts():
        global num_abs
        global num_tgt
        with open("abs.rowdata") as f:
            num_abs = len(f.readlines())
        with open("tgt.rowdata") as f:
            num_tgt = len(f.readlines())

    def callback(window, event):
        global num_abs
        global num_tgt
        print("button press")
        update_counts()
        window.queue_draw()

    # window.connect('key-press-event', callback)
    window.connect('destroy', lambda w: Gtk.main_quit())

    drawingarea = Gtk.DrawingArea()
    window.add(drawingarea)
    drawingarea.connect('draw', draw)

    window.show_all()

    count = 0
    def update_state():
        global count
        callback(window,None)
        count = count + 1
        print("UPDATE", count)
        window.queue_draw()
        return True

    # Bind the function above as an 'idle' callback.
    cid = GLib.idle_add(update_state)

    # start the main loop
    Gtk.main()

if __name__ == "__main__":
    main()
