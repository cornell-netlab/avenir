from argparse import ArgumentParser
import os
import plotter
import csv
import random

parser = ArgumentParser(description='process graphs for microbenchmarks')
parser.add_argument('data', help='the data file',type=str, default=None)
parser.add_argument('output', help='directory for output files', type=str, default=None)
parser.add_argument('timeout', help='the timeout, in seconds, used for the experiment', type=float, default=None)

args = parser.parse_args()

def parse_data(f):
    data = []
    slow_data = []
    with open(f,'r') as csvfile:
        csvrows = csv.DictReader(csvfile)
        for row in csvrows:
            if row["time"] is None or row["time"] == "time" or not(bool(row["succeed"])) :
                continue
            else:
                row["time"] = float(row["time"]) / 1000.0
                if row["time"] > args.timeout: continue
                if "acts" in row and "data" in row:
                    row["acts_div_data"] = float (row["acts"]) / float(row["data"])
                    print row["acts_div_data"]
                    row["data_div_acts"] = float (row["data"]) / float(row["acts"])
                data.append(row)
                if row["time"] > 1.5:
                    slow_data.append(row)

    # with open ("%s_slow"%f,'w') as csvfile :
    #     writer = csv.DictWriter(csvfile,fieldnames=["bits","keys","tables","vars","data","acts","edits","succeed","time"])
    #     for row in slow_data:
    #         writer.writerow(row)

    return data

def get_bounds(k, data):
    hi = 0
    for d in data:
        if d[k] == k : continue
        if int(d[k]) > hi:
            hi = int(d[k])
    lo = hi
    for d in data:
        if d[k] == k : continue
        if int(d[k]) < lo:
            lo = int(d[k])
    return (lo, hi)

def percent_timeout(xlabel,data, fix=None):
    xs = list(set(int(d[xlabel]) for d in data if d[xlabel] != xlabel))
    timeouts = []
    distribs = []
    averages = []
    for x in xs:
        count = 0
        times = []
        total = 0
        for d in data:
            if d[xlabel] == xlabel: continue
            if int(d[xlabel]) != int(x) : continue
            if fix is not None and int(d[fix[0]]) != int(d[fix[0]]): continue
            total += 1
            times.append(min(float(d["time"]), args.timeout))
            if d["succeed"] != "true": count += 1
        if len(times) == 0:
            continue
        timeouts.append(float(count)/float(total))
        distribs.append(times)
        averages.append(sum(times) / len(times))
    title_prc = "Percent_Timeout_100Edits_32bits_%s" % xlabel
    title_avg = "Mean_Time_100Edits_32bits_%s" % xlabel
    title_viol = "Violin_100Edits_32bits_%s" % xlabel
    if fix is not None:
        app_str = str(fix[0]) + str(fix[1])
        title_prc += app_str
        title_avg += app_str
    assert len(xs) == len(averages)
    plotter.scatter(args.output, xs, timeouts, title_prc, xlabel, "% timeout", ylim=(0.0,1.0))
    plotter.scatter(args.output, xs, averages, title_avg, xlabel, "time(s)", ylim=(0.0,args.timeout))
    plotter.violins(args.output, xs, distribs, title_viol,xlabel, "time (s)", ylim=(0.0,args.timeout))



def scatter_color(xlabel, ylabel, data, maxtime, xlim=None, ylim=None):
    xs = []
    ys = []
    cs = []
    for d in random.sample(data,k=10000):
        if d[xlabel] == xlabel: continue
        xs.append(float(d[xlabel]))
        ys.append(float(d[ylabel]))
        time = float(d["time"])
        if time > maxtime :
            time = maxtime
        cs.append(time)
    title = "{}_vs_{}_vs_time".format(xlabel,ylabel)
    print "generating"
    plotter.scatter(args.output, xs, ys, title, xlabel, ylabel, ylim=ylim, xlim=xlim, colors=cs)

    
def plot_scatter(x,y,label,data,maxy=None,fix=None):
    xs = []
    ys = []
    for d in data:
        if d[fix[0]] == fix[0]: continue
        if fix is None or int(d[fix[0]]) == int(fix[1]):
            xs.append(int(d[x]))
            if maxy is not None and float(d[y]) > float(maxy):
                ys.append(float(maxy))
            else:
                ys.append(float(d[y]))
    print "data for", fix, len(xs), len(ys)
    if maxy is None:
        ylim = None
    else:
        ylim = (0,maxy)
    plotter.scatter(args.output,xs, ys, label, x, y, ylim=ylim)

def num_bits (data, maxy):
    dataset = {}
    for d in data:
        if d["time"] == "time": continue
        x = (int(d["bits"]) * (int(d["keys"]) + int(d["outs"])))
        time = min(float(d["time"]), maxy)
        if x not in dataset:
             dataset[x] = [time]
        else:
             dataset[x].append(time)
    xs = dataset.keys()
    ys = [dataset[x] for x in xs]
    plotter.violins(args.output, xs, ys, "Bits_100EditsTime", "bits", "time (s)", widths=30,
                    xlines = [(["L2","Router"], 210, 220, 210, 20, 0),
                              (["L2-L4 Router"], 494, 510, 250, 120, 0),
                              (["L2/L3 tunnelling"], 1122, 590, 150, 90, 0),
                              (["Ethernet Frame"],1500, 1510, 250, 90, 0)])

def main ():
    print "getting data"
    data = parse_data(args.data)

    # min_key, max_key = get_bounds ("keys", data)
    # min_out, max_outs = get_bounds ("outs", data)

    # num_bits(data, args.timeout)
    percent_timeout("keys",data)
    # percent_timeout("outs",data)
    # percent_timeout("tables", data)
    # percent_timeout("vars", data)
    # percent_timeout("data", data)
    # percent_timeout("acts", data)

    # percent_timeout("data_div_acts", [d for d in data
    #                                   if d["vars"] == "15"])
    # percent_timeout("acts_div_data", [d for d in data
    #                                   if d["vars"] == "15"])



    # scatter_color("data","acts", data, args.timeout)
    # scatter_color("acts","data", data, args.timeout)

    # scatter_color("vars","acts", data, args.timeout)
    # scatter_color("acts","vars", data, args.timeout)

    # scatter_color("data","vars", data, args.timeout)
    # scatter_color("vars","data", data, args.timeout)

    # scatter_color("tables","acts", data, args.timeout)
    # scatter_color("acts","tables", data, args.timeout)

    # scatter_color("tables","data", data, args.timeout)
    # scatter_color("data","tables", data, args.timeout)

    # scatter_color("tables","vars", data, args.timeout)
    # scatter_color("vars","tables", data, args.timeout)

    # for i in range(min_key, max_key + 1):
    #     percent_timeout("vars", data, fix=("keys",i))
    #     # plot_scatter("outs","time", "100Edits_%dKeys"%i, data, maxy=args.timeout, fix=("keys", i))
    # for i in range(min_out, max_outs + 1):
    #     percent_timeout("keys", data, fix=("outs",i))
    #     # plot_scatter("keys","time", "100Edits_%dOuts"%i, data, maxy=args.timeout, fix=("outs", i))



if "__main__" == __name__: main()
