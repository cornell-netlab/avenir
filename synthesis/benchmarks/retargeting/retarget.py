from argparse import ArgumentParser
import os
import plotter
import csv

parser = ArgumentParser(description='retargeting demos')
parser.add_argument('--run', help='do run the experiments instead of only data collection',
                    action='store_const', const=True, default=False)

args = parser.parse_args()

def parse_data(f):
    data = [(0.0,0.0)]
    with open("%s.csv" % f,'r') as csvfile:
        csvrows = csv.DictReader(csvfile)
        for row in csvrows:
            print row
            data.append((float(row["time"]) / 1000.0, (int(row["log_inst_size"]) * 100.0) / 1000.0))
    time_so_far = 0.0
    acc_data = []
    for (time,percent) in data:
        time_so_far = time + time_so_far
        acc_data.append((time_so_far, percent))

    return {t:p for (t,p) in acc_data}


def main ():
    experiments = ["self", "action_decomp", "metadata", "early_validate"]

    if args.run:
        for exp in experiments:
            print exp
            os.system("sh {0}.sh | tee {0}.csv".format(exp))
            os.system("sh {0}_hot.sh | tee {0}_hot.csv".format(exp))

    for exp in experiments:
        print "plotting", exp, "data"
        plotter.plot_series(data0 = parse_data(exp), name0 = "cold start",
                            data1 = parse_data(exp + "_hot"), name1 = "hot start",
                            name = exp,
                            xlabel=(exp + " time (s)"),
                            ylabel="% completed")

    print "generating graphs"
    plotter.plot_series(data0 = parse_data("self"), name0 = "self",
                        data1 = parse_data("action_decomp"), name1 = "ActDec",
                        data2 = parse_data("metadata"), name2 = "Meta",
                        data3 = parse_data("early_validate"), name3 = "Valid",
                        xlabel = "time (s)",
                        ylabel = "completion %",
                        name = "retargeting")






if "__main__" == __name__: main()
