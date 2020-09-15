from argparse import ArgumentParser
import os
import plotter
import csv

parser = ArgumentParser(description='retargeting demos')
parser.add_argument('--run', help='do run the experiments instead of only data collection',
                    action='store_const', const=True, default=False)

args = parser.parse_args()



def parse_data(f):
    data = []
    with open("%s.csv" % f,'r') as csvfile:
        csvrows = csv.DictReader(csvfile)
        for row in csvrows:
            data.append((float(row["time"]), (int(row["log_inst_size"]) * 100.0) / 1000.0))
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

    for exp in experiments:
        print "plotting", exp, "data"
        plotter.plot_series(parse_data(exp), name = exp, xlabel="time", ylabel="completion %")

    # print "plotting action decomp data"
    # plotter.plot_series(parse_data("action_decomp.csv"), name = "action_decomp)
    # print "collecting metadata data"
    # plotter.plot_series(parse_data("metadata.csv"))
    # print "collecting early validation data"
    # plotter.plot_series(parse_data("early_validate.csv"))

    print "generating graphs"
    plotter.plot_series(data0 = parse_data("self"),
                        data1 = parse_data("action_decomp"),
                        data2 = parse_data("metadata"),
                        data3 = parse_data("early_validate"),
                        xlabel = "time (ms)", ylabel = "completion %",
                        name = "retargeting")






if "__main__" == __name__: main()
