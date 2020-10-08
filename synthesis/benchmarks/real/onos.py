
import plotter
import os
import csv


onos_data_fp = "../onos_201_adds.csv"
command = "../../avenir onf-real p4programs/onos/pipelines/fabric/impl/src/main/resources/fabric.p4 p4programs/stratum-onos-demo/p4src/main.p4 log_edits.csv phys_edits.csv onos_fvs_full.csv assume.mt -I1 p4programs/onos/pipelines/fabric/impl/src/main/resources/include -I1 benchmarks/real/p4includes/ -I2 p4programs/stratum-onos-demo/ -I2 p4includes/ -data ../onos_201_adds.csv -b 100 -e 2 --fastcx --restrict-masks -w --domain-restr --hints exact --cache-edits 1 --min --cache-queries --no-deletes  --reach-restrict -s -S > ../fabric.csv"



def main ():
    onos_timestamps = []
    start_time = None
    with open(onos_data_fp,'r') as f:
        for line in csv.reader(f):
            try:
                t = float(line[0]) / 60
                if start_time :
                    onos_timestamps.append(t - start_time)
                else :
                    start_time = float(t)
                    onos_timestamps.append(0.0)
            except e:
                print "Failed to parse", line
                continue

    # print "file read"


    avenir_deltas = []
    with open("../fabric.csv", 'r') as f:
        for line in csv.DictReader(f):
            try:
                avenir_deltas.append(float(line['time'])/(1000.0 * 60.0))
            except e:
                print "Failed to parse", line
                continue

    print "file read"

    avenir_delay = 0.0
    avenir_ts = []
    for o_send_time, a_delta in zip(onos_timestamps,avenir_deltas):
        avenir_delay = avenir_delay + a_delta
        avenir_ts.append(avenir_delay)

    num_rules = len(avenir_ts)

    # onos_timestamps = [float(i) * (1.0 / float(40000)) * 15.0 for i in xrange(40000)]

    print "normalizing onos"
    onos_time_series = { t : 100.0 * float(i) / float(num_rules) for (i,t) in enumerate(onos_timestamps)}
    print "normalizing avenir"
    avenir_time_series = {t : 100.0 * float(i) / float(num_rules) for (i,t) in enumerate(avenir_ts)}

    print "plotting"
    plotter.plot_series(avenir_time_series)


    

        






if __name__ == "__main__": main()
