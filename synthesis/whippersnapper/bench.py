#!/usr/bin/python

import os
import shutil
import subprocess
import sys

def rules_for_obt(fn):
  edits_file = "whippersnapper/empty_edits.txt";
  fvs_file = "output/fvs.txt";
  assume_file = "whippersnapper/empty_assume.txt";
  commands_file = "output/commands.txt";
  commands_no_def_file = "output/commands_no_def.txt"

  with open(fvs_file, 'w') as fvs:
    fvs.write("standard_metadata.egress_spec,standard_metadata.egress_spec,9")
  
  with open(commands_file, 'r') as cmds:
    with open(commands_no_def_file, 'w') as cmdnd:
      for line in cmds:
        if not line.startswith("table_set_default"):
          cmdnd.write(line);
  

  subprocess.run(["./avenir", "to-obt", "output/main16.p4", edits_file, fvs_file, assume_file, "-b", "100", "-data", commands_no_def_file, "-e", "100", "-I", "whippersnapper/p4includes", "--thrift",]);

def run_whippersnapper(mx):
  if not os.path.isdir("whippersnapper/pipelines"):
    os.mkdir("whippersnapper/pipelines")

  for i in list(range(1, int(mx))):
    print(str(i));
    subprocess.run(["p4benchmark", "--feature", "pipeline", "--tables", str(i), "--table-size", "16"]);
    subprocess.run(["p4test", "--p4v", "14", "--pp", "output/main16.p4", "output/main.p4"]);
    rules_for_obt("output");
    
    shutil.move("output", "whippersnapper/pipelines/output_" + str(i));

def run_avenir():
  mx = 1;
  while os.path.isdir("whippersnapper/pipelines/output_" + str(mx)):
    mx += 1;
  
  for i in list(range(1, int(mx))):
    print(str(i));

cmd = sys.argv[1];

if cmd == "generate":
  mx = sys.argv[2];
  run_whippersnapper(mx);
elif cmd == "eval":
  run_avenir();
