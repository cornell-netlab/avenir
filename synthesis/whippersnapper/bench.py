#!/usr/bin/python

import os
import shutil
import subprocess
import sys
import time

# fvs gen

def pipeline_fvs(i):
    return "hdr.ethernet.dstAddr,hdr.ethernet.dstAddr,48\nstandard_metadata.egress_spec,standard_metadata.egress_spec,9"

def set_field_fvs(mx):
    fvs = "hdr.ptp.reserved2,hdr.ptp.reserved2,8\n";
    for i in list(range(0, mx)):
      fvs += "hdr.header_0.field_" + str(i) + ",hdr.header_0.field_" + str(i) + ",16\n"
    return fvs;

# whippersnapper commands
def whippersnapper_cmds():
  return { "set-field" : (["--operations"], [], set_field_fvs)
         , "pipeline" : (["--tables"], ["--table-size", "16"], pipeline_fvs) }

# generate the OBT, and the corresponding rules
def rewrite_cmd(cmd):
  try:
    pieces = cmd.split(",");
    params = pieces[2];
    fparams = params.split(" ")[2::3];
    fparams = ";".join(fparams);
    pieces[2] = fparams;
    #pieces = list(map(lambda s : s[s.find("0x"):] if s.find("0x") != -1 else s, pieces));
    fcmd = ",".join(pieces);
    return fcmd;
  except:
    return cmd;

def rewrite_cmds(cmds):
  fcmds = list(map(rewrite_cmd, cmds.split("\n")));
  fmcds = "\n".join(fcmds);
  return fmcds;

def rules_for_obt(fn, fvs):
  edits_file = "whippersnapper/empty_edits.txt";
  fvs_file = "output/fvs.txt";
  assume_file = "whippersnapper/empty_assume.txt";
  commands_file = "output/commands.txt";
  commands_no_def_file = "output/commands_no_def.txt"

  with open(fvs_file, 'w') as fvsf:
    fvsf.write(fvs);
  
  with open(commands_file, 'r') as cmds:
    with open(commands_no_def_file, 'w') as cmdnd:
      for line in cmds:
        if not line.startswith("table_set_default"):
          cmdnd.write(line);
  

  res = subprocess.run(["./avenir", "to-obt", "output/main16.p4", edits_file, edits_file, fvs_file, assume_file, "-b", "100", "-data", commands_no_def_file, "-e", "100", "-p", "-I", "whippersnapper/p4includes"], stdout = subprocess.PIPE);

  obt_commands = "output/obt_commands.txt";

  cmds = res.stdout.decode('utf-8');
  cmds = cmds.split("Edits\n")[1]; 
  with open(obt_commands, 'w') as f:
    f.write(rewrite_cmds(cmds));
  #except:
  #  print("no commands written");

def run_whippersnapper(ws_cmd, mx):
  if not os.path.isdir("whippersnapper/" + ws_cmd):
    os.mkdir("whippersnapper/" + ws_cmd)

  for i in list(range(1, int(mx))):
    print(str(i));
    (cmd_line1, cmd_line2, fvs) = whippersnapper_cmds()[ws_cmd];
    subprocess.run(["p4benchmark", "--feature", ws_cmd] + cmd_line1 + [str(i)] + cmd_line2);
    subprocess.run(["p4test", "--p4v", "14", "--pp", "output/main16.p4", "output/main.p4"]);
    rules_for_obt("output", fvs(i));
    
    shutil.move("output", "whippersnapper/" + ws_cmd + "/output_" + str(i));

# run the actual evaluation, using OBT as the logical program

def run_avenir(ws_cmd):
  mx = 1;
  while os.path.isdir("whippersnapper/" + ws_cmd + "/output_" + str(mx)):
    mx += 1;
  
  res = ""
  for i in list(range(1, int(mx))):
    output = "whippersnapper/" + ws_cmd + "/output_" + str(i) +"/";
    commands_file = output + "obt_commands.txt";
    edits_file = "whippersnapper/empty_edits.txt";
    assume_file = "whippersnapper/empty_assume.txt";
    fvs_file = output + "fvs.txt";
    
    st_time = time.perf_counter();
    subprocess.run(["./avenir", "from-obt", output + "main16.p4", edits_file, edits_file, fvs_file, assume_file, "-b", "100", "-data", commands_file, "-e", "100", "-p", "-I", "whippersnapper/p4includes"], stdout = subprocess.PIPE);
    end_time = time.perf_counter();
    elapsed = end_time - st_time;
    res += str(i) + "," + str(elapsed) + "\n"
  
  with open("whippersnapper/" + ws_cmd + "_res.csv", "w") as res_file:
    res_file.write(res);

cmd = sys.argv[1];
ws_cmd = sys.argv[2];

if cmd == "generate":
  mx = sys.argv[3];
  run_whippersnapper(ws_cmd, mx);
elif cmd == "eval":
  run_avenir(ws_cmd);
