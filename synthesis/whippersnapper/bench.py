#!/usr/bin/python

import os
import random
import shutil
import subprocess
import sys
import time

import plotter as pl

# generate a rule, generic functions

# take a list of rule templates (see gen_rule).  Generates num rules, using the rule templates as evenly as possible.
def gen_rules(num, rule_temps):
  res = []
  for i in list(range(0, num)):
    j = i % len(rule_temps);
    rt = rule_temps[j];
    r = gen_rule(rt);
    res.append(r);
  return res;

# takes a list of strings and ints.  Replaces every int n with a hex string of width n,
# concatenates into a single list, and returns
def gen_rule(rule_temp):
  rule = map(adj_rule_piece, rule_temp);
  return "".join(rule);

def adj_rule_piece(p):
  if isinstance(p, str):
    return p;
  else:
    b = random.getrandbits(p);
    b = str(b); #add_colons(str(hex(b))[2:]);
    return b;

def add_colons(s):
  s2 = ""
  for (i, c) in enumerate(s):
    if i != 0 and i % 2 == 0:
        s2 += ":"
    s2 += c;
  return s2;


# fvs and rule gen

def pipeline_fvs(i):
    return "hdr.ethernet.dstAddr,hdr.ethernet.dstAddr,48\nstandard_metadata.egress_spec,standard_metadata.egress_spec,9"

def pipeline_rules(ind):
  res = []
  for i in list(range(0, ind)):
    (tbl_name, act) = ("forward_table", "forward") if i == 0 else ("table_" + str(i), "forward" + str(i)); 
    res += [["table_add " + tbl_name + " " + act + " ", 48, " => ", 9]];
  return res;

def set_field_fvs(ind):
    fvs = pipeline_fvs(ind);
    fvs += "\nhdr.ptp.reserved2,hdr.ptp.reserved2,8\n";
    for i in list(range(0, ind)):
      fvs += "hdr.header_0.field_" + str(i) + ",hdr.header_0.field_" + str(i) + ",16\n"
    return fvs;

def set_field_rules(ind):
  return [ ["table_add forward_table forward ", 48, " => ", 9]
         , ["table_add test_tbl mod_headers ", 8, " =>"]];


def add_header_fvs(ind):
    fvs = "hdr.ptp.reserved2,hdr.ptp.reserved2,8\nstandard_metadata.egress_spec,standard_metadata.egress_spec,9\n";
    fvs += "hdr.ethernet.dstAddr,hdr.ethernet.dstAddr,48\n";
    for i in list(range(0, ind)):
      fvs += "hdr.header_" + str(i) + ".field_0,hdr.header_" + str(i) + ".field_0,16\n"
    return fvs;


def add_header_rules(ind):
  return [ ["table_add forward_table forward ", 48, " => ", 9]
         , ["table_add test_tbl add_headers ", 8, " =>"]];


def rm_header_fvs(ind):
  return add_header_fvs(ind);

def rm_header_rules(ind):
  return [ ["table_add forward_table forward ", 48, " => ", 9]
         , ["table_add test_tbl remove_headers ", 8, " =>"]];

# whippersnapper commands
def whippersnapper_cmds():
  return { "set-field" : (["--operations"], [], set_field_fvs, set_field_rules)
         , "add-header" : (["--headers"], [], add_header_fvs, add_header_rules)
         , "rm-header" : (["--headers"], [], rm_header_fvs, rm_header_rules)
         , "pipeline" : (["--tables"], ["--table-size", "16"], pipeline_fvs, pipeline_rules) }

# generate the OBT, and the corresponding rules
def rewrite_cmd(cmd):
  try:
    pieces = cmd.split(",");
    if pieces[0] == "ADD":
      params = pieces[2];
      fparams = params.split(" ")[2::3];
      fparams = ";".join(fparams);
      pieces[2] = fparams;
      #pieces = list(map(lambda s : s[s.find("0x"):] if s.find("0x") != -1 else s, pieces));
      fcmd = ",".join(pieces);
      return fcmd;
    elif pieces[0] == "DEL":
      return cmd;
    elif pieces[0] == "":
      return "";
  except:
    return cmd;

def rewrite_cmds(cmds):
  fcmds = list(map(rewrite_cmd, cmds.split("\n")));
  fmcds = "\n".join(fcmds);
  return fmcds;

def non_cache_flags():
  return ["--reach-filter"];

def avenir_flags():
  x = ["--cache-edits", "1", "--cache-queries"]
  x.extend(non_cache_flags());
  return x;

def hot_start_flags():
  x = ["--hot-start"];
  x.extend(avenir_flags());
  return x;

def orig_to_obt(ws_cmd, fldr):
  return ("whippersnapper/" + fldr + "/" + ws_cmd + "_orig_to_obt_res.csv");

def rules_for_obt(ws_cmd, fldr, i, fn, num, rule_temps, fvs, flags = avenir_flags()):
  edits_file = "whippersnapper/empty_edits.txt";
  fvs_file = "output/fvs.txt";
  assume_file = "whippersnapper/empty_assume.txt";
  #commands_file = "output/commands.txt";
  commands_no_def_file = "output/commands_no_def.txt"

  with open(fvs_file, 'w') as fvsf:
    fvsf.write(fvs);
  
  rules = gen_rules(num, rule_temps);  
  
  with open(commands_no_def_file, 'w') as cmdnd:
    rules_str = "\n".join(rules);
    cmdnd.write(rules_str);
  #with open(commands_file, 'r') as cmds:
  #  with open(commands_no_def_file, 'w') as cmdnd:
  #    for line in cmds:
  #      if not line.startswith("table_set_default"):
  #        cmdnd.write(line);
  
  
  st_time = time.perf_counter();
  res = subprocess.run(["./avenir", "to-obt", "output/main16.p4", edits_file
                       , edits_file, fvs_file, assume_file, "-b", "100", "-data"
                       , commands_no_def_file, "-e", "100", "-p"] + flags + ["-I", "whippersnapper/p4includes"], stdout = subprocess.PIPE, stderr = subprocess.PIPE);
  end_time = time.perf_counter();
  elapsed = end_time - st_time;

  obt_commands = "output/obt_commands.txt";

  cmds = res.stdout.decode('utf-8');
  cmds = cmds.split("Edits\n")[1]; 
  with open(obt_commands, 'w') as f:
    f.write(rewrite_cmds(cmds));
  #except:
  #  print("no commands written");

  with open(orig_to_obt(ws_cmd, fldr), "a") as res_file:
    res_file.write(str(i) + "," + str(elapsed) + "\n")

def run_whippersnapper(ws_cmd, fldr,  rule_num, mx, flags = avenir_flags()):
  if not os.path.isdir("whippersnapper/" + fldr):
      os.mkdir("whippersnapper/" + fldr)

  if not os.path.isdir("whippersnapper/"+ fldr + "/" + ws_cmd):
      os.mkdir("whippersnapper/" + fldr + "/" + ws_cmd)

  mn = 1;
  if os.path.exists(orig_to_obt(ws_cmd, fldr)):
    with open(orig_to_obt(ws_cmd, fldr), 'r') as res_file:
      res = res_file.read();
      for lne in res.split("\n"):
        if ',' in lne:
          mn += 1;

  for i in list(range(mn, int(mx))):
    print(str(i));
    (cmd_line1, cmd_line2, fvs, get_rule_temps) = whippersnapper_cmds()[ws_cmd];
    subprocess.run(["p4benchmark", "--feature", ws_cmd] + cmd_line1 + [str(i)] + cmd_line2);
    subprocess.run(["p4test", "--p4v", "14", "--pp", "output/main16.p4", "output/main.p4"]);
    rules_for_obt(ws_cmd, fldr, i, "output", rule_num, get_rule_temps(i), fvs(i), flags);
    
    shutil.move("output", "whippersnapper/" + fldr + "/"+ ws_cmd + "/output_" + str(i));


# run the actual evaluation, using OBT as the logical program

def run_avenir(ws_cmd, fldr, rn, ceil_mx, flags):
  mx = 1;
  while os.path.isdir("whippersnapper/" + fldr + "/" + ws_cmd + "/output_" + str(mx)):
    mx += 1;

  mx = min(mx, ceil_mx);

  res = ""
  for i in list(range(1, int(mx))):
    print(ws_cmd + " " + str(i));
    output = "whippersnapper/" + fldr + "/" + ws_cmd + "/output_" + str(i) +"/";
    commands_file = "whippersnapper/cache_" + str(rn) + "/" + ws_cmd + "/output_" + str(i)  + "/obt_commands.txt";
    edits_file = "whippersnapper/empty_edits.txt";
    assume_file = "whippersnapper/empty_assume.txt";
    fvs_file = output + "fvs.txt";
    
    st_time = time.perf_counter();
    subprocess.run(["./avenir", "from-obt", output + "main16.p4", edits_file, edits_file, fvs_file, assume_file, "-b", "100", "-data", commands_file, "-e", "100", "-p"] + flags + ["-I", "whippersnapper/p4includes"], stdout = subprocess.PIPE, stderr = subprocess.PIPE);
    end_time = time.perf_counter();
    elapsed = end_time - st_time;
    res += str(i) + "," + str(elapsed) + "\n"
  
  with open("whippersnapper/" + fldr + "/" + ws_cmd + "_obt_to_orig_res.csv", "w") as res_file:
    res_file.write(res);

# plot

def read_data(fn):
  data_str = ""
  with open(fn) as f:
    data_str = f.read();
  return data_str;

def string_to_data(data_str):
  data = {};
  for line in data_str.split("\n"):
    if "," in line:
      pieces = line.split(",");
      data[int(pieces[0])] = float(pieces[1]);

  return data

def get_data(mx, f):
  s = read_data(f);
  data = string_to_data(s);
  data = { k: data[k] for k in data.keys() if k < mx };
  return data;

def adjust_hot(hot, cold):
  for k in hot:
    hot[k] = hot[k] - cold[k];

def plot(rn, mx, ws_cmd, direction, xlbl):
  cache_data = get_data(mx, "whippersnapper/cache_" + str(rn) + "/" + ws_cmd + "_" + direction + "_res.csv")
  hot_data = get_data(mx, "whippersnapper/hot_cache_" + str(rn) + "/" + ws_cmd + "_" + direction + "_res.csv")
  no_cache_data = get_data (mx, "whippersnapper/no_cache_" + str(rn) + "/" + ws_cmd + "_" + direction + "_res.csv")
  
  adjust_hot(hot_data, cache_data)

  pl.plot_series(cache_data, hot_data, no_cache_data, name = "whippersnapper/" + ws_cmd + "_" + direction, xlabel = xlbl, ylabel = "synthesis time (s)");

cmd = sys.argv[1];
ws_cmd = sys.argv[2] if len(sys.argv) > 2 else None;

if cmd == "generate" and os.path.exists("whippersnapper/" + ws_cmd + "_orig_to_obt_res.csv"):
  print("output file already exists");
  sys.exit();

if cmd == "gen-all":
  rule_num = int(sys.argv[2]);
  max_pl = int(sys.argv[3]);
  max_sf = int(sys.argv[4]);

  run_whippersnapper("pipeline", "cache_" + str(rule_num) , rule_num, max_pl, avenir_flags());
  run_whippersnapper("pipeline", "hot_cache_" + str(rule_num), rule_num, max_pl, hot_start_flags());
  run_whippersnapper("pipeline", "no_cache_" + str(rule_num), rule_num, max_pl, non_cache_flags());

  run_whippersnapper("set-field", "cache_" + str(rule_num), rule_num, max_sf, avenir_flags());
  run_whippersnapper("set-field", "hot_cache_" + str(rule_num), rule_num, max_sf, hot_start_flags());
  run_whippersnapper("set-field", "no_cache_" + str(rule_num), rule_num, max_sf, non_cache_flags());

  plot(rule_num, max_pl, "pipeline", "orig_to_obt", "# of tables");
  plot(rule_num, max_sf, "set-field", "orig_to_obt", "# of fields");
elif cmd == "gen-all-rev":
  rule_num = int(sys.argv[2]);
  max_pl = int(sys.argv[3]);
  max_sf = int(sys.argv[4]);

  run_avenir("pipeline", "cache_" + str(rule_num) , rule_num, max_pl, avenir_flags());
  run_avenir("pipeline", "hot_cache_" + str(rule_num), rule_num, max_pl, hot_start_flags());
  run_avenir("pipeline", "no_cache_" + str(rule_num), rule_num, max_pl, non_cache_flags());

  run_avenir("set-field", "cache_" + str(rule_num), rule_num, max_sf, avenir_flags());
  run_avenir("set-field", "hot_cache_" + str(rule_num), rule_num, max_sf, hot_start_flags());
  run_avenir("set-field", "no_cache_" + str(rule_num), rule_num, max_sf, non_cache_flags());

  plot(rule_num, max_pl, "pipeline", "obt_to_orig", "# of tables");
  plot(rule_num, max_sf, "set-field", "obt_to_orig", "# of fields");


elif cmd == "generate":
  rule_num = int(sys.argv[3]);  
  mx = sys.argv[4];
  flags = non_cache_flags() if "--no-cache" in sys.argv else avenir_flags();
  fldr = "no_cache" if "--no-cache" in sys.argv else "cache";
  run_whippersnapper(ws_cmd, fldr, rule_num, mx, flags);
elif cmd == "eval":
  run_avenir(ws_cmd);
elif cmd == "plot":
  if ws_cmd == "pipeline":
    xlbl = "# of tables"
  else:
    xlbl = "# of fields"
  plot(ws_cmd, xlbl);
