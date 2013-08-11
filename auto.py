import os
import sys
import shutil
import subprocess

from icfpc_requests import dump_problems_list, generate_inputs_10

dump_problems_list()

inp_dir = "hack_problems"
n_proc = sys.argv[1] if len(sys.argv) > 1 else 4

shutil.rmtree(inp_dir)

if not os.path.exists(inp_dir):
    os.mkdir(inp_dir)

for size in range(27, 44):
    generate_inputs_10(inp_dir, size)
print("auto:   generated input file")

for f in os.listdir(inp_dir):
    f = os.path.join(inp_dir, f)
    print("auto:   starting " + f)
    bvs = "./dist/build/bvs/bvs +RTS -N{} < {}".format(n_proc, f)
    p = subprocess.Popen(bvs, stdin=subprocess.PIPE, stdout=sys.stdout, stderr=sys.stderr, shell=True)
    ret = p.wait()
    print("auto:    terminating " + f + " with ret " + str(ret))
    
    
    
