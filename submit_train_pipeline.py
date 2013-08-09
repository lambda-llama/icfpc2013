import time
import os
import sys
import random
import json
import subprocess
from icfpc_requests import *


def main(id_s, inp):
    child = subprocess.Popen("./dist/build/bvi/bvi", stdin=subprocess.PIPE, 
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    m_out, m_err = child.communicate(bytes(inp, "utf8"))
    lines = str(m_out)[2:-1].split(r"\n")
    li = 0
    args = lines[li].split()
    li += 1
    eq_number = int(lines[li])
    li += 1
    eqs = [()] * eq_number
    for eq_i in range(eq_number):
        eq_id = int(lines[li]) - 1
        li += 1
        eq_size = int(lines[li])
        li += 1
        eq_output = [int(i, 16) for i in lines[li].split()]
        li += 1
        eq_funcs = []
        for i in range(eq_size):
            eq_funcs.append(lines[li])
            li += 1
        eqs[eq_id] = (eq_output, eq_funcs)

    s = None
    while not s:
        s = eval_id(id_s, args)
        if s:
            s = [int(i, 16) for i in s["outputs"]]
            break
    for i, eq in enumerate(eqs):
        if s == eq[0]:
            result = None
            while not result:
                result = guess(id_s, eq[1][0])
            if result["status"] == "win":
                print("WIN! %s" % id_s)
                break
            else:
                eq_id = i + 1
                while result["status"] != "win":
                    print("Mismatch, another try")
                    vin, vout, _ = [int(jj, 16) for jj in result["values"].split()]
                    m_out, m_err = child.communicate(bytes("%i\n%i\n%i\n" % (eq_id, vin, vout), "utf8"))
                    lines = str(m_out)[2:-1].split(r"\n")
                    result = guess(id_s, lines[0])
                print("WIN! %s" % id_s)
                break
    else:
        print("Loosers!!!")

if __name__ == "__main__":
    while True:
        t = get_train(8)
        if t == None:
            continue
        inp = str(t["size"]) + "\n"
        inp += str(t["operators"]).replace("'", "\"") + "\n"
        print(t["operators"])
        main(t["id"], inp)
        time.sleep(5)