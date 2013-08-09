import time
import os
import sys
import random
import json
import subprocess
from icfpc_requests import *


def main(id_s, inp):
    child = subprocess.Popen("./dist/build/bvgen/bvgen", stdin=subprocess.PIPE, 
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    m_out, m_err = child.communicate(inp.encode())
    lines = iter(m_out.decode().splitlines())
    args = next(lines).split()
    eq_number = int(next(lines))
    eqs = [()] * eq_number
    for eq_i in range(eq_number):
        eq_id = int(next(lines)) - 1
        eq_size = int(next(lines))
        eq_outputs = next(lines).split()
        eq_output = [int(i, 16) for i in eq_outputs]
        eq_funcs = []
        for i in range(eq_size):
            eq_funcs.append(next(lines))
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
                lines = eq[1]
                while result["status"] != "win":
                    print("Mismatch, another try %s" % id_s)
                    vin, vout, _ = [int(jj, 16) for jj in result["values"]]
                    
                    newlines = []
                    for variant in lines:
                        child = subprocess.Popen("./dist/build/bvi/bvi", stdin=subprocess.PIPE, 
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
                        to_send = variant + "\n" + str(vin) + "\n"
                        m_out, m_err = child.communicate(to_send.encode())
                        anss = m_out.decode().splitlines()[0]
                        if int(anss) == vout:
                            newlines.append(variant)
                    lines = newlines
                    result = guess(id_s, lines[0])
                print("WIN! %s" % id_s)
                break
    else:
        print("Loobsters!!!")
        return False
    return True

def run(dirname):
    for id_s in os.listdir(dirname):
        fd = open(os.path.join(dirname, id_s), "rt")
        inp = fd.readline() + fd.readline().replace("'", "\"")
        if not main(id_s, inp):
            return
        time.sleep(5)

def train(k):
    while True:
        t = get_train(k)
        if t == None:
            continue
        inp = str(t["size"]) + "\n"
        inp += str(t["operators"]).replace("'", "\"") + "\n"
        print(t["operators"], t["id"])
        if not main(t["id"], inp):
            return
        time.sleep(5)

if __name__ == "__main__":
    #run("problems")
    train(9)