import time
import os
import sys
import random
import json
import subprocess
from icfpc_requests import *


def prepare_now(id_s, inp):
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
    return eqs

def main(id_s, inp, loadfrom=None):
    # child = subprocess.Popen("./dist/build/bvgen/bvgen", stdin=subprocess.PIPE, 
    #     stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    # m_out, m_err = child.communicate(inp.encode())
    # lines = iter(m_out.decode().splitlines())
    
    if not loadfrom:
        lines = prepare_now(id_s, inp)
    else:
        fd = open(os.path.join(loadfrom, id_s), "rt")
        lines = iter(fd.readlines())

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
                    print("Mismatch, another try %s (size: %i)" % (id_s, len(lines)))
                    vin, vout, _ = [int(jj, 16) for jj in result["values"]]
                    print("Mismatch point: %i" % vin)
                    newlines = []
                    for variant in lines:
                        child = subprocess.Popen("./dist/build/bvi/bvi", stdin=subprocess.PIPE, 
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
                        to_send = variant.strip() + "\n" + str(vin) + "\n"
                        m_out, m_err = child.communicate(to_send.encode())
                        anss = m_out.decode().splitlines()[0]
                        if int(anss) == vout:
                            newlines.append(variant)
                    lines = newlines
                    result = guess(id_s, lines[0])
                    while not result:
                        time.sleep(2)
                        result = guess(id_s, lines[0])
                print("WIN! %s" % id_s)
                break
    else:
        print("Loobsters!!!")
        return False
    return True

def prepare(dirname, predir):
    for id_s in os.listdir(dirname):
        fd = open(os.path.join(dirname, id_s), "rt")
        inp = fd.readline() + fd.readline().replace("'", "\"")
        child = subprocess.Popen("./dist/build/bvgen/bvgen", stdin=subprocess.PIPE, 
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
        m_out, m_err = child.communicate(inp.encode())
        fd.close()
        fd = open(os.path.join(predir, id_s), "wt")
        lines = m_out.decode().splitlines()
        for line in lines:
            fd.write(line + "\n")
        fd.close()


def run_prepared(dirname, predir):
    for id_s in os.listdir(dirname):
        #fd = open(os.path.join(dirname, id_s), "rt")
        #inp = fd.readline() + fd.readline().replace("'", "\"")
        if not main(id_s, "", predir):
            return
        time.sleep(5)


def run(dirname):
    for id_s in os.listdir(dirname):
        fd = open(os.path.join(dirname, id_s), "rt")
        inp = fd.readline() + fd.readline().replace("'", "\"")
        if not main(id_s, inp):
            return
        time.sleep(5)

def train(k):
    i = 1
    while True:
        print("Test:", i)
        t = get_train(k)
        if t == None:
            continue
        inp = str(t["size"]) + "\n"
        inp += str(t["operators"]).replace("'", "\"") + "\n"
        print(t["operators"], t["id"])
        print(t["challenge"])
        if not main(t["id"], inp):
            return
        i += 1
        time.sleep(5)

if __name__ == "__main__":
    #prepare("problems", "preprocessed")
    run_prepared("problems", "preprocessed")
    #run("problems")
    #train(11)
    # id_s = "PfTVWO3WTdUXIVG6Bo0OGRFO"
    # inp = "9\n[\"and\", \"if0\", \"shr16\"]\n"
    # main(id_s, inp)