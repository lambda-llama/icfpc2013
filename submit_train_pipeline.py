import time
import os
import sys
import random
import json
import subprocess
from icfpc_requests import *

# def main(id_s, inp):
#     child = subprocess.Popen("./dist/build/bvi/bvi", stdin=subprocess.PIPE, 
#         stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
#     child.stdin.write(bytes(inp, "utf8"))
#     child.stdin.flush()
    
#     args = child.stdout.readline().split()
#     eq_number = int(child.stdout.readline())
#     eqs = [()] * eq_number
#     for eq_i in range(eq_number):
#         eq_id = int(child.stdout.readline()) - 1
#         eq_size = int(child.stdout.readline())
#         eq_output = bytes.decode(child.stdout.readline(), "utf8")
#         eq_output = [int(i, 16) for i in eq_output.split()]
#         eq_funcs = []
#         for i in range(eq_size):
#             foo = bytes.decode(child.stdout.readline(), "utf8")
#             eq_funcs.append(foo)
#         eqs[eq_id] = (eq_output, eq_funcs)
    
#     s = None
#     while not s:
#         s = eval_id(id_s, args)
#         if s:
#             s = [int(i, 16) for i in s["outputs"]]
#             break
#         time.sleep(2)

#     for i, eq in enumerate(eqs):
#         if s == eq[0]:
#             result = None
#             while not result:
#                 result = guess(id_s, eq[1][0])
#                 time.sleep(2)
#             if result["status"] == "win":
#                 print("WIN! %s" % id_s)
#                 break
#             else:
#                 eq_id = i + 1
#                 while result["status"] != "win":
#                     print("Mismatch, another try of %s" % id_s)
#                     vin, vout, _ = [int(jj, 16) for jj in result["values"]]
#                     child.stdin.write(bytes("%i\n%i\n%i\n" % (eq_id, vin, vout), "utf8"))
#                     child.stdin.flush()
#                     r_count = int(child.stdout.readline())
#                     lines = []
#                     for i in range(r_count):
#                         lines.append(bytes.decode(child.stdout.readline(), "utf8"))
#                     result = guess(id_s, lines[0])
#                 print("WIN! %s" % id_s)
#                 break
#     else:
#         print("Loosers!!!")


def main(id_s, inp):
    child = subprocess.Popen("./dist/build/bvi/bvi", stdin=subprocess.PIPE, 
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    child.stdin.write(inp.encode())
    child.stdin.flush()

    filename = child.stdout.readline().decode()
    fd = open(filename, "rt")
    
    args = fd.readline()
    eq_number = fd.readline()
    eqs = [()] * eq_number
    for eq_i in range(eq_number):
        eq_id = int(fd.readline()) - 1
        eq_size = int(fd.readline)
        eq_output = [int(i, 16) for i in fd.readline().split()]
        eq_funcs = []
        for i in range(eq_size):
            eq_funcs.append(fd.readline())
            li += 1
        eqs[eq_id] = (eq_output, eq_funcs)
    fd.close()

    s = None
    while not s:
        s = eval_id(id_s, args)
        if s:
            s = [int(i, 16) for i in s["outputs"]]
            break
        time.sleep(2)
    for i, eq in enumerate(eqs):
        if s == eq[0]:
            result = None
            while not result:
                result = guess(id_s, eq[1][0])
                time.sleep(2)
            if result["status"] == "win":
                print("WIN! %s" % id_s)
                break
            else:
                eq_id = i + 1
                while result["status"] != "win":
                    print("Mismatch, another try of %s" % id_s)
                    vin, vout, _ = [int(jj, 16) for jj in result["values"]]
                    child.stdin.write(("%i\n%i\n%i\n" % (eq_id, vin, vout)).encode())
                    filename = child.stdout.readline().decode()
                    fd = open(filename, "rt")
                    l_count = fd.readline()
                    result = guess(id_s, fd.readline())
                    fd.close()
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
        print(t["operators"], t["id"])
        main(t["id"], inp)
        time.sleep(5)