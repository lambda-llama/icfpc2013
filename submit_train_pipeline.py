import time
import os
import sys
import random
import json
import subprocess
import logging
from icfpc_requests import *


def prepare_now(id_s, inp):
    logging.debug("BEGIN: bvgen for %s" % id_s)
    child = subprocess.Popen("./dist/build/bvgen/bvgen", stdin=subprocess.PIPE, 
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    m_out, m_err = child.communicate(inp.encode())
    logging.debug("bvgen stderr log: %s" % m_err)
    lines = iter(m_out.decode().splitlines())
    logging.debug("END: bvgen for %s" % id_s)
    return lines


def prepare(dirname, predir):
    for id_s in os.listdir(dirname):
        logging.debug("BEGIN: preprocessing for %s" % id_s)
        fd = open(os.path.join(dirname, id_s), "rt")
        inp = fd.readline() + fd.readline().replace("'", "\"")
        # child = subprocess.Popen("./dist/build/bvgen/bvgen", stdin=subprocess.PIPE, 
        #     stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
        # m_out, m_err = child.communicate(inp.encode())
        fd.close()
        lines = prepare_now(id_s, inp)
        fd = open(os.path.join(predir, id_s), "wt")
        # lines = m_out.decode().splitlines()
        for line in lines:
            fd.write(line + "\n")
        fd.close()
        logging.debug("END: preprocessing for %s" % id_s)


def parse_lines(iter_lines):
    logging.debug("BEGIN: parsing preprocessing")
    args = next(iter_lines).split()
    eq_number = int(next(iter_lines))
    eqs = [()] * eq_number
    for eq_i in range(eq_number):
        eq_id = int(next(iter_lines)) - 1
        eq_size = int(next(iter_lines))
        eq_outputs = next(iter_lines).split()
        eq_output = [int(i, 16) for i in eq_outputs]
        eq_hash = sum(eq_output)
        eq_funcs = []
        for i in range(eq_size):
            eq_funcs.append(next(iter_lines))
        eqs[eq_id] = (eq_output, eq_funcs, eq_hash)
    logging.debug("END: parsing preprocessing")
    return args, eqs


def get_outputs(id_s, args):
    logging.debug("BEGIN: getting outputs for %s" % id_s)
    s = None
    while not s:
        s = eval_id(id_s, args)
        if s:
            s = [int(i, 16) for i in s["outputs"]]
            s_hash = sum(s)
            break
    logging.debug("Outputs for %s are: %s" % (id_s, str(s)))
    logging.debug("END: getting outputs for %s" % id_s)
    return s, s_hash


def get_new_funcs_byone(vin, vout, funcs):
    logging.debug("BEGIN: searching [byone] for pair (%i -> %i)" % (vin, vout))
    newfuncs = []
    for variant in funcs:
        child = subprocess.Popen("./dist/build/bvi/bvi", stdin=subprocess.PIPE, 
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
        to_send = variant.strip() + "\n" + str(vin) + "\n"
        m_out, m_err = child.communicate(to_send.encode())
        logging.debug("searching stderr log: %s" % m_err)
        anss = m_out.decode().splitlines()[0]
        if int(anss) == vout:
            newfuncs.append(variant)
    logging.debug("END: searching [byone] for pair (%i -> %i)" % (vin, vout))
    return newfuncs


def get_new_funcs_batch(vin, vout, funcs):
    logging.debug("BEGIN: searching [batch] for pair (%i -> %i)" % (vin, vout))
    newfuncs = []
    to_send = ""
    for variant in funcs:
        to_send += variant.strip() + "\n" + str(vin) + "\n"
        
    child = subprocess.Popen("./dist/build/bvi/bvi", stdin=subprocess.PIPE, 
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    m_out, m_err = child.communicate(to_send.encode())
    anss = m_out.decode().splitlines()
    for i, a in enumerate(anss):
        if a == vout:
            newfuncs.append(funcs[i])
    logging.debug("searching stderr log: %s" % m_err)
    logging.debug("END: searching [batch] for pair (%i -> %i)" % (vin, vout))
    return newfuncs


def get_result(id_s, func):
    result = None
    logging.debug("BEGIN: Geussing '%s' as %s" % (func, id_s))
    while not result:
        result = guess(id_s, func)
        time.sleep(1)
    logging.debug("Guessing result: %s" % str(result))
    logging.debug("END: Geussing '%s' as %s" % (func, id_s))
    return result


def submitter(id_s, inp, loadfrom=None):
    if not loadfrom:
        lines = prepare_now(id_s, inp)
    else:
        fd = open(os.path.join(loadfrom, id_s), "rt")
        lines = iter(fd.readlines())
    args, eqs = parse_lines(lines)
    s, s_hash = get_outputs(id_s, args)
    for i, eq in enumerate(eqs):
        if s_hash == eq[2] and s == eq[0]: # First compare hashes, then deep equality
            # Get result
            result = get_result(id_s, eq[1][0])
            # No mismatch
            if result["status"] == "win":
                print("WIN! %s" % id_s)
                break
            elif result["status"] == "error":
                print("Shit happend!")
                break
            # Mismatch
            else:
                # Save functions to some list
                funcs = eq[1]
                while result["status"] != "win":
                    print("Mismatch, another try %s (size: %i)" % (id_s, len(funcs)))
                    vin, vout, _ = [int(jj, 16) for jj in result["values"]]
                    print("Mismatch point: %i (%s)" % (vin, result["values"][0]))
                    # One func check strategy
                    #funcs = get_new_funcs_byone(vin, vout, funcs)
                    # Batch func check strategy
                    funcs = get_new_funcs_batch(vin, vout, funcs)
                    result = get_result(id_s, funcs[0])
                print("WIN! %s" % id_s)
                break
    else:
        print("Loobsters!!!")
        return False
    return True


def run_prepared(dirname, predir):
    for id_s in os.listdir(dirname):
        if not submitter(id_s, "", predir):
            return
        time.sleep(5)


def run(dirname):
    for id_s in os.listdir(dirname):
        fd = open(os.path.join(dirname, id_s), "rt")
        inp = fd.readline() + fd.readline().replace("'", "\"")
        if not submitter(id_s, inp):
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
        if not submitter(t["id"], inp):
            return
        i += 1
        time.sleep(5)


def test_current(id_s, size, operators):
    inp = "%i\n%s" % (size, str(operators).replace("'", "\""))
    submitter(id_s, inp)

if __name__ == "__main__":
    logging.basicConfig(format=u'%(filename)s[LINE:%(lineno)d]# %(levelname)-8s [%(asctime)s]  %(message)s',
                    level=logging.DEBUG, filename="submitting.log")
    #prepare("problems", "preprocessed")
    #run_prepared("problems", "preprocessed")
    #run("problems")
    train(9)
    #test_current("w9a79P3FCt71KojxbGHw0HLH", 9, ["and", "if0", "shr4"])