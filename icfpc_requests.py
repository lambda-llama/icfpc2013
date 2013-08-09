__author__ = 'mactep'

import os
import time
import requests
import json

MY_TOKEN = "0047qni4Jv0ErZcl53WtPweKjim6oLQ6dqDLc9SzvpsH1H"


def send_request(request, d):
    r = requests.post("http://icfpc2013.cloudapp.net/{0}?auth={1}".format(request, MY_TOKEN), data=json.dumps(d))
    if r.status_code != 200:
        print("Error %i: %s" % (r.status_code, r.text))
        return
    return r.json()


def status():
    return send_request("status", {})


def get_problem():
    return send_request("myproblems", {})


def eval_id(id_s, args):
    return send_request("eval", {"id": id_s, "arguments": args})


def eval_program(program, args):
    return send_request("eval", {"program": program, "arguments": args})


def guess(id_s, program):
    return send_request("guess", {"id": id_s, "program": program})


def get_train(size=None, operators=None):
    d = {}
    if size:
        d['size'] = size
    if operators:
        d['operators'] = operators
    return send_request("train", d)


def dump_problems_list():
    d = get_problem()
    d = sorted(d, key=(lambda i: 0 if "solved" in i and i["solved"] == True else i["size"]))
    fd = open("problems.txt", "wt")
    for p in d:
        if "solved" in p and p["solved"] == True:
            fd.write("#")
        fd.write(json.dumps(p) + "\n")
    fd.close()


def load(size=None):
    fd = open("problems.txt", "rt")
    r = []
    for line in fd:
        if line.startswith("#"):
            continue
        p = json.loads(line)
        if "solved" in p:
            continue
        if not size:
            size = p["size"]
        if p["size"] != size:
            break
        r.append((p["id"], p["size"], p["operators"]))
    fd.close()
    return r


def solve_3():
    fd = open("problems.txt", "rt")
    r = []
    for line in fd:
        if line.startswith("#"):
            continue
        p = json.loads(line)
        if "solved" in p:
            continue
        if p["size"] > 3:
            break
        r.append((p["id"], "(lambda (x) (%s x))" % p["operators"][0], p))
    fd.close()
    return r


def generate_inputs(out, size=None):
    results = load(size)
    for id_s, size, oper in results:
        fd = open(os.path.join(out, id_s), "wt")
        fd.write(str(size) + "\n")
        fd.write(str(oper).replace("'", "\"") + "\n")
        fd.close()

def find_solution(sdir, id_s):
    flag = True
    fd = open(os.path.join(sdir, id_s), "rt")
    leftnumber = int(fd.readline())
    args = fd.readline().split()
    r = []
    for line in fd:
        if line.startswith("("):
            r.append(line)
        else:
            r.append(line.split())
    s = [i.lower() for i in eval_id(id_s, args)["outputs"]]
    for i, c in enumerate(r):
        if not i % 2:
            continue
        for j in range(len(c)):
            if int(c[j], 16) != int(s[j], 16):
                break
        else:
            result = guess(id_s, r[i-1])
            if not result or isinstance(result, str):
                time.sleep(5)
                result = guess(id_s, r[i-1])
            print(result)
            if result["status"] != "win":
                continue
            break
    else:
        flag = false
    fd.close()
    return r, s, flag