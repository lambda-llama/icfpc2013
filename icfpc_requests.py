__author__ = 'mactep'

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