import time
import os
import sys
import random
import json
import subprocess
from icfpc_requests import *


def main(id_s):
	child = subprocess.Popen("./dist/build/bvi/bvi < problems/%s > train/%s" % (id_s, id_s), stdin=subprocess.PIPE, 
		stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
	child.communicate()
	r, s, flag = find_solution("train", id_s)
	os.unlink("train/%s" % id_s)

if __name__ == "__main__":
	for f in os.listdir(sys.argv[1]):
		main(f)
		time.sleep(5)