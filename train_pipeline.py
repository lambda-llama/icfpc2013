import os
import sys
import json
import subprocess
from icfpc_requests import *


def main():
	size = int(sys.argv[1])

	t = get_train(size)
	#t = {"size": 4, "operators": ['and', 'shr1'], "id": "1111111111111111111111"}
	print(json.dumps(t))

	s = str(t["size"]) + "\n"
	s += str(t["operators"]).replace("'", "\"") + "\n"
	child = subprocess.Popen("./dist/build/bvi/bvi > train/%s" % t["id"], stdin=subprocess.PIPE, 
		stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
	child.communicate(bytes(s, "utf8"))
	r, s = find_solution("train", t["id"])
	os.unlink("train/%s" % t["id"])

if __name__ == "__main__":
	main()