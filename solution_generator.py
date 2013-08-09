import icfpc_requests


def generate_inputs(size=None):
	results = icfpc_requests.load(size)
	for id_s, size, oper in results:
		fd = open(id_s, "wt")
		fd.write(str(size) + "\n")
		fd.write(str(oper) + "\n")
		fd.close()