RIGHTS = ['shl1', 'shr1', 'shr4', 'shr16', 'fold', 'tfold']

def find_lefts():
    fd = open("problems.txt", "rt")
    result = []
    for line in fd:
        for pattern in RIGHTS:
            if pattern in line:
                break
        else:
            result.append(line)
    return result
