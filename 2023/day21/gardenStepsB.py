import time

with open("input.txt") as f:
    m = [line.rstrip() for line in f]
M = len(m[0]) # map is square, equal in each dimension

start = time.time()

mps = ['center', 'nw', 'ne', 'sw', 'se']
dst = dict()
for mp in mps:
    with open(mp+".txt") as f:
        entries = [line.rstrip() for line in f]
    dst[mp] = []
    for e in entries:
        dst[mp].append(list())
        dst[mp][-1] = [int(x) for x in e.split(', ')]

corners = [0, 0, 0, 0]
corners[0] = dst["center"][0][0]
corners[1] = dst["center"][0][M-1]
corners[2] = dst["center"][M-1][0]
corners[3] = dst["center"][M-1][M-1]
print("Corner distances:")
print(corners)

max_dist = 0
for r in range(M):
    for c in range(M):
        if dst["center"][r][c] != M*M:
            max_dist = max(max_dist, dst["center"][r][c])
print("Max steps to fill from center:")
print(max_dist)

num_odd = 0
num_even = 0
for k, v in dst.items():
    num_odd = 0
    num_even = 0
    for r in range(M):
        for c in range(M):
            if v[r][c] != M*M:
                if v[r][c] % 2 == 0:
                    num_even += 1
                else:
                    num_odd += 1
    print(k + ": even (" + str(num_even) + "), odd (" + str(num_odd) + ")")
    if k == 'center':
        print()
        continue
    max_dist = 0
    for r in range(M):
        for c in range(M):
            if v[r][c] != M*M:
                max_dist = max(max_dist, v[r][c])
    print("Max steps to fill from " + k)
    print(max_dist)

# by manual inspection of the output computed above,
# the following is true:
#   (1) the distance from the center to each corner is (M-1)
#   (2) the fill time from any corner to all other places on the map is (M-1)*2
#   (3) the number of even and odd steps is identical from any corner or center

T = 26501365 # steps the elf takes, direct from problem page
P = (T-corners[0])//M # number of full plots that are passed
rem = (T-corners[0])%M # number of steps remaining to be taken after last garden plot reached
print()
print("M: " + str(M) + " P: " + str(P) + " rem: " + str(rem))
print()

# build double-point maps
mps = ['ne-nw', 'ne-se', 'nw-sw', 'se-sw', 'ne-sw', 'nw-se']
for mp in mps:
    dst[mp] = [ [M*M]*M for k in range(M) ]
    [a, b] = mp.split("-")
    for r in range(M):
        for c in range(M):
            dst[mp][r][c] = min(dst[a][r][c], dst[b][r][c])
    max_dist = 0
    for r in range(M):
        for c in range(M):
            if dst[mp][r][c] != M*M:
                max_dist = max(max_dist, dst[mp][r][c])
    print("Max steps to fill from " + mp)
    print(max_dist)


# by manual inspection of the output computed above,
# the additional following is true:
#   (4) remainder steps is (M-1)/2 + 1
#   (5) only (M-1) + (M-1)/2 steps are needed to fill the previous plot
#   (6) therefore - only "edge" plots need to be considered
#   (7) T is odd, so the center plot should use odd coverage
#   (8) P is odd, so all edge plots are odd
#   (9) number of plots p steps away from center is p*4
#  (10) so there are (P+1)*4 edge plots
#  (11) but 4 are end points (n,s,e,w) and there are 4 diagonals
#  (12) therefore - each diagonal edge has P plots

# interior plots
res = num_odd # start with center plot locations available
for p in range(P):
    if p % 2 == 0:
        res += 4*(p+1)*num_even
    else:
        res += 4*(p+1)*num_odd

# end caps
mps = ['ne-nw', 'ne-se', 'nw-sw', 'se-sw']
for mp in mps:
    for r in range(M):
        for c in range(M):
            if dst[mp][r][c] <= rem and dst[mp][r][c] % 2 == 1:
                res += 1

# edges
mps = ['ne', 'nw', 'se', 'sw']
for mp in mps:
    for r in range(M):
        for c in range(M):
            if dst[mp][r][c] <= (rem+M) and dst[mp][r][c] % 2 == 1:
                res += P

print()
print(res)

print()
print("-------")
end = time.time()
print(end - start)

# 609294306282603 is too low
# 609294372636995 is too low
# 609298026763512 is too low
# 609298026764036 no
# 609298106065040
