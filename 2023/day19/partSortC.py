import time
import copy

class part:
    def __init__(self) -> None:
        self.x = [1, 4000]
        self.m = [1, 4000]
        self.a = [1, 4000]
        self.s = [1, 4000]

    def __repr__(self):
        return f"x:{self.x} m:{self.m} a:{self.a} s:{self.s}"

    def __str__(self):
        return f"x:{self.x} m:{self.m} a:{self.a} s:{self.s}"

    def pprod(self):
        return (self.x[1]-self.x[0]+1)*(self.m[1]-self.m[0]+1)*(self.a[1]-self.a[0]+1)*(self.s[1]-self.s[0]+1)

class procSingle:
    def __init__(self, v, thresh, lt, nxt) -> None:
        self.v = v
        self.thresh = thresh
        self.lt = lt
        self.nxt = nxt
    
    def apply(self, p):
        if self.v == 'x' and self.lt == False and (p.x[0] > self.thresh or p.x[1] > self.thresh):
            q = copy.deepcopy(p)
            if p.x[0] <= self.thresh:
                q.x[0] = self.thresh+1
                p.x[1] = self.thresh
            else:
                p.x[0] = 0
                p.x[1] = -1
            return [self.nxt, q]
        if self.v == 'x' and self.lt == True and (p.x[0] < self.thresh or p.x[1] < self.thresh):
            q = copy.deepcopy(p)
            if p.x[1] >= self.thresh:
                q.x[1] = self.thresh-1
                p.x[0] = self.thresh
            else:
                p.x[0] = 0
                p.x[1] = -1
            return [self.nxt, q]
        if self.v == 'm' and self.lt == False and (p.m[0] > self.thresh or p.m[1] > self.thresh):
            q = copy.deepcopy(p)
            if p.m[0] <= self.thresh:
                q.m[0] = self.thresh+1
                p.m[1] = self.thresh
            else:
                p.m[0] = 0
                p.m[1] = -1
            return [self.nxt, q]
        if self.v == 'm' and self.lt == True and (p.m[0] < self.thresh or p.m[1] < self.thresh):
            q = copy.deepcopy(p)
            if p.m[1] >= self.thresh:
                q.m[1] = self.thresh-1
                p.m[0] = self.thresh
            else:
                p.m[0] = 0
                p.m[1] = -1
            return [self.nxt, q]
        if self.v == 'a' and self.lt == False and (p.a[0] > self.thresh or p.a[1] > self.thresh):
            q = copy.deepcopy(p)
            if p.a[0] <= self.thresh:
                q.a[0] = self.thresh+1
                p.a[1] = self.thresh
            else:
                p.a[0] = 0
                p.a[1] = -1
            return [self.nxt, q]
        if self.v == 'a' and self.lt == True and (p.a[0] < self.thresh or p.a[1] < self.thresh):
            q = copy.deepcopy(p)
            if p.a[1] >= self.thresh:
                q.a[1] = self.thresh-1
                p.a[0] = self.thresh
            else:
                p.a[0] = 0
                p.a[1] = -1
            return [self.nxt, q]
        if self.v == 's' and self.lt == False and (p.s[0] > self.thresh or p.s[1] > self.thresh):
            q = copy.deepcopy(p)
            if p.s[0] <= self.thresh:
                q.s[0] = self.thresh+1
                p.s[1] = self.thresh
            else:
                p.s[0] = 0
                p.s[1] = -1
            return [self.nxt, q]
        if self.v == 's' and self.lt == True and (p.s[0] < self.thresh or p.s[1] < self.thresh):
            q = copy.deepcopy(p)
            if p.s[1] >= self.thresh:
                q.s[1] = self.thresh-1
                p.s[0] = self.thresh
            else:
                p.s[0] = 0
                p.s[1] = -1
            return [self.nxt, q]
        return ['nullstep', None]

class proc:
    def __init__(self, name, steps, dflt) -> None:
        self.name = name
        self.steps = steps
        self.dflt = dflt
    
    def apply(self, p):
        qlist = []
        qd = copy.deepcopy(p)
        for s in self.steps:
            [nxt, q] = s.apply(qd)
            if nxt == 'nullstep':
                continue
            if q.x[0] <= q.x[1] and q.m[0] <= q.m[1] and q.a[0] <= q.a[1] and q.s[0] < q.s[1]:
                qlist.append([nxt, q])
        if qd.x[0] <= qd.x[1] and qd.m[0] <= qd.m[1] and qd.a[0] <= qd.a[1] and qd.s[0] < qd.s[1]:
            qlist.append([self.dflt, qd])
        return qlist


with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

start = time.time()

procs = dict()
for e in entries:
    if e == '':
        break
    [name, lprocs] = e.split('{')
    steps = lprocs.split(',')
    p = proc(name, [], 'R')
    for s in steps:
        if ':' not in s:
            p.dflt = s[:-1]
        else:
            [eqt, nxt] = s.split(':')
            if '>' in eqt:
                [v, thresh] = eqt.split('>')
                p.steps.append( procSingle(v, int(thresh), False, nxt) )
            else:
                [v, thresh] = eqt.split('<')
                p.steps.append( procSingle(v, int(thresh), True, nxt) )
    procs[name] = p

res = 0
plist = [ ['in', part()] ]
while len(plist) > 0:
    nxt_plist = []
    for p in plist:
        qlist = procs[p[0]].apply(p[1])
        for q in qlist:
            if q[0] == 'A':
                res += q[1].pprod()
            elif q[0] != 'R':
                nxt_plist.append(q)
    plist = nxt_plist

print(res)

print()
print("-------")
end = time.time()
print(end - start)
