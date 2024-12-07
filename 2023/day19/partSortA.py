import time

class part:
    def __init__(self, x, m, a, s) -> None:
        self.x = x
        self.m = m
        self.a = a
        self.s = s

    def psum(self) -> int:
        return self.x + self.m + self.a + self.s

class procSingle:
    def __init__(self, v, thresh, lt, nxt) -> None:
        self.v = v
        self.thresh = thresh
        self.lt = lt
        self.nxt = nxt
    
    def apply(self, p):
        if self.v == 'x' and self.lt == False and p.x > self.thresh:
            return self.nxt
        if self.v == 'x' and self.lt == True and p.x < self.thresh:
            return self.nxt
        if self.v == 'm' and self.lt == False and p.m > self.thresh:
            return self.nxt
        if self.v == 'm' and self.lt == True and p.m < self.thresh:
            return self.nxt
        if self.v == 'a' and self.lt == False and p.a > self.thresh:
            return self.nxt
        if self.v == 'a' and self.lt == True and p.a < self.thresh:
            return self.nxt
        if self.v == 's' and self.lt == False and p.s > self.thresh:
            return self.nxt
        if self.v == 's' and self.lt == True and p.s < self.thresh:
            return self.nxt
        return 'nullstep'

class proc:
    def __init__(self, name, steps, dflt) -> None:
        self.name = name
        self.steps = steps
        self.dflt = dflt
    
    def apply(self, p):
        for s in self.steps:
            nxt = s.apply(p)
            if nxt == 'nullstep':
                continue
            return nxt
        return self.dflt


with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

start = time.time()

parseParts = False
parts = []
procs = dict()
for e in entries:
    if e == '':
        parseParts = True
        continue
    if parseParts:
        pts = e[1:-1].split(',')
        p = part(0, 0, 0, 0)
        for pt in pts:
            [typ,v] = pt.split('=')
            if typ == 'x':
                p.x = int(v)
            if typ == 'm':
                p.m = int(v)
            if typ == 'a':
                p.a = int(v)
            if typ == 's':
                p.s = int(v)
        parts.append(p)
    else:
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
for p in parts:
    pname = 'in'
    while pname != 'R' and pname != 'A':
        pname = procs[pname].apply(p)
    if pname == 'A':
        res += p.psum()

print(res)

print()
print("-------")
end = time.time()
print(end - start)
