import time

class part:
    def __init__(self) -> None:
        self.x = [1, 4000]
        self.m = [1, 4000]
        self.a = [1, 4000]
        self.s = [1, 4000]

def intersect(p, q):
    v = part()
    v.x = [max(p.x[0], q.x[0]), min(p.x[1], q.x[1])]
    v.m = [max(p.m[0], q.m[0]), min(p.m[1], q.m[1])]
    v.a = [max(p.a[0], q.a[0]), min(p.a[1], q.a[1])]
    v.s = [max(p.s[0], q.s[0]), min(p.s[1], q.s[1])]
    return v

class procSingle:
    def __init__(self, v, thresh, lt, nxt) -> None:
        self.v = v
        self.thresh = thresh
        self.lt = lt
        self.nxt = nxt
    
    def predict(self, p):
        q = part()
        if self.v == 'x' and self.lt == False:
            q.x = [self.thresh+1, 4000]
        if self.v == 'x' and self.lt == True:
            q.x = [1, self.thresh-1]
        if self.v == 'm' and self.lt == False:
            q.m = [self.thresh+1, 4000]
        if self.v == 'm' and self.lt == True:
            q.m = [1, self.thresh-1]
        if self.v == 'a' and self.lt == False:
            q.a = [self.thresh+1, 4000]
        if self.v == 'a' and self.lt == True:
            q.a = [1, self.thresh-1]
        if self.v == 's' and self.lt == False:
            q.s = [self.thresh+1, 4000]
        if self.v == 's' and self.lt == True:
            q.s = [1, self.thresh-1]
        return intersect(p, q)

class proc:
    def __init__(self, name, steps, dflt) -> None:
        self.name = name
        self.steps = steps
        self.dflt = dflt
    
    def backstep(self, plist):
        qlist = []
        for s in self.steps:
            for p in plist:
                q = s.predict(p)
                if q.x[0] <= q.x[1] and q.m[0] <= q.m[1] and q.a[0] <= q.a[1] and q.s[0] <= q.s[1]:
                    qlist.append(q)
        return qlist


with open("example.txt") as f:
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
