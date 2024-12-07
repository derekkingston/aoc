example_stacks = ['NZ', 'DCM', 'P']
stacks = ['CSGB', 'GVNJHWMT', 'SQM', 'MNWTLSB', 'PWGVTFZJ', 'SHQGBTC', 'WBPJT', 'MQTFZCDG', 'FPBHSN']
stk = stacks

def move_stack(s, oi, di):
    c = s[oi][0]
    s[oi] = s[oi][1:]
    s[di] = c + s[di]
    return s

with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

for e in entries:
    move, amt, frm, o_stack, t, d_stack = e.split(' ')
    o_idx = int(o_stack) - 1  # index of origin stack
    d_idx = int(d_stack) - 1  # index of destination stack
    N = int(amt)              # number of times to move

    for n in range(N):
        stk = move_stack(stk, o_idx, d_idx)

for s in stk:
    print(s[0])