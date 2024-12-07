example_stacks = ['NZ', 'DCM', 'P']
stacks = ['CSGB', 'GVNJHWMT', 'SQM', 'MNWTLSB', 'PWGVTFZJ', 'SHQGBTC', 'WBPJT', 'MQTFZCDG', 'FPBHSN']
stk = stacks

def move_stack(s, N, oi, di):
    c = s[oi][0:N]
    s[oi] = s[oi][N:]
    s[di] = c + s[di]
    return s

with open("input.txt") as f:
    entries = [line.rstrip() for line in f]

for e in entries:
    move, amt, frm, o_stack, t, d_stack = e.split(' ')
    o_idx = int(o_stack) - 1  # index of origin stack
    d_idx = int(d_stack) - 1  # index of destination stack
    N = int(amt)              # number of times to move

    stk = move_stack(stk, N, o_idx, d_idx)

for s in stk:
    if s:
        print(s[0])
    else:
        print(' ')