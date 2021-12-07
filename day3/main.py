# %%

fn = 'input.txt'

lines = [l.strip() for l in list(open(fn))]

ln = len(lines[0])
# %%
gamma = 0
epsilon = 0
for i in range(ln):
    acc = 0
    for l in lines:
        if l[i] == "1":
            acc += 1
        else:
            acc -=1
    gamma = gamma * 2
    epsilon = epsilon * 2
    if acc > 0:
        gamma += 1
    else:
        epsilon += 1
gamma*epsilon

# %%
from collections import defaultdict

tmp = lines


for i in range(ln):
    acc = defaultdict(list)
    for l in tmp: acc[l[i]].append(l)
    if len(acc["1"]) >= len(acc["0"]):
        tmp = acc["1"]
    else:
        tmp = acc["0"]
    if len(tmp) == 1: break
x, = tmp
oxy = int(x,2)

oxy


# %%
tmp = lines

for i in range(ln):
    acc = defaultdict(list)
    for l in tmp: acc[l[i]].append(l)
    if len(acc["1"]) >= len(acc["0"]):
        tmp = acc["0"]
    else:
        tmp = acc["1"]
    if len(tmp) == 1: break
x, = tmp
co2 = int(x,2)
co2
# %%
co2*oxy
# %%
