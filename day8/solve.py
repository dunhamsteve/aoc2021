# %%
from collections import defaultdict

fn = 'input.txt'
# fn = 'eg1.txt'

hist = defaultdict(lambda:0)

for l in list(open(fn,)):
    ps = l.split()
    i = ps.index('|')
    a = ps[:i]
    b = ps[i+1:]
    for x in b:
        hist[len(x)] += 1
        
    
hist[2] + hist[3] + hist[4] + hist[7]
# %%

def sub(x,y):
    return len(set(x)-set(y))

def parse(chunk):
    pass
    

def infer(line):
    ps = line.split()
    i = ps.index('|')
    a = ps[:i]
    b = ps[i+1:]
    all = a + b
    state = {c:127 for c in 'abcdef'}
    acc = 0
    
    data = {}
    for y in all:
        l = len(y)
        if l == 2: data[1] = y
        elif l == 3: data[7] = y
        elif l == 4: data[4] = y
        elif l == 7: data[8] = y
    
    four = set(data[4])
    one = set(data[1])

    acc = 0
    for y in b:
        l = len(y)
        m1 = len(set(y)-one) 
        m4 = len(set(y)-four)
        if l == 2: v = 1
        if l == 3: v = 7
        if l == 4: v = 4
        if l == 5: # 2 3 5
            if   m1 == 3: v = 3
            elif m4 == 3: v = 2
            elif m4 == 2: v = 5
            else:
                assert f'err {y,a,b,four,one}'
        if l == 6: # 0 5 9
            if   m1 == 5: v = 6
            elif m4 == 3: v = 0
            elif m4 == 2: v = 9
            else: 
                assert f'err2 {y,a,b,four,one}'
        if l == 7: v = 8
        acc = acc * 10 + v
    return acc

def run():
    s=0
    for l in list(open(fn)):
        x = infer(l)
        s += x
        print(x)
    print('total',s)
run()
# %%
