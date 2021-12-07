# %%

# %%


fn = 'input.txt'


def p_point(s):
    a,b = s.split(',')
    return int(a),int(b)

def read_file(fn):
    lines = []
    for l in open(fn).read().split('\n'):
        if not l: continue
        a,_,b = l.split()
        lines.append((p_point(a),p_point(b)))
    return lines
        
data = read_file(fn)

# %%
horiz = []
vert = []
max_x = max_y = 0
for l in data:
    ((x1,y1),(x2,y2)) = l
    max_x = max(max_x, x1, x2)
    max_y = max(max_y, y1, y2)
    if x1 == x2: horiz.append(l)
    elif y1 == y2: vert.append(l)
    else: continue

max_x += 1
max_y += 1
    
        

# %%
mem = [0] * (max_x*max_y)
print(len(mem),'cells')

def incr(x,y): 
    mem[x + y*max_x] += 1

for l in data:
    ((x1,y1),(x2,y2)) = l
    if x1 == x2:
        if y2 < y1:
            y1,y2 = y2,y1
        for y in range(y1,y2+1):
            incr(x1,y)
    elif y1 == y2:
        if x2 < x1:
            x1,x2 = x2, x1
        for x in range(x1,x2+1):
            incr(x,y1)
        
     

len([x for x in mem if x > 1])

# %%
mem = [0] * (max_x*max_y)
print(len(mem),'cells')

def incr(x,y): 
    mem[x + y*max_x] += 1

def sign(a,b):
    if a == b: return 0
    if a < b: return 1
    return -1

for l in data:
    ((x1,y1),(x2,y2)) = l
    dx = sign(x1,x2)
    dy = sign(y1,y2)
    while x1 != x2 or y1 != y2:
        incr(x1,y1)
        x1 += dx
        y1 += dy
    incr(x1,y1)
    
len([x for x in mem if x > 1])
        

# %%
