# %%

fn = 'input.txt'
nums = list(map(int,open(fn).read().split(',')))
mx = max(nums)
mn = min(nums)

solution = None
for i in range(mn,mx):
    x = 0
    for n in nums:
        x += abs(n-i)
    # print(i,x)
    if not solution or solution > x: solution = x
solution

# %%
solution = None
for i in range(mn,mx):
    x = 0
    for n in nums:
        b = abs(n-i)
        x += b * (b+1) // 2
    # print(i,x)
    if not solution or solution > x: solution = x
solution
# %%
