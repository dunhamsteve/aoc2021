
# Advent of Code 2021

This year I'm attempting AoC in Idris2, which I don't really know. A couple of days I fell back to Python for expediency or did Python first so I could see the full puzzle first, and work on the Idris solution at my leisure. The challenge here is that I don't know the language, don't know the libraries, and I've got a couple of decades of imperative programming behind me. 



## Notes on Individual Days

(backfilled on day 11)

**Day 1**

- Learned how to read files

**Day 2**

**Day 3**

- Bailed for python
- Went back and redid in Idris

**Day 4**

- Looks like I started to switch to python (beginnings of a parser) then switched back

**Day 5**

- I did this in python between parts 1/2
- Decided to take the dx/dy approach while in python land
- I changed the code for part two and left instructions to make the part 2 solution work for part 1

**Day 7**

- I think I did python first

**Day 8**

- Looks like I did it in python first and then Idris
- This was the day I had issues trying to figure out Bitset.bit and ended up with a case statement instead.

**Day 9**

**Day 10**

**Day 11**

**Day 12**

**Day 13**

Had a weird issue I don't understand where (Int,Int) was a (Type,Type) ratherthan a Type.

**Day 15**

Initial version is a little slow, but works. I'll probably revisit and get rid of the sorted maps.  There also was
a choice to be $O(n^2)$ instead of $O(n \log n)$, because I didn't want to take the time to write Heap.

    ./build/exec/day15  2127.84s user 22.29s system 99% cpu 35:59.38 total

Using a second SortedMap for the unvisited nodes: (score,Point) -> ()

    ./build/exec/day15  2.77s user 0.05s system 99% cpu 2.836 total

**Day 16**

Spent way too much time tracking down < 16 vs <= 16. Other than that pretty straightforward.  For fun, I decided to write a little combinator parser instead of using Data.String.Parser.  The "Show Packet" was a little bit of a pain because it had to be total, so I had to work in a Nat for depth.

**Day 17**

I spent some time rearranging stuff to make it perform better, turns out the performance issue might have been the `nub` I was doing at the end. 

**Day 18**

Initially I parsed into a tree, but it was much easier to implement the normalization rules on a list of tokens.

**Day 19**

This one was a bit of a slog. Dunno if I have the best solution, but it's fast, if complicated. I decided that an invariant would be the vectors between points, with some normalization that is described at the head of `day19/Main.idr`.

**Day 20**

The trick here was that an infinite number of cells change in problem given in input, but not the example.  The problem statement hints at this possibility.

**Day 21**

This is slower than I'd like and takes a bunch of memory, so I'm probably missing something.
