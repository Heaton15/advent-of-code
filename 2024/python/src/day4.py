#!/usr/bin/env python3

from util.util import to_str_array


class Node:
    def __init__(self, x, y, char):
        self.x = x
        self.y = y
        self.char = char


def main():
    text = to_str_array("inputs/xmas_search.txt")
    r_height, c_height = len(text), len(text[0]) - 1  # remove newline at the end
    grid = {(y, x): text[y][x] for y in range(r_height) for x in range(c_height)}

    count = 0
    for r in range(r_height):
        for c in range(c_height):
            for dx in (-1, 0, 1):
                for dy in (-1, 0, 1):
                    word = "".join(
                        grid.get((r + dx * n, c + dy * n), "") for n in range(4)
                    )
                    if word == "XMAS":
                        count += 1

    print(f"Day 4: Part 1: {count}")

    count = 0
    for (x, y), l in grid.items():
        # if SAM / MAS -> S->M or M->S wraps around A in both sides, we have an X
        if l == "A":
            if grid.get((x+1, y+1), "") == "M" and grid.get((x-1,y-1), "") == "S" or grid.get((x+1, y+1), "") == "S" and grid.get((x-1,y-1), "") == "M":
                if grid.get((x+1, y-1), "") == "M" and grid.get((x-1,y+1), "") == "S" or grid.get((x+1, y-1), "") == "S" and grid.get((x-1,y+1), "") == "M":
                    count+=1

    print(f"Day 4: Part 2: {count}")



if __name__ == "__main__":
    main()
