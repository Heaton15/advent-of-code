#!/usr/bin/env python3

import re
from typing import List


def main():
    l: List[int] = []
    r: List[int] = []
    with open("inputs/id_list.txt") as f:
        while True:
            line = f.readline()
            if not line:
                break
            match = re.match(r"(\d+)\s+(\d+)", line)
            if match:
                l = l + [int(match.groups()[0])]
                r = r + [int(match.groups()[1])]

    l.sort()
    r.sort()

    d = sum([abs(a - b) for a, b in zip(l, r)])
    print(f"Day 1: Part 1: {d}")

    sim_score = 0
    for ele1 in l:
        mult = 0
        for ele2 in r:
            if ele2 == ele1:
                mult += 1
        sim_score += ele1 * mult

    print(f"Day 1: Part 2: {sim_score}")


if __name__ == "__main__":
    main()
