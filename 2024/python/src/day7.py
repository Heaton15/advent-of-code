#!/usr/bin/env python3
from enum import StrEnum
import itertools
from pathlib import Path
from typing import List
import re


class Operators(StrEnum):
    MULT = "*"
    ADD = "+"
    CAT = "||"


def scan_sequence(target: int, sequence: List[int], p2: bool = False) -> int:
    prod = list(itertools.product(["+", "*"], repeat=len(sequence) - 1))

    if p2:
        prod = list(itertools.product(["+", "*", "||"], repeat=len(sequence) - 1))

    for comb in prod:
        result = sequence[0]
        for i in range(len(sequence) - 1):
            match comb[i]:
                case Operators.MULT:
                    result = result * sequence[i + 1]
                case Operators.ADD:
                    result = result + sequence[i + 1]
                case Operators.CAT:
                    result = int(str(result)+str(sequence[i+1]))
            if result > target:
                break  # No need to keep calculating sum since its dead on arrival
        if result == target:
            return target
    return 0


def main():
    file = Path("inputs/operator_list.txt").read_text().split("\n")
    inputs: list[list[int]] = [
        [int(f[0]), *list(map(int, f[1:]))]
        for line in file
        if (f := re.findall(r"\d+", line))
    ]

    p1, p2 = 0, 0
    for s in inputs:
        p1 += scan_sequence(s[0], s[1:])

    for s in inputs:
        p2 += scan_sequence(s[0], s[1:], True)

    print(f"Day 7: Part 1: {p1}")
    print(f"Day 7: Part 1: {p2}")


if __name__ == "__main__":
    main()
