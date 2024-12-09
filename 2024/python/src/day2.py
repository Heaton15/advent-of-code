#!/usr/bin/env python3

from util import util
from typing import List
from enum import Enum, auto


class Direction(Enum):
    Inc = auto()
    Dec = auto()


def main():
    level_data: List[List[int]] = util.to_int_array("inputs/level_data.txt")
    print(len(level_data))

    safe = 0
    safe_dampened = 0
    for level in level_data:
        dampened = 0

        if level[1] == level[0]:
            dampened = 1
            level = level[1:]

        if level[1] < level[0]:
            dir = Direction.Dec
        else:
            dir = Direction.Inc

        is_safe = 1
        is_safe_dampened = 1

        for i in range(len(level) - 1):
            match dir:
                case Direction.Dec:
                    if not (level[i + 1] < level[i] and (level[i] - level[i + 1] <= 3)):
                        is_safe = 0

                case Direction.Inc:
                    if not (level[i + 1] > level[i] and (level[i + 1] - level[i] <= 3)):
                        is_safe = 0
        if is_safe:
            safe += 1

        for i in range(len(level) - 1):
            match dir:
                case Direction.Dec:
                    if not (level[i + 1] < level[i] and (level[i] - level[i + 1] <= 3)):
                        if not dampened:
                            dampened = 1
                            level = [level.pop(i)] + level
                        else:
                            is_safe_dampened = 0

                case Direction.Inc:
                    if not (level[i + 1] > level[i] and (level[i + 1] - level[i] <= 3)):
                        print(level)
                        if not dampened:
                            dampened = 1
                            level = [level.pop(i)] + level
                            print(level)
                        else:
                            is_safe_dampened = 0

        if is_safe_dampened:
            safe_dampened += 1

    print(f"Day 2: Part 1: {safe}")
    print(f"Day 2: Part 1: {safe_dampened}")


if __name__ == "__main__":
    main()
