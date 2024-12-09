#!/usr/bin/env python3

from util.util import to_str_array
import re
import enum

MUL_INSTR = r"mul("


class EnablePattern(enum.StrEnum):
    enabled = "do()"
    disabled = "don't()"


def main():
    memory = to_str_array("inputs/corrupted_memory.txt")

    sum = 0
    for line in memory:
        for i in range(len(line)):
            if line[i : i + 4] == MUL_INSTR:
                arg_str = line[i + 4 : i + 12]
                i = i + 4
                match = re.match(r"(\d{1,3}),(\d{1,3})\)", arg_str)
                if match:
                    sum += int(match.groups()[0]) * int(match.groups()[1])

    print(f"Day 3: Part 1: {sum}")

    sum = 0
    isEnabled = EnablePattern.enabled
    for line in memory:
        for i in range(len(line)):
            if line[i : i + len(EnablePattern.enabled)] == EnablePattern.enabled:
                isEnabled = EnablePattern.enabled
            elif line[i : i + len(EnablePattern.disabled)] == EnablePattern.disabled:
                isEnabled = EnablePattern.disabled

            if isEnabled == EnablePattern.enabled:
                if line[i : i + 4] == MUL_INSTR:
                    arg_str = line[i + 4 : i + 12]
                    i = i + 4
                    match = re.match(r"(\d{1,3}),(\d{1,3})\)", arg_str)
                    if match:
                        sum += int(match.groups()[0]) * int(match.groups()[1])

    print(f"Day 3: Part 2: {sum}")


if __name__ == "__main__":
    main()
