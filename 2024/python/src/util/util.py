#!/usr/bin/env python3

from typing import List

def to_int_array(file: str) -> List[List[int]]:
    ary: List[List[int]] = []
    with open(file) as f:
        while True:
            line = f.readline()
            if not line:
                break
            ary.append([int(i) for i in line.split()])
    return ary

def to_str_array(file: str) -> List[str]:
    ary: List[str] = []
    with open(file) as f:
        while True:
            line = f.readline()
            if not line:
                break
            ary.append(line)
    return ary
