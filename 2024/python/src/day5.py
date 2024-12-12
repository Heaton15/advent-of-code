#!/usr/bin/env python3

from util.util import to_str_array
from typing import Dict, List, Optional, Set, Tuple

import re


def parse_file(file: str) -> Tuple[Dict[int, Set[int]], List[List[int]]]:
    cmds: List[str] = []
    orders: List[List[int]] = []
    pages: Set[int] = set()

    graph: Dict[int, Set[int]] = {}

    with open(file) as f:
        while True:
            line = f.readline()
            if not line:
                break
            if m := re.match(r"(\d+)\|(\d+)", line):
                cmds += [line.strip()]
                k = int(m.groups()[0])
                v = int(m.groups()[1])
                pages.add(k)
                pages.add(v)
                if k not in graph.keys():
                    graph[k] = set()
                if v not in graph.keys():
                    graph[v] = set()
                graph[k].add(v)
            if re.match(r"^\d+(,\d+)*$", line):
                m = re.findall(r"\d+", line)
                orders += [list(map(int, m))]
    return (graph, orders)


def main():
    rules, orders = parse_file("inputs/paper_order.txt")

    def is_valid(rules: Dict[int, Set[int]], sequence: List[int]):
        """ Informs us if the sequence we are looking at is valid or not """
        empty = set()
        for seq in sequence:
            if empty & rules.get(seq, set()):
                return False
            empty.add(seq)
        return True

    # Stole this from the internet. My DFS / topographical sorting implementations all failed
    # since there is a cycle in the graph. Turns out that was intended to mess with people.
    # Needed to just move on 
    def median(rules: dict[int, set[int]], update: set[int], k: int) -> int:
        """
        Return the k_th ordered element of this set given the rules
        """
        candidate = update.pop()
        # If this was the last element in update, it must be the k_th element by elimination
        if not update:
            return candidate
        # Else, find the elements that are greater than the candidate using rules
        gt_candidate = update - rules.get(candidate, set())
        # The elements less that the candidate are whatever remains from the update
        lt_candidate = update - gt_candidate
    
        N = len(lt_candidate)
        # If there are exactly N elements less than the candidate, we've found the median
        if N == k:
            return candidate
        # If there are too many elements less than the candidate, we look for the k_th element of lt_candidate
        if k < N:
            return median(rules, lt_candidate, k)
        # Otherwise, the k_th element is in gt_candidate
        # We subtract N + 1 from k since they're out of consideration
        return median(rules, gt_candidate, k - N - 1)

    p1, p2 = 0, 0
    for o in orders:
        i = len(o) // 2
        if is_valid(rules, o):
            p1+=o[i]
        else:
            p2 += median(rules, set(o), i)

    print(p1)
    print(p2)






if __name__ == "__main__":
    main()
