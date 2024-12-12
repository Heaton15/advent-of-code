#!/usr/bin/env python3

from pathlib import Path

from dataclasses import dataclass, fields, asdict
from typing import List, NamedTuple, Optional
import copy

Grid = List[List[str]]


class Coordinate(NamedTuple):
    y: int
    x: int
    direction: str


@dataclass
class Guard:
    UP: str = "^"
    RIGHT: str = ">"
    DOWN: str = "v"
    LEFT: str = "<"

    @staticmethod
    def turn(obj: Coordinate):
        """We only turn 90 degrees"""
        new_direction = ""
        for i, guard in enumerate(fields(Guard)):
            if guard.default == obj.direction:
                new_direction = fields(Guard)[(i + 1) % len(fields(Guard))].default
        assert new_direction != ""
        return Coordinate(obj.y, obj.x, str(new_direction))

    @staticmethod
    def move(obj: Coordinate):
        """Move the guard across the grid. Note that 0,0 is the top left of the grid"""
        match obj.direction:
            case Guard.UP:
                return Coordinate(obj.y - 1, obj.x, obj.direction)
            case Guard.RIGHT:
                return Coordinate(obj.y, obj.x + 1, obj.direction)
            case Guard.DOWN:
                return Coordinate(obj.y + 1, obj.x, obj.direction)
            case Guard.LEFT:
                return Coordinate(obj.y, obj.x - 1, obj.direction)
            case _:
                raise Exception("No Valid Movement Direction Specified")


@dataclass
class Blocked:
    BLOCKED: str = "#"


@dataclass
class Visited:
    VISITED: str = "X"


def num_visited_locations(grid: list[list[str]]):
    sum = 0
    for i in grid:
        for j in i:
            if j == Visited.VISITED:
                sum += 1
    return sum


def print_grid(grid: list[list[str]]):
    print("\n\n")
    for row in grid:
        print("".join(row))


def find_guard(grid: list[list[str]]) -> Optional[Coordinate]:
    for i, row in enumerate(grid):
        for j, _ in enumerate(row):
            for f in fields(Guard):
                if grid[i][j] == f.default:
                    return Coordinate(i, j, str(f.default))
    return None


def visited_points(grid: Grid, init_guard: Coordinate):
    """Calculate the coordinates that we visited so we know which squares to test blockages in"""
    return [
        (y, x)
        for y, row in enumerate(grid)
        for x, col in enumerate(row)
        if col == Visited.VISITED and (y, x) != (init_guard.y, init_guard.x)
    ]


def traverse_grid(grid: Grid, init_guard: Coordinate):
    """Moves the guard throughout the grid and marks locations it is in"""
    guard_loc = copy.deepcopy(init_guard)
    grid_tracker = copy.deepcopy(grid)

    # This will let us know if a specific point / direction has been seen already
    blockage_tracker = {
        (i, j): {k: 0 for k in str(asdict(Guard()).values())}
        for i in range(len(grid_tracker))
        for j, ele in enumerate(grid_tracker[i])
        if ele == Blocked.BLOCKED
    }

    while True:
        next_loc = Guard.move(guard_loc)
        if (
            next_loc.y < 0
            or next_loc.y > len(grid) - 1
            or next_loc.x < 0
            or next_loc.x > len(grid[0]) - 1
        ):
            # We are off the grid and are done
            grid_tracker[guard_loc.y][guard_loc.x] = Visited.VISITED
            break
        match grid[next_loc.y][next_loc.x]:
            case Blocked.BLOCKED:
                blockage_tracker[(next_loc.y, next_loc.x)][next_loc.direction] += 1
                if blockage_tracker[(next_loc.y, next_loc.x)][next_loc.direction] > 1:
                    grid_tracker[guard_loc.y][guard_loc.x] = guard_loc.direction
                    return -1, grid_tracker
                guard_loc = Guard.turn(guard_loc)
            case _:
                grid_tracker[guard_loc.y][guard_loc.x] = Visited.VISITED
                guard_loc = Guard.move(guard_loc)
    return (num_visited_locations(grid_tracker), grid_tracker)


def solutions():
    # Step 1. Build a grid that will be walked
    input = Path("inputs/guard_path.txt").read_text()
    grid: list[list[str]] = []
    for i in range(len(input.split())):
        row = []
        for _, ele in enumerate(input.split()[i]):
            row.append(ele)
        grid.append(row)

    init_guard = find_guard(grid)
    assert init_guard is not None, "No guard can be found in the grid"

    p1, new_grid = traverse_grid(grid, init_guard)
    visited = visited_points(new_grid, init_guard)
    print(f"Day 6: Part 1: {p1}")

    p2 = 0
    for i, v in enumerate(visited):
        new_grid = copy.deepcopy(grid)
        new_grid[v[0]][v[1]] = Blocked.BLOCKED
        result, g = traverse_grid(new_grid, init_guard)
        if result == -1:
            p2 += 1
    print(f"Day 6: Part 2: {p2}")


def main():
    solutions()


if __name__ == "__main__":
    main()
