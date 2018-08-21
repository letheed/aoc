from typing import List, Set, Tuple
from aoc.common import answer, Answers, Date, Day, Puzzle
from . import YEAR
from itertools import accumulate

DATE: Date = Date(Day.D03, YEAR)
PUZZLE: Puzzle = Puzzle(DATE, lambda input: solve(input))


def solve(input: str) -> Answers:
    moves = [parse_move(c) for c in input]
    count_solo = len(houses(moves))
    count_duo = len(houses(moves[::2]) | houses(moves[1::2]))
    return answer(count_solo, count_duo)


Move = Tuple[int, int]


def parse_move(c: str) -> Move:
    if   c == '>': return  1, 0
    elif c == '<': return -1, 0
    elif c == '^': return  0, 1
    elif c == 'v': return  0, -1
    else: raise ValueError(str)


def houses(moves: List[Move]) -> Set[Move]:
    return set(accumulate(moves, lambda a, b: (a[0] + b[0], a[1] + b[1])))
