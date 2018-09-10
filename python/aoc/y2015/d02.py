from typing import List
from aoc.common import answer, Answers, Date, Day, Puzzle
from . import YEAR


DATE: Date = Date(YEAR, Day.D02)
PUZZLE: Puzzle = Puzzle(DATE, lambda input: solve(input))


def solve(input: str) -> Answers:
    gifts = [parse_gift(line) for line in input.splitlines()]
    paper_area = sum(map(gift_paper_area, gifts))
    ribbon_length = sum(map(gift_ribbon_length, gifts))
    return answer(paper_area, ribbon_length)


def parse_gift(line: str) -> List[int]:
    gift = [int(n) for n in line.split('x')]
    gift.sort()
    return gift


def gift_paper_area(gift: List[int]) -> int:
    [a, b, c] = gift
    return 3 * (a * b) + 2 * c * (a + b)


def gift_ribbon_length(gift: List[int]) -> int:
    [a, b, c] = gift
    return 2 * (a + b) + a * b * c
