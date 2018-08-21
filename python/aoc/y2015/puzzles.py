from typing import List, Dict
from aoc.common import Day, Puzzle
from . import d01, d02, d03

puzzles_list: List[Puzzle] = [
    d01.PUZZLE,
    d02.PUZZLE,
    d03.PUZZLE,
]

puzzles: Dict[Day, Puzzle] = {puzzle.date.day: puzzle for puzzle in puzzles_list}
