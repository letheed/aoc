from aoc.common import Puzzle
from aoc.y2015 import puzzles
from colors import red, green


def main():
    present_puzzles()


def present_puzzles():
    for puzzle in puzzles.values():
        present_puzzle(puzzle)


def present_puzzle(puzzle: Puzzle):
    date_str = f'{puzzle.date}'
    solutions_str = ''
    answers, duration = puzzle.solve()
    if not answers:
        duration_str = '          '
        answers_str = 'no result'
    else:
        duration = round(duration * 1e6)
        if duration < 1000:
            duration_str = f'{duration:7} μs'
        elif duration < 1_000_000:
            dur_int, dur_frac = divmod(duration, 1000)
            duration_str = f'{dur_int:3}.{dur_frac:03} ms'
        else:
            dur_int, dur_frac = divmod(duration, 1_000_000)
            duration_str = red(f'{dur_int:3}.{dur_frac:03} s ')

        solutions = puzzle.read_solution()
        if solutions is None:
            if len(answers) == 1:
                answers_str = f'{answers[0]}'
            else:
                answers_str = f'({answers[0]}'
                for answer in answers[1:]:
                    answers_str += f', {answer}'
                answers_str += ')'
        else:
            if len(answers) == 1:
                answer = answers[0]
                answers_str = f'{answer}'
                try:
                    solution = solutions[0]
                    if answer == solution:
                        answers_str = green(answers_str)
                    else:
                        answers_str = red(answers_str)
                        solutions_str = f' solution = {solution}'
                except IndexError:
                    pass
            else:
                answer_strs, solution_strs = ([], [])
                for i, answer in enumerate(answers):
                    try:
                        solution = solutions[i]
                        if answer == solution:
                            answer_strs.append(green(f'{answer}'))
                        else:
                            answer_strs.append(red(f'{answer}'))
                            solution_strs.append(f'{solution}')
                    except IndexError:
                        answer_strs.append(f'{answer}')
                answers_str = f'({answer_strs[0]}'
                for answer_str in answer_strs[1:]:
                    answers_str += f', {answer_str}'
                answers_str += ')'
                if not solution_strs:
                    pass
                elif len(solution_strs) == 1:
                    solutions_str = f' solution = {solution_strs[0]}'
                else:
                    solutions_str = f' solution = ({solution_strs[0]}'
                    for solution_str in solution_strs[1:]:
                        solutions_str += f', {solution_str}'
                    solutions_str += ')'
    print(f'{date_str} {duration_str} {answers_str}{solutions_str}')


if __name__ == '__main__':
    main()
