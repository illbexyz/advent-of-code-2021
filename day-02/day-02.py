import sys
from enum import Enum
from dataclasses import dataclass
from typing import List

Direction = Enum("Direction", "UP FORWARD DOWN")


@dataclass
class Instruction:
    direction: Direction
    amount: int


@dataclass
class Position:
    x: int
    y: int


def parse_line(line: str) -> Instruction:
    [dir, amount] = line.split(" ")

    if dir == "forward":
        direction = Direction.FORWARD
    elif dir == "up":
        direction = Direction.UP
    elif dir == "down":
        direction = Direction.DOWN
    else:
        raise Exception(f"Can't parse: {line}")

    return Instruction(direction, int(amount))


def read_file(filename: str) -> List[str]:
    with open(filename) as f:
        lines = f.read().splitlines()
        return lines


def evaluate_instruction(curr_position: Position, instruction: Instruction) -> Position:
    if instruction.direction == Direction.DOWN:
        return Position(curr_position.x, curr_position.y + instruction.amount)
    elif instruction.direction == Direction.FORWARD:
        return Position(curr_position.x + instruction.amount, curr_position.y)
    elif instruction.direction == Direction.UP:
        return Position(curr_position.x, curr_position.y - instruction.amount)


def evaluate_instructions(instructions: List[Instruction]) -> Position:
    position = Position(0, 0)
    for instruction in instructions:
        position = evaluate_instruction(position, instruction)
    return position


def part_1(instructions: List[Instruction]) -> int:
    final_position = evaluate_instructions(instructions)
    return final_position.x * final_position.y


def main() -> int:
    lines = read_file("input.txt")
    instructions = list(map(parse_line, lines))
    p1 = part_1(instructions)
    print(f"Part 1: {p1}")
    return 0


if __name__ == "__main__":
    sys.exit(main())  # next section explains the use of sys.exit
