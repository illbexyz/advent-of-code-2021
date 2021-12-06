use std::{fs, io};

fn count_increments(numbers: &[i32]) -> i32 {
    let no_tail = &numbers[0..numbers.len() - 1];
    let no_head = &numbers[1..numbers.len()];

    no_tail
        .iter()
        .zip(no_head)
        .map(|(prev, curr)| if curr > prev { 1 } else { 0 })
        .sum()
}

fn part_1(numbers: &[i32]) -> i32 {
    count_increments(&numbers)
}

fn part_2(numbers: &[i32]) -> i32 {
    let no_head = &numbers[1..numbers.len()];
    let no_head_2 = &numbers[2..numbers.len()];
    let summed_triples: Vec<i32> = numbers
        .iter()
        .zip(no_head)
        .zip(no_head_2)
        .map(|((x, y), z)| x + y + z)
        .collect();

    count_increments(&summed_triples)
}

fn main() -> Result<(), io::Error> {
    let filename = "input.txt";
    let file_contents = fs::read_to_string(filename).expect("The file doesn't exist");
    let numbers: Vec<i32> = file_contents
        .lines()
        .map(|line| line.parse().unwrap())
        .collect();
    let p1 = part_1(&numbers);

    println!("Part One: {}", p1);

    let p2 = part_2(&numbers);

    println!("Part Two: {}", p2);

    Ok(())
}
