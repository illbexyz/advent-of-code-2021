use std::{fs, io};

fn part_1(numbers: &[i32]) -> i32 {
    let no_tail = &numbers[0..numbers.len() - 1];
    let no_head = &numbers[1..numbers.len()];

    no_tail
        .iter()
        .zip(no_head)
        .map(|(prev, curr)| if curr > prev { 1 } else { 0 })
        .sum()
}

fn main() -> Result<(), io::Error> {
    let filename = "input.txt";
    let file_contents = fs::read_to_string(filename).expect("The file doesn't exist");
    let numbers: Vec<i32> = file_contents
        .lines()
        .map(|line| line.parse().unwrap())
        .collect();
    let increments = part_1(&numbers);

    println!("{}", increments);

    Ok(())
}
