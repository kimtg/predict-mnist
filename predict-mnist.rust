use std::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::str::FromStr;

fn square(x: i32) -> i32 {
    x * x
}

fn sum_sq_err(x: &[i32], y: &[i32]) -> i32 {
    if x.len() != y.len() {
        panic!("not same size");
    }
    let mut sum = 0;
    for i in 0..x.len() {
        sum += square(x[i] - y[i]);
    }
    sum
}

fn main() -> Result<(), Box<dyn Error>> {
    println!("!");
    let file_name_train = "mnist_train.csv";
    let file_name_test = "mnist_test.csv";

    // train
    let train_data = read_csv(file_name_train)?;
    println!("train data loaded. rows: {}", train_data.len());

    // test
    let mut n_rows = 0;
    let mut n_correct = 0;
    for line in read_csv(file_name_test)? {
        n_rows += 1;
        let (answer, data) = (line[0], &line[1..]);
        let mut best = 0;
        let mut min_error = -1;
        for row in &train_data {
            let error = sum_sq_err(&data, &row[1..]);
            if min_error < 0 || error < min_error {
                min_error = error;
                best = row[0];
            }
        }
        if best == answer {
            n_correct += 1;
        }
        println!(
            "row: {}, predicted: {}, answer: {}, accuracy: {}",
            n_rows,
            best,
            answer,
            (n_correct as f64) / (n_rows as f64)
        );
    }
    Ok(())
}

fn read_csv(file_name: &str) -> Result<Vec<Vec<i32>>, Box<dyn Error>> {
    let path = Path::new(file_name);
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let mut data = Vec::new();
    for line in reader.lines() {
        let line = line?;
        let values: Vec<i32> = line
            .split(',')
            .map(|s| i32::from_str(s).unwrap())
            .collect();
        data.push(values);
    }
    Ok(data)
}

