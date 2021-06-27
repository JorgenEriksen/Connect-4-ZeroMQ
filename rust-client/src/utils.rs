use ansi_term::Colour;
use std::convert::TryInto;
use std::io;

// prints the colored welcome message.
pub fn print_welcome_message() {
    let message = String::from("\n##############################################\n###        Welcome to connect 4!           ###\n##############################################");
    for (i, c) in message.chars().enumerate() {
        let r: u8 = (110 + i).try_into().unwrap();
        let g: u8 = (180).try_into().unwrap();
        let b: u8 = (230 - i).try_into().unwrap();
        print!("{}", Colour::RGB(r, g, b).paint(c.to_string()));
    }
    print!("\n");
}

// reads input from user
pub fn read_input() -> String {
    let mut input_integer = String::new();
    io::stdin()
        .read_line(&mut input_integer)
        .expect("failed to read int");
    let trimmed_input = input_integer.trim();
    return trimmed_input.to_string();
}

// check if input is number
pub fn validate_input(input: &mut String) -> bool {
    let mut valid_input = false;
    match input.parse::<u32>() {
        Ok(i) => valid_input = true,
        Err(..) => valid_input = false,
    };
    return valid_input;
}

/**Input validation tests**/
#[cfg(test)]
mod main_test {
    #[test]
    fn validate_input_test() {
        let mut input = String::from("3");
        assert_eq!(super::validate_input(&mut input), true);
        let mut input = String::from(" 3");
        assert_eq!(super::validate_input(&mut input), false);
        let mut input = String::from("3 ");
        assert_eq!(super::validate_input(&mut input), false);
        let mut input = String::from("");
        assert_eq!(super::validate_input(&mut input), false);
        let mut input = String::from(" ");
        assert_eq!(super::validate_input(&mut input), false);
        let mut input = String::from("test");
        assert_eq!(super::validate_input(&mut input), false);
    }
}
