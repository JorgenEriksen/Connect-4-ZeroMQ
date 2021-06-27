use ansi_term::Colour;

// checks if the column of the board is not filled up
pub fn validate_board(board: &mut String, index: usize) -> bool {
    if board.chars().nth(index).unwrap() != '0' {
        return false;
    }
    return true;
}

// modifies the board with the new player move
pub fn modify_board(board: &mut String, column: usize, player: String) -> String {
    let mut current_row = 99;
    for i in 0..6 {
        let index = column + (i * 7);
        let circle = board.chars().nth(index).unwrap();
        if circle == '0' {
            current_row = i;
        }
    }
    let position = (current_row * 7) + column;
    let mut new_board = String::from("");
    for (i, c) in board.chars().enumerate() {
        if i == position {
            new_board.push(player.chars().next().unwrap())
        } else {
            new_board.push(c)
        }
    }
    return new_board;
}

// prints the colored board.
pub fn print_board(board: &mut String) {
    print!("\n 1   2   3   4   5   6   7\n");
    for (i, mut c) in board.chars().enumerate() {
        if i % 7 == 0 && i != 0 {
            // if new line
            print!("\n\n");
        }
        if c == '0' {
            c = ' ';
        }
        //Colour::Red.paint("({}) ", c);
        print!("{}", Colour::Blue.paint('('.to_string()));
        if c == '1' {
            print!("{}", Colour::Red.paint(c.to_string()));
        } else if c == '2' {
            print!("{}", Colour::RGB(255, 255, 0).paint(c.to_string()));
        } else {
            print!(" ");
        }
        print!("{} ", Colour::Blue.paint(')'.to_string()));
    }
    print!("\n");
}

/*********BOARD TESTS**********/
#[cfg(test)]
mod board_test {
    #[test]
    fn validate_board_test() {
        let mut board = String::from("000000000000000000000000000000000000000000");
        let mut player_move: usize = 1;
        assert_eq!(super::validate_board(&mut board, player_move), true);
        let mut board = String::from("001000000000000000000000000000000000000000");
        let mut player_move: usize = 2;
        assert_eq!(super::validate_board(&mut board, player_move), false);
        let mut board = String::from("101122112211221122112211221122112211221122");
        let mut player_move: usize = 1;
        assert_eq!(super::validate_board(&mut board, player_move), true);
    }
    #[test]
    fn modify_board_test() {
        let mut board = String::from("000000000000000000000000000000100000120200");
        let mut column: usize = 2;
        let mut player = 1;
        assert_eq!(
            super::modify_board(&mut board, column, player.to_string()),
            "000000000000000000000001000000100000120200"
        );
        let mut board = String::from("000000000000000000000000000000100000120200");
        let mut column: usize = 6;
        let mut player = 2;
        assert_eq!(
            super::modify_board(&mut board, column, player.to_string()),
            "000000000000000000000000000000100000120202"
        );
    }
}
