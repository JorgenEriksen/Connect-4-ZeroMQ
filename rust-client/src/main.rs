use ansi_term::Colour;
use rand::Rng;
use std::env;
use std::io;
use std::io::Write;

mod board; // give access to usage of board specific functions.
mod utils;

fn main() {
    let args: Vec<String> = env::args().collect(); // collect all CLI arguments passed to the program.
    let pve_arg = String::from("-ai");
    let mut pve = false;
    if args.contains(&pve_arg) {
        pve = true;
        println!("ai mode has been choosen!");
    }

    let mut player = 1;
    let context = zmq::Context::new();
    let requester = context.socket(zmq::REQ).unwrap();
    let mut board: String = "000000000000000000000000000000000000000000".to_owned();

    utils::print_welcome_message();
    board::print_board(&mut board);
    requester
        .connect("tcp://localhost:5555")
        .expect("failed to connect requester");

    loop {
        // endless loop until someone wins
        if player == 2 && pve {
            // if ai's turn
            print!(
                "\ncalculating {} move...",
                Colour::RGB(255, 255, 0).paint("AI")
            );
        } else if player == 2 {
            print!(
                "\nplayer {} move: ",
                Colour::RGB(255, 255, 0).paint("yellow")
            );
        } else {
            print!("\nplayer {} move: ", Colour::Red.paint("red"));
        }
        io::stdout().flush();
        let mut number_input = String::from("");
        let mut is_valid_input = false;

        if player == 2 && pve {
            // calculates ai move
            let mut found_winning_move = false;
            for x in 1..7 {
                if !board::validate_board(&mut board, x - 1) {
                    // if column x is full
                    continue;
                }

                let check_if_win_board = board::modify_board(&mut board, x - 1, player.to_string());
                requester.send(&check_if_win_board, 0).unwrap(); // sends board to haskell server with zeroMQ
                let message = requester.recv_msg(0).unwrap(); // recives message from haskell with result if won or not

                if message.as_str().unwrap() == "won" {
                    // if move will win the game
                    number_input = x.to_string(); // chooses the winning move
                    found_winning_move = true;
                    break; // exits the loop
                }
            }

            if !found_winning_move {
                // if not found winning move, choose a random move
                let mut column_not_full = false;
                let mut random_move: usize = 1;
                while !column_not_full {
                    random_move = rand::thread_rng().gen_range(1..8);
                    if board::validate_board(&mut board, random_move - 1) {
                        column_not_full = true;
                    } else {
                        column_not_full = false;
                    }
                }
                number_input = random_move.to_string();
            }
            print!("{}\n", number_input); // prints the ai move
            is_valid_input = true;
        } else {
            // pvp
            number_input = utils::read_input();
            is_valid_input = utils::validate_input(&mut number_input);
        }

        // let (trimmed_input, valid_input) = readInput();
        if is_valid_input {
            // if a number
            let my_int = number_input.parse::<usize>().unwrap();
            if 1 <= my_int && my_int <= 7 {
                // if between 1 and 7
                if board::validate_board(&mut board, my_int - 1) {
                    board = board::modify_board(&mut board, my_int - 1, player.to_string());
                    requester.send(&board, 0).unwrap(); // sends board to haskell server with zeroMQ
                    let message = requester.recv_msg(0).unwrap(); // recives message from haskell with result if won or not
                    board::print_board(&mut board);
                    if message.as_str().unwrap() == "won" {
                        break;
                    } else {
                        player = (player % 2) + 1;
                    }
                } else {
                    board::print_board(&mut board);
                    println!("Column is filled up. Pick another one");
                }
            } else {
                println!("Number needs to be from 1 to 7");
            }
        } else {
            println!("Input needs to be number");
        }
    }
    if player == 2 {
        println!(
            "\nplayer {} won!\n",
            Colour::RGB(255, 255, 0).paint("yellow")
        );
    } else {
        println!("\nplayer {} won!\n", Colour::Red.paint("red"));
    }
}
