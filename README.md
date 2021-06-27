# Connect 4
### Project by @jrgn and @elvisa

For our project in prog2006 we have chosen to re-create the classic game of **CONNECT 4**. We chose this because we feel it has a nice intersection of both challenging validation aspects and ai while at the same time having a limited scope and a workload that is manageable. We have gone above and beyond and made the the visual experience better for the user. We have introduced colors in the terminal in the welcome message as well as the board/players. 

# Build for unix like operating systems (ubuntu,mac-OS,debian,arch linux).
### Installing zeroMQ
- Tutorial:  
  - clone this repository anywhere https://github.com/zeromq/libzmq.git
  - cd into the directory with the source code
  - type the command `./configure`
  - type `make`
  - type command `make install` 
- Clone this repository 
- Open a terminal in the root of the repository (this project repo)
- cd into `progProject2006`
- open either a screen instance or another terminal and cd into the same directory (progProject2006)
- in one terminal, cd into `haskell-server` and run following commands: `stack build -> stack run`
  - Now the haskell server should be up and running. You will not see anything until you start the game with rust.
- in another terminal, cd into `rust-client` and run the command: `cargo run` to play player vs player, or run: `cargo run -- -ai` to play against AI
- Now you should see rust showing you a welcome message and you are ready to play!

**NB! AS OF NOW THIS PROJECT CAN NOT PROVIDE YOU WITH A INSTALLATION TUTORIAL FOR ØMQ ON WINDOWS. IF YOU HAVE A WORKING ØMQ INSTALLATION ON WINDOWS THIS PROJECT WILL BE ABLE TO BE RAN THERE AS WELL.**

# Usage
The game is very simple, you are player 1, the red player. You will try to make your 1's line up in a line of 4 in any configuration. The goal of player 2 is to do the same. You can play versus the ai by supplying `--ai` to rust like this: `cargo run -- --ai`

Input numbers between 1 and 7, you will get an appropriate error message otherwise.

# Tests
- To run the tests, in the root of both the client and the server run `stack test` for **HASKELL** and `cargo test` for **RUST**

# Rust's job
Rust works as a terminal client for the user, and actually handle most of the game, like the interface, input validation, creating and modifying board, calculating position of a player pieces when choosing a move/column.

# Hakell's job
Checks if there is a winning condition in the board

# AI algorithm explanation.
When it's the AI's turn, it will first check if there is a move it can do in order to win the game. It will do that sending the result board for each possible move top the haskell server, which checks if one of the moves are a winning move. If there are no winning move, the AI will just choose a random move within possible moves.

