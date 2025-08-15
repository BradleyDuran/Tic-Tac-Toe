This project is a complete implementation of Tic-Tac-Toe in Racket, supporting both the standard 3×3 grid and larger N×N boards. The game logic is fully functional and uses immutable data structures, recursion, and list processing.
Features
Board validation – Ensures the board is a valid square grid, contains only legal symbols ('X, 'O, 'E), and follows turn order rules.
Turn tracking – Determines which player moves next based on the current board state.
Move validation – Checks whether a move is legal for a given player.
Immutable board updates – Returns a new board with the move applied (no mutation).
Winner detection – Identifies if 'X or 'O has won, or if the game continues.
Functions
board? – Validates a board’s structure and rules.
next-move – Determines the next player.
valid-move? – Checks if a move is allowed.
make-move – Returns a new board with a move applied.
winner? – Detects a winner or returns #f if none.
How to Play
The project includes a GUI for interactive play.
From the terminal:
racket gui.rkt         # Standard 3×3 game
racket gui.rkt -v      # Verbose mode
racket gui.rkt -k 4    # Play on a 4×4 board

| O| X |O |
| E| X |E |
| E| X |E |

Winner: 'X (vertical win in middle column)

Skills Used:

Functional programming in Racket
Recursion and list processing
Immutable data handling
Game logic implementation
