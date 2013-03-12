README ====> chequers.lisp

BY: CODY CANNING

OBJECT:

Move all your marbles into the opponent's home zone. Legal moves are single space hops, or consecutive jumps over marbles.
See http://en.wikipedia.org/wiki/Chinese_checkers for complete rules.

HOW-TO-USE:

In order to play a game follow these steps:

1. (defconstant gm (init-game))  ;; define gm to a blank board

2. The GREEN player is at the top, the BLUE player is at the bottom.

3. Use either MOVE or HARD-MOVE for the human player's move. Since the chinese checkers board consists of offset rows, it's difficult to tell where exactly the marbles are located. For convenience the program prints the current locations of the player's marbles and the legal moves (x1 y1 x2 y2) possible for that player.

MOVE takes the following inputs: GAME (gm), (DIRECTION) (e.g. NE), X1, Y1.
 for example the call (move gm NE 13 6) would move the marble at 13 6 diagonally NE

HARD-MOVE takes GAME (gm), X1 Y1 X2 Y2
 for example the call (hard-move gm 13 6 12 6) would move the marble at 13 6 to 12 6

4. For the AI's move, use (COMP-DO-MOVE! gm T) or (COMP-DO-MOVE! gm nil). The bool at the end denotes a randomized move list.







