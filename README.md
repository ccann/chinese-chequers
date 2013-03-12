# README for chequers.lisp

Author: ccann
Date: spring 2010

OBJECT:

Move all your marbles into the opponent's home zone. Legal moves are single space hops, or
consecutive jumps over marbles. See http://en.wikipedia.org/wiki/Chinese_checkers for
complete rules.

HOW-TO-USE:

In order to play a game follow these steps:

* (defconstant gm (init-game)) ;; define gm to a blank board

* The GREEN player is at the top, the BLUE player is at the bottom.

* Use either MOVE or HARD-MOVE as the player. The latter allows you to specify a discrete
location while the former allows you to specify the direction instead. Since the chinese
checkers board consists of offset rows, it's difficult to tell where exactly the marbles
are located. For convenience the program prints the current locations of the player's
marbles and the legal moves (x1 y1 x2 y2) possible for that player.

  * MOVE takes the following inputs: GAME (gm), (DIRECTION) (e.g. NE), X1, Y1.
    
    e.g. (move gm NE 13 6)  ;;  move the marble at (13,6) northeast

  * HARD-MOVE takes GAME (gm), X1 Y1 X2 Y2

    e.g. (hard-move gm 13 6 12 6) ;; move the marble at (13,6) to (12,6)

* For the AI's move, use (COMP-DO-MOVE! gm T) or (COMP-DO-MOVE! gm nil). The bool at the
end denotes a randomized move list.







