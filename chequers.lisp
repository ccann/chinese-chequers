;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chinese Chequers (currently 2-player)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Cody C. Canning
;;; Due Date: December 12th, 2010
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; COMPILER-FLAGS (must be loaded before compiling)

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t)

;;; -----------------------------------------------
;;; GLOBAL CONSTANTS
;;; -----------------------------------------------

;;; Players

(defconstant *green* 0)
(defconstant *blue* 1)
;;(defconstant *red* 2)
;;(defconstant *violet* 3)

(defconstant *PLAYERS* (list *green* *blue*))
;; *red*
;; *violet*

;;; Cardinal Direction Moves

(defconstant NE 0) ;; NORTH EAST
(defconstant NW 1) ;; NORTH WEST
(defconstant E 2)  ;; EAST
(defconstant W 3)  ;; WEST
(defconstant SE 4) ;; SOUTH EAST
(defconstant SW 5) ;; SOUTH WEST

(defparameter *row* 0)
(defparameter *col* 0)


;;; Number of players: 2 

(defconstant *NUM-PLAYERS* 2)

;;; Depth limit for the search

(defconstant *cutoff-depth* 4)
;;; WIN/LOSS values
;;; ----------------------------------------

(defconstant *win-value* 400000)
(defconstant *loss-value* -400000)

;;;  NEGATIVE and POSITIVE INFINITY
;;; ----------------------------------------

(defconstant *neg-inf* -10000000)
(defconstant *pos-inf*  10000000)

;; global parameter to see how many moves checked during search
(defparameter *num-moves* 0)

;;; Marble symbols arrayed by player

(defconstant *marble-symbols* #2A((#\G) ;; GREEN
				  (#\B) ;; BLUE
				  (#\R) ;; RED
				  (#\V) ;; VIOLET
				  ))


;;; List of illegal spaces that cannot be occupied. Necessary because the six-pointed star-
;;; shaped board is contained by a rectangular 2D array.

(defconstant *illegal-spaces* (list '(0 0)'(0 1)'(0 2)'(0 3)'(0 4)'(0 5)'(0 7)'(0 8)'(0 9)'(0 10)'(0 11)'(0 12)
				    '(1 0)'(1 1)'(1 2)'(1 3)'(1 4)'(1 5)'(1 8)'(1 9)'(1 10)'(1 11)'(1 12)
				    '(2 0)'(2 1)'(2 2)'(2 3)'(2 4)'(2 8)'(2 9)'(2 10)'(2 11)'(2 12)
				    '(3 0)'(3 1)'(3 2)'(3 3)'(3 4)'(3 9)'(3 10)'(3 11)'(3 12)
				    '(5 0)
				    '(6 0)'(6 12)
				    '(7 0)'(7 1)'(7 12)
				    '(8 0)'(8 1)'(8 11)'(8 12)
				    '(9 0)'(9 1)'(9 12)
				    '(10 0)'(10 12)
				    '(11 0)
				    '(13 0)'(13 1)'(13 2)'(13 3)'(13 4)'(13 9)'(13 10)'(13 11)'(13 12)
				    '(14 0)'(14 1)'(14 2)'(14 3)'(14 4)'(14 8)'(14 9)'(14 10)'(14 11)'(14 12)
				    '(15 0)'(15 1)'(15 2)'(15 3)'(15 4)'(15 5)'(15 8)'(15 9)'(15 10)'(15 11)'(15 12)
				    '(16 0)'(16 1)'(16 2)'(16 3)'(16 4)'(16 5)'(16 7)'(16 8)'(16 9)'(16 10)'(16 11)'(16 12)))

;;; GOAL SPACES are the top points of the star opposite the player
(defconstant *goal-spaces* (list '(16 6) ;; for GREEN Player
				 '(0 6) ;; for BLUE Player 
				 '(12 12) ;; for the RED player
				 '(4 0)  ;; for the VIOLET player
				 '(12 0) 
				 '(4 12)))

;;;lists of star coordinates (home zones) for the players
(defconstant *green-home-zone* (list '(0 6) '(1 6) '(1 7) '(2 5) '(2 6) '(2 7) '(3 5) '(3 6) '(3 7) '(3 8)))
(defconstant *blue-home-zone* (list '(16 6) '(15 6) '(15 7) '(14 5) '(14 6) '(14 7) '(13 5) '(13 6) '(13 7) '(13 8)))
(defconstant *red-home-zone* (list '(9 11) '(10 10) '(10 11) '(11 10) '(11 11) '(11 12) '(12 9) '(12 10) '(12 11) '(12 12)))
(defconstant *violet-home-zone* (list '(7 2) '(6 1) '(6 2) '(5 1) '(5 2) '(5 3) '(4 0) '(4 1) '(4 2) '(4 3)))

;;; HOME ZONES, a list of the home zones. Index into the list with the player number
;;; to get the goal zone opposite of that player.
(defconstant *HOME-ZONES* (list *blue-home-zone* *green-home-zone* *violet-home-zone* *red-home-zone*))



;;; CHEQUERS struct
;;; ---------------------------------------------------------------------------------------------
;;; Fields:
;;;
;;;    BOARD -- a 13 by 17 array containing MARBLE structs, or NIL
;;;    MARBLES -- an 2 by 10 array for accessing marbles by player
;;;    MOVE-HISTORY -- list of moves from start to finish
;;;    EVAL-SUBTOTALS -- Vector containing the sums of the distances of all the player's marbles
;;;                      from the goal space.
;;;    WHOSE-TURN? -- Designates whose turn it currently is. *green* starts.

(defstruct (chequers (:print-function print-chequers))
  (board (make-array '(17 13) :initial-element nil))
  (marbles (make-array '(2 10) :initial-element nil))
  (move-history nil)
  (eval-subtotals (vector 0 0 0 0))
  (whose-turn? *green*))



;;; PRINT CHEQUERS
;;; ------------------------------------------------------
;;; Print function for chinese chequers struct

(defun print-chequers (game str depth)
  (declare (ignore depth))
  (let* ((board (chequers-board game))
	 (whose-turn? (chequers-whose-turn? game)))
    ;; print the column numbers across the top
    (format str "    0 1 2 3 4 5 6 7 8 9 101112~%")
    (format str "   ---------------------------~%")
    ;; print the row number for each row
    (dotimes (row 17)
      ;; in the case of an even row, use additional space
      (if (evenp row) 
	  ;; in the case of a row > 9 use additional space
	  (if (> row 9)
	      (format str "~A:  " row)
	    (format str "~A:   " row))
	(if (> row 9)
	    (format str"~A: " row)
	  (format str "~A:  " row)))
      
      (dotimes (col 13) ;; for each column
	(let ((state (aref board row col))) ;; get state of the board slot
	  (if (eq state nil) ;; if there is no marble
	      (format str "O ") ;; print a O
	    (format str "~A " state)))) ;; if it's a marble, print the symbol for it
      (format str "~%"))
    (format str "   ----------------------------~%")
    
    ;; print whose turn it is
    (format str " It is ~A's turn.~%"
	    (cond ((eq *green* whose-turn?)"green")
		  ((eq *blue* whose-turn?)"blue")
		  ((eq *red* whose-turn?) "red")
		  ((eq *violet* whose-turn?) "violet")))
    ;; print out the locations of the current player's marbles
    (format str "~A's marbles are located at: ~A~%" 
	    (cond ((eq *green* whose-turn?)"green")
		  ((eq *blue* whose-turn?)"blue")
		  ((eq *red* whose-turn?) "red")
		  ((eq *violet* whose-turn?) "violet"))
	    (get-marble-locations game))
    ;; print out the legal moves that the current player can make
    (format str "~A's legal moves are: ~%"
	    (cond ((eq *green* whose-turn?)"green")
		  ((eq *blue* whose-turn?)"blue")
		  ((eq *red* whose-turn?) "red")
		  ((eq *violet* whose-turn?) "violet")))
    (dolist (move (get-all-legal-moves game nil))
      (print move)))
  (format str "~%"))

;;; MARBLE
;;; ------------------------------------------------------------------
;;; Fields:
;;;         OWNER: which player the marble belongs to
;;;         ROW: row of the marble
;;;         COL: column of the marble
;;;         IN-GOAL?: T if the marble is in the goal zone, NIL if not
;;;         DISTANCE: distance of the marble from the goal space
;;; 

(defstruct (marble (:print-function print-marble))
  owner    ;; which player the marble belongs to
  row      ;; which row the marble is located at
  col      ;; which column the marble is located at
  in-goal? ;; T if in the opposite star home base, NIL if not
  distance ;; distance of the marble from the goal space
  )


;;; GET-MARBLE-LOCATIONS
;;; ------------------------------------
;;; INPUT: GAME, a chequers struct
;;; OUTPUT: the locations of the current player's marbles
;;;         in list form
;;;

(defun get-marble-locations (game)
  (let* ((plyr (chequers-whose-turn? game))
	 (marbles (chequers-marbles game))
	 (locations '()))
    (dotimes (x 10)
      (let ((marb (aref marbles plyr x)))
	(setf locations (cons (list (marble-row marb) (marble-col marb)) locations))))
    ;; return the list of locations of all the player's marbles
    locations))


;;;  PRINT-MARBLE
;;; --------------------------------------------------------
;;;  Print function for MARBLE structs

(defun print-marble (mb str depth)
  (declare (ignore depth))
  (format str "~A" (aref *marble-symbols* (marble-owner mb) 0)))


;;;  CREATE-PLACE-NEW-MARBLE!  -- used by INIT-GAME
;;; ---------------------------------------------------------------
;;;  INPUTS:  GAME, a CHEQUERS struct
;;;           OWNER, a color representing the player who owns the marble
;;;           ROW, COL, indices of the marble on the board
;;;           I, index between 0 and 9
;;;  OUTPUT:  T
;;;  SIDE EFFECTS:  Creates a MARBLE struct using the given information, places
;;;    it onto the board, and inserts it into the MARBLES array with index I.

(defun create-place-new-marble! (game owner row col i)
  (let ((marbles (chequers-marbles game))
	;; make a new marble
	(marb (make-marble :owner owner
			   :row row
			   :col col
			   :in-goal? nil
			   :distance 0
			   )))
    
    ;; add the marble to the players list of marbles
    (setf (aref marbles owner i) marb)
    
    ;; calculate the marble's distance from the goal zone
    (setf (marble-distance marb) (distance-formula row col (first (nth owner *goal-spaces*))
						   (second (nth owner *goal-spaces*))))
    
    ;; put marble on the game board
    (place-marble! game marb))
  t)


;;;  PLACE-MARBLE! 
;;; -------------------------------------------
;;;  INPUTS:  GAME, a CHEQUERS struct
;;;           MARB, a MARBLE struct
;;;  OUTPUT:  None
;;;
;;;  SIDE EFFECT:  Places marble on the game board at the location specified
;;;                by row and column.

(defun place-marble! (game marb)
  (setf (aref (chequers-board game) (marble-row marb) (marble-col marb))
        marb))


;;;  INIT-GAME
;;; ----------------------------------------------------------------
;;;  INPUTS:  None
;;;  OUTPUT:  A CHEQUERS struct corresponding to a new game of chinese chequers

(defun init-game ()
  (let ((game (make-chequers)))
    ;; place the marbles on the board 
    
    ;; create and place the marbles for the GREEN player
    ;; in the very top star base
    (create-place-new-marble! game *green* 0 6 0)
    (create-place-new-marble! game *green* 1 6 1)
    (create-place-new-marble! game *green* 1 7 2)
    (create-place-new-marble! game *green* 2 5 3)
    (create-place-new-marble! game *green* 2 6 4)
    (create-place-new-marble! game *green* 2 7 5)
    (create-place-new-marble! game *green* 3 5 6)
    (create-place-new-marble! game *green* 3 6 7)
    (create-place-new-marble! game *green* 3 7 8)
    (create-place-new-marble! game *green* 3 8 9)
    
    ;; create and place the marbles for the BLUE player
    ;; in the very bottom star base
    (create-place-new-marble! game *blue* 16 6 0)
    (create-place-new-marble! game *blue* 15 6 1)
    (create-place-new-marble! game *blue* 15 7 2)
    (create-place-new-marble! game *blue* 14 5 3)
    (create-place-new-marble! game *blue* 14 6 4)
    (create-place-new-marble! game *blue* 14 7 5)
    (create-place-new-marble! game *blue* 13 5 6)
    (create-place-new-marble! game *blue* 13 6 7)
    (create-place-new-marble! game *blue* 13 7 8)
    (create-place-new-marble! game *blue* 13 8 9)
    
    ;; create and place the marbles for the RED player
    ;; in the left top star base
    ;;(create-place-new-marble! game *red* 4 0 0)
    ;;(create-place-new-marble! game *red* 4 1 1)
    ;;(create-place-new-marble! game *red* 4 2 2)
    ;;(create-place-new-marble! game *red* 4 3 3)
    ;;(create-place-new-marble! game *red* 5 1 4)
    ;;(create-place-new-marble! game *red* 5 2 5)
    ;;(create-place-new-marble! game *red* 5 3 6)
    ;;(create-place-new-marble! game *red* 6 1 7)
    ;;(create-place-new-marble! game *red* 6 2 8)
    ;;(create-place-new-marble! game *red* 7 2 9)
    
    ;; create and place the marbles for the VIOLET player
    ;; in the right bottom star base
    ;;(create-place-new-marble! game *violet* 9 11 0)
    ;;(create-place-new-marble! game *violet* 10 10 1)
    ;;(create-place-new-marble! game *violet* 10 11 2)
    ;;(create-place-new-marble! game *violet* 11 10 3)
    ;;(create-place-new-marble! game *violet* 11 11 4)
    ;;(create-place-new-marble! game *violet* 11 12 5)
    ;;(create-place-new-marble! game *violet* 12 9 6)
    ;;(create-place-new-marble! game *violet* 12 10 7)
    ;;(create-place-new-marble! game *violet* 12 11 8)
    ;;(create-place-new-marble! game *violet* 12 12 9)
    
    ;; for each index in illegal spaces place a " " symbol
    (dolist (spc *illegal-spaces*)
      (setf (aref (chequers-board game) (first spc) (second spc)) " "))
    
    ;; set the eval subtotals
    (dolist (plyr *players*)
      (set-subtotals game plyr ))
    
    ;; return the game
    game))



;;;  TOGGLE-TURN! 
;;; -------------------------------------------------------
;;;  INPUT:  GAME, a CHEQUERS struct
;;;  OUTPUT:  none
;;;  SIDE EFFECT:  Changes whose turn it is

(defun toggle-turn! (game)
  (let* ((current-turn (chequers-whose-turn? game))
	 ;; game with > 2 players
	 (other-player (if (> *NUM-PLAYERS* 2)
			   (if (eq current-turn 3)
			       *green*
			     (+ 1 current-turn))
			 ;; 2 player game
			 (- 1 current-turn))))
    
    ;; reset the subtotals after a player moves
    (set-subtotals game current-turn)
    
    ;; toggle player
    (setf (chequers-whose-turn? game) other-player)))


;;;  DO-MOVE!
;;; -------------------------------------------
;;;  INPUTS:  GAME, chequers struct
;;;           CHECK-LEGAL?, a boolean flag
;;;           R1,C1, position of marble to move
;;;           R2,C2, position of destination
;;;  OUTPUT:  Resulting CHEQUERS struct if move legal and made
;;;           NIL otherwise
;;;  NOTE:  Only checks legality of proposed move if CHECK-LEGAL? set

(defun do-move! (game check-legal? r1 c1 r2 c2)
  (cond
   ;; Case 1:  Illegal Move??
   ((or (and check-legal? (not (member? (list r1 c1 r2 c2) (get-all-legal-moves game nil)))) 
	(not (eq (chequers-whose-turn? game) (marble-owner (aref (chequers-board game) r1 c1)))))
    (format t  "Can't do illegal move: ~A,~A ==> ~A,~A~%" r1 c1 r2 c2)
    nil)
   
   ;; Case 2:  Legal Move (or wasn't asked to check)
   (t
    (let* ((bored (chequers-board game))
	   (marb (aref bored r1 c1))
	   (owner (marble-owner marb)))
      ;; remove marble from (r1,c1)
      (setf (aref bored r1 c1) nil)
      ;; place marble on destination square
      (setf (aref bored r2 c2) marb)
      (setf (marble-row marb) r2)
      (setf (marble-col marb) c2)
      ;; push move info onto move-history
      (push (list r1 c1 r2 c2 (aref bored r2 c2)) (chequers-move-history game))
      ;; re-calculate the distance of the marble from the goal zone
      (setf (marble-distance marb) (distance-formula r2 c2 (first (nth owner *goal-spaces*))
						     (second (nth owner *goal-spaces*))))
      ;; toggle the turn
      (toggle-turn! game)
      ;; return the GAME
      game))))


;;; UNDO-MOVE!
;;; ----------------------------------------
;;; INPUTS: G, a chequers struct
;;; OUTPUT: Modified chequers struct
;;; SIDE EFFECT: Destructively undoes the most recent move in the 
;;;              move history
;;;


(defun undo-move! (g)
  (cond
   ;; Case 1:  No moves in move history!
   ((null (chequers-move-history g))
    ;;(format t "Can't undo move... empty history!~%")
    g)
   
   ;; Case 2:  There is a move to undo...
   (t
    (let* ((move (pop (chequers-move-history g)))
	   (bored (chequers-board g))
	   (r1 (first move))
	   (c1 (second move))
	   (r2 (third move))
	   (c2 (fourth move))
	   (marble (aref bored r2 c2))
	   (owner (marble-owner marble)))
      (when (aref bored r1 c1)
	(format t "Gonna undo move, but something was on source square!~%"))
      (when (null marble)
	(format t "Wanna undo a move, but there's no piece at destn!~%"))
      ;; remove piece from (r2,c2)
      (setf (aref bored r2 c2) nil)
      ;; move piece back to (r1,c1)
      (setf (aref bored r1 c1) marble)
      (setf (marble-row marble) r1)
      (setf (marble-col marble) c1)
      ;; recalculate the distance of the marble from the goal zone
      (setf (marble-distance marble) (distance-formula r1 c1 (first (nth owner *goal-spaces*))
                                                       (second (nth owner *goal-spaces*))))
      ;; Toggle the turn!
      (toggle-turn! g)
      ;; Return the CHEQUERS struct
      g))))



;;;
;;; GAME-OVER?
;;; -------------------------------------------
;;; INPUTS: GAME, the chequers struct
;;; OUTPUTS: nil or a statement denoting who wins
;;;
;;;

(defun game-over? (game)
  (let* ((over? T)
	 (marbles (chequers-marbles game)))
    
    ;; iterate through the players
    (dolist (plyr *PLAYERS*)
      
      ;; iterate through the marbles
      (dotimes (x 10)
	(let* ((marb (aref marbles plyr x))
	       (row (marble-row marb))
	       (col (marble-col marb)))
	  
	  ;; if the marble is not in the goal zone, set over? to nil
	  (when (not (member? (list row col) (nth plyr *HOME-ZONES*)))
	    (setf over? nil))))
      
      ;; if the game is over, return the winner
      (when over?
	(return-from game-over?
	  (cond ((eq *green* plyr) "green")
		((eq *blue* plyr) "blue"))))
      
      ;; reset over? for the next player check
      (setf over? T)))
  
  ;; otherwise return nil
  nil)      


;;; MEMBER? (helper function)
;;; -----------------------------------
;;;  INPUTS: LISTX, a list
;;;          LISTY, a list of lists
;;;  OUTPUTS: T or NIL, T if listx is a member of listy


(defun member? (listx listy)
  (dolist (inner-list listy)
    (when (equal listx inner-list)
      (return-from member? T)))
  nil)



;;; GET-POTENTIAL-MOVES
;;; ------------------------------------------------
;;; INPUTS: R,C, the row and column of the marble
;;; OUTPUTS: a list containing the row and column
;;;          indices of spaces in all legal move 
;;;          directions.
;;;

(defun get-potential-moves (r c)
  (let ((pot-moves '())) ;; list to hold the potential moves
    (if (evenp r)
	;; append the potential moves for an even row
	(setf pot-moves
              (list
               (list (- r 1) c)
               (list (- r 1) (+ c 1))
               (list r (- c 1))
               (list r (+ c 1))
               (list (+ r 1) c)
               (list (+ r 1) (+ c 1))))
      ;; otherwise append the potential moves for an odd row
      (setf pot-moves 
            (list
             (list (- r 1) c)
             (list (- r 1) (- c 1))
             (list r (- c 1))
             (list r (+ c 1))
             (list (+ r 1) c)
             (list (+ r 1) (- c 1)))))
    
    (dolist (spc pot-moves)
      (when (or 
	     (member? spc *illegal-spaces*)  ;; remove moves that are to illegal spaces
	     (not (on-board? (first spc) (second spc))))
	(setf pot-moves (remove spc pot-moves)))) 
    
    ;; return potential moves list
    pot-moves))


;;;
;;; GET-ROW-COL-JUMPED-TO
;;; ----------------------------------------------
;;; INPUTS: R, C, the row and column of the jumper
;;;         LOC, the location of the jumpee
;;; OUTPUTS: a list containing the row and column of the
;;;          resulting landing.
;;;

(defun get-row-col-jumped-to (r c loc)
  (let ((row (first loc))  ;; row of the marble to be jumped
	(col (second loc)) ;; col of the marble to be jumped
	(destnr -1)        ;; row index to be recursively passed
	(destnc -1))       ;; col index to be recursively passed
    (cond (
	   ;; cases where the jumped marble is in the same row
	   (and (eq row r) (eq col (- c 1)))
	   (setf destnr r)
	   (setf destnc (- c 2)) )
	  (
	   (and (eq row r) (eq col (+ c 1)))
	   (setf destnr r)
	   (setf destnc (+ c 2)) )
	  
	  ;; cases where the jumped marble is in the row index 1 less than
	  ;; the marble jumping it.
	  
	  ( (or (and (eq row (- r 1)) (eq col c) (evenp r))
		(and (eq row (- r 1)) (eq col (- c 1)))) ;; odd row
	    (setf destnr (- r 2))
	    (setf destnc (- c 1)))
	  ( (or (and (eq row (- r 1)) (eq col c) (oddp r))
		(and (eq row (- r 1)) (eq col (+ c 1)))) ;; even row
	    (setf destnr (- r 2))
	    (setf destnc (+ c 1)))
	  
	  ;; cases where the jumped marble is in the row index 1 more than
	  ;; the marble jumping it.
	  
	  ( (or (and (eq row (+ r 1)) (eq col c) (evenp r))
		(and (eq row (+ r 1)) (eq col (- c 1)))) ;; odd row
	    (setf destnr (+ r 2))
	    (setf destnc (- c 1)))
	  ( (or (and (eq row (+ r 1)) (eq col c) (oddp r))
		(and (eq row (+ r 1)) (eq col (+ c 1)))) ;; even row
	    (setf destnr (+ r 2))
	    (setf destnc (+ c 1))))
    
    (list destnr destnc)))



;;; ON-BOARD?
;;; -------------------------------------------------------------
;;; INPUTS: R,C, row and column of the marble
;;; Returns T if the marble is on the game board, NIL otherwise.

(defun on-board? (r c)
  (and 
   (and (< c 13) (>= c 0))    ;; column bounds
   (and (< r 17) (>= r 0))))  ;; row bounds


;; LEGAL-MOVES
;; --------------------------------------------------------------
;;  INPUTS: GAME, chequers struct
;;          R, C, position of the marble to get legal moves for
;;          FIRST-MARB?, a boolean flag
;;          LEG-MOVES, list of legal moves to be populated
;;          JUMPED-SLOTS, list of jumped marble locations
;;  OUPUTS: LEG-MOVES, list of legal moves for this marble
;;

(defun legal-moves (game r c first-marb? leg-moves jumped-to-spaces)
  (let* ((board (chequers-board game))
	 (pot-moves (get-potential-moves r c)))
    ;; print the current marble
    ;; (format t "Current Marble: ~A~%" (list r c))
    ;; On a non-recursive call record the row and column of the marble
    (when first-marb?
      (setf *row* r)
      (setf *col* c))
    ;; for each potential move
    (dolist (loc pot-moves)
      ;; (format t "potential move: ~A~%" loc)
      (let ((spc (aref board (first loc) (second loc))))
	
	;; if space is NIL and this is the first marble, i.e. not a recursive call
	(cond ( (and (not (typep spc 'marble)) first-marb?)  
		;; add the slot to the legal moves list
		(setf leg-moves (cons (list r c (first loc) (second loc)) leg-moves)))
              ;;(format t "adding to leg-moves: ~A~%" (list r c (first loc) (second loc))))
	      
              ;; otherwise, if space is a marble, and hasnt been jumped
              (T
               (when (and (typep spc 'marble) (not (member? loc jumped-to-spaces)))
                 (let ((destination (get-row-col-jumped-to r c loc)))
                   
		   
                   ;; (format t "might add: ~A~%" (list r c (first destination) (second destination)))
		   
                   ;; recursive call on the space jumped to
                   (when (and 
                          
                          ;; space is legal
                          (not (member? destination *illegal-spaces*))
                          
                          ;; neither row nor column is negative
                          (on-board? (first destination) (second destination))
                          
                          ;; space is empty
                          (not (typep (aref board (first destination) (second destination)) 'marble))
                          
                          ;; space has not been jumped to
                          (not (member? destination jumped-to-spaces)))

                     ;; Make the recursive call

                     ;;(format t "Recursive call~%")
                     ;;(format t "jumped-to-spaces: ~A" jumped-to-spaces)
                     (setf leg-moves (append leg-moves 
                                             (legal-moves game 
                                                          (first destination) 
                                                          (second destination)
                                                          nil 
                                                          (setf leg-moves (cons (list *row*
                                                                                      *col*
                                                                                      (first destination)
                                                                                      (second destination))
                                                                                leg-moves)) 
                                                          (setf jumped-to-spaces 
                                                                (cons destination jumped-to-spaces)))))))))))))
  
  ;; return legal moves list
  leg-moves)



;;; GET-ALL-LEGAL-MOVES
;;; --------------------------------------------------
;;; INPUTS: G, a CHEQUERS struct
;;; OUTPUTS: List of all possible legal moves for every
;;;          marble of the player whose turn it is
;;;

(defun get-all-legal-moves (g use-rand?)
  (if use-rand? 
      ;; make a call to the randomized version of the legal moves list
      (gen-rand-legal-moves-list g)
    ;; otherwise, generate the non-random moves list
    (let ((marbles (chequers-marbles g))
	  (curr-player (chequers-whose-turn? g))
	  (all-legal-moves '()))
      (dotimes (x 10)
	(let* ((marb (aref marbles curr-player x))
	       (row (marble-row marb))
	       (col (marble-col marb))
	       (moves (legal-moves g row col T '() '())))
	  (dolist (move moves)
	    (when (not (member? move all-legal-moves))
	      (setf all-legal-moves (cons move all-legal-moves))))))
      all-legal-moves)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;------------------------  MINIMAX WITH ALPHA-BETA PRUNING  -----------------------;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;  COMPUTE-MOVE
;;; -------------------------------------------------------------
;;;  INPUT:  G, a CHEQUERS struct
;;;          USE-RAND?, use a randomly sorted version of the moves list?
;;;  OUTPUT:  The best move according to MINIMAX with ALPHA-BETA
;;;   pruning, using the distance eval func, EVAL-FUNC.  Searches to
;;;   a depth of *CUTOFF-DEPTH*.

(defun compute-move (g use-rand?)
  (cond
   ;; Case 1:  Game already over, nothing to do
   ((game-over? g)
    (format t "Game is over!  Sorry dude!~%")
    nil)
   ;; Case 2:  Game still on, compute best move...
   (t
    ;; Reset global counter
    (setf *num-moves* 0)
    ;; Call COMPUTE-MAX with init alpha/beta values
    (let ((best-move (compute-max g 0 *neg-inf* *pos-inf* use-rand?)))
      ;; Report number of moves considered...
      (format t "NUM-MOVES: ~A~%" *num-moves*)
      best-move))))






;;;  COMPUTE-MAX
;;; ---------------------------------------------------------------
;;;  INPUTS:  G, a CHEQUERS struct
;;;           CURR-DEPTH, the current depth in the search
;;;           ALPHA, BETA, alpha/beta values for this node in search
;;;           USE-RAND?, use a randomly sorted moves list?
;;;  OUTPUT:  If CURR-DEPTH is zero, returns best move
;;;           Otherwise returns value of this node according
;;;           to MINIMAX with ALPHA-BETA pruning.

(defun compute-max (g curr-depth alpha beta use-rand?)
  (let ((best-move-so-far nil))
    (cond
     ;; Base Case 1:  Game over
     ((game-over? g)
      ;; MAX player LOSES
      *loss-value*)
     
     ;; Base Case 2:  at the cutoff depth
     ((>= curr-depth *cutoff-depth*)
      ;; use the distance evaluation func
      (eval-func g))
     
     ;; Recursive Case:  Need to do minimax with alpha-beta pruning
     (t
      (let* ((moves (get-all-legal-moves g use-rand?))) ;; RANDOMIZE
	;; do
	(dolist (mv moves)
	  (incf *num-moves*)
	  (apply #'do-move! g nil mv)
	  (let ((child-val (compute-min g (1+ curr-depth) alpha beta use-rand?)))
	    (undo-move! g)
	    ;; Check for updating ALPHA value...
	    (when (> child-val alpha)
	      (setf alpha child-val)
	      (setf best-move-so-far mv)
	      ;; Check for pruning ...
	      (when (<= beta alpha)
		;; Hey! PRUNE!  Forget about any remaining moves in 
		;;  this DOLIST... We're outta here!!
		(return-from compute-max 
		  ;; Need to return BEST-MOVE if we're at depth 0
		  ;; Otherwise return ALPHA value
		  (cond ((zerop curr-depth)
			 (format t "ROOT NODE ALPHA: ~A~%" alpha)
			 best-move-so-far)
			(t
			 alpha)))))))
	;; return alpha or best-move-so-far, depending on whether
	;; we're at depth 0 or not
	(cond
	 ((zerop curr-depth)
	  (format t "ROOT NODE ALPHA: ~A~%" alpha)
	  best-move-so-far)
	 (t
	  alpha)))))))




;;;  COMPUTE-MIN
;;; -------------------------------------------------------
;;;  INPUTS:  G, a CHEQUERS struct
;;;           CURR-DEPTH, the depth of this MIN node
;;;           USE-RAND?, use a randomly sorted version of the moves list?
;;;           ALPHA, BETA, values received from parent MAX node
;;;  OUTPUT:  The value of this MIN node according to rules
;;;           of MINIMAX with ALPHA-BETA pruning

(defun compute-min (g curr-depth alpha beta use-rand?)
  (cond
   ;; Game Over
   ((game-over? g)
    ;; Win for MAX node
    *win-value*)
   
   ;; Hit the cutoff depth
   ((>= curr-depth *cutoff-depth*)
    ;; Use the EVAL-FUNC
    (eval-func g))
   
   ;; RECURSE
   (t
    (let* ((moves (get-all-legal-moves g use-rand?)))
      (dolist (mv moves)
	(incf *num-moves*) ;; increment the number of moves explored
	;; do the move
	(apply #'do-move! g nil mv)
	;; make recursive call on child node
	(let ((child-val (compute-max g (1+ curr-depth) alpha beta use-rand?)))
	  ;; undo the move
	  (undo-move! g)
	  ;; Update BETA value if necessary...
	  (when (< child-val beta)
	    (setf beta child-val)
	    ;; Check for ALPHA-BETA PRUNING
	    (when (<= beta alpha)
	      ;; PRUNE
	      (return-from compute-min beta)))))
      ;; return beta 
      ;; NOTE:  Depth can't be zero for a MIN node
      beta))))




;;; MULTI-EVAL-FUNC (UNUSED)
;;; -----------------------------------------------------
;;; INPUTS: G, a chequers struct
;;; OUTPUT: The distance function evaluation of the current
;;;         state of the game based on the difference in
;;;         total distances of marbles from the goal zone.
;;;         e.g. it is green's turn: -(green's total distances)
;;;         -  -(blue's total distance's).
;;;

(defun multi-eval-func (g)
  (let* ((plyr (chequers-whose-turn? g)) ;; player
	 (two (cycle-plr plyr))          ;; 2nd plyr
	 (three (cycle-plr two))         ;; 3rd plyr
	 (four (cycle-plr three))        ;; 4th plyr
	 (total-plr (svref (chequers-eval-subtotals g) plyr))
	 (total-two (svref (chequers-eval-subtotals g) two))
	 (total-three (svref (chequers-eval-subtotals g) three))
	 (total-four (svref (chequers-eval-subtotals g) four)))
    ;; see EVAL-FUNC for explanation
    (- (* -1 total-plr) (* -1 total-two) (* -1 total-three) (* -1 total-four))))




;;; EVAL-FUNC
;;; -----------------------------------------------------
;;; INPUTS: G, a chequers struct
;;; OUTPUT: The distance function evaluation of the current
;;;         state of the game based on the difference in
;;;         total distances of marbles from the goal zone.
;;;         e.g. it is green's turn: -(green's total distances)
;;;         -  -(blue's total distance's).
;;;

(defun eval-func (g)
  (let* ((plyr (chequers-whose-turn? g)) ;; player
	 (other (other-plr plyr))        ;; opponent
	 (total-plr (svref (chequers-eval-subtotals g) plyr))     ;; player's total
	 (total-other (svref (chequers-eval-subtotals g) other))) ;; opponent's total
    ;; the player wants to maximize [- player total] - [- opponent total] since
    ;; a higher total implies the marbles are further from the goal space.
    ;; e.g. [-100] - [-102] = 2. The player likes this because 
    ;; the opponent's 102 > player's 100.
    (- (* -1 total-plr) (* -1 total-other)))) 




;;;  CYCLE-PLR (UNUSED)
;;; --------------------------------------------------------
;;;  INPUT:  PLR, either *green* or *blue* or *red* or *violet*
;;;  OUTPUT:  The next player

(defun cycle-plr (plr)
  (if (eq plr *violet*)
      *green*
    (+ 1 plr)))




;;;  OTHER-PLR 
;;; --------------------------------------------------------------
;;;  INPUT: PLR, either *green* or *blue*
;;;  OUTPUT: The other player

(defun other-plr (plr)
  (- 1 plr))




;;; SET-SUBTOTALS
;;; ---------------------------------------------
;;; INPUT: G, a chequers struct
;;;        PLYR, the current player whose turn it is
;;; OUTPUT: N/A
;;; SIDE EFFECT: modify the eval-subtotals field of the
;;;              chequers struct

(defun set-subtotals (g plyr)
  (let* ((marbles (chequers-marbles g))
	 (total 0))
    (dotimes (x 10)
      ;; access the marbles array by player and for each marble
      (let* ((marb (aref marbles plyr x)))
	;; increment the total
	(setf total (+ total (marble-distance marb)))))
    ;; set the total distances of the player's marbles from goal space
    (setf (svref (chequers-eval-subtotals g) plyr) total)))




;;; DISTANCE-FORMULA
;;; -------------------------------------------------
;;; INPUTS: X1, Y1, coordinates of the marble
;;;         X2, Y2, coordinates of the destination
;;; OUTPUT: The distance between the two points as derived
;;;        from the pythagorean theorem. 

(defun distance-formula (x1 y1 x2 y2)
  (if (oddp x1)
      ;; if the row is odd, subtract one from the column to make up for the row offset of the board
      (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 (- y1 1)) 2)))
    ;; otherwise just use the normal distance formula
    (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))
;; sqrt[ (x2-x1)^2 + (y2-y1)^2 ]




;;; ====================================================
;;; GEN-RAND-LEGAL-MOVES-LIST
;;; 
;;; returns a randomized version of the legal moves list
;;; ====================================================

(defun gen-rand-legal-moves-list (g)
  (let* ((moves (get-all-legal-moves g nil))
	 (new-moves (copy-list moves))
	 (len (length new-moves))
	 (index-list (gen-rnd-helper len)))
    (dotimes (x len new-moves)
      (setf (nth x new-moves) (nth (nth x index-list) moves)))))

;;; ============================================
;;;              GEN-RND-HELPER
;;; returns a list containing the randomly sorted
;;; indices of the input list
;;; =============================================

(defun gen-rnd-helper (len)
  (let* ((new-list '()))
    (while (not (eq (length new-list) len))
      (let ((rnd-index (random len)))
	(when (not (member rnd-index new-list))
	  (setf new-list (cons rnd-index new-list)))))
    new-list))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;-------------------------  MARBLE MOVEMENT --------------------------------;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;; HARD-MOVE
;;; ----------------------------------------------------------
;;; Hard-move, named for the "hard" coded nature of specifying
;;; exactly the row and column of the destination


(defun hard-move (game r1 c1 r2 c2)
  (do-move! game T r1 c1 r2 c2))


;;; MOVE 
;;; -------------------------------------------------------
;;; Use 6 cardinal directions to move the marbles one space
;;; or to jump one marble.
;;;
;;; Turn ends after the players uses this function.
;;; For multiple jumps use hard-move

(defun move (game move r1 c1)
  (let ((r2 -1)
	(c2 -1))
    (cond 
     ;; move North East
     ((eq move NE)(setf r2 (- r1 1))(setf c2 (if (evenp r1)
						 (+ c1 1)
					       c1)))
     ;; move North West
     ((eq move NW)(setf r2 (- r1 1))(setf c2 (if (evenp r1)
						 c1
					       (- c1 1))))
     ;; move East
     ((eq move E)(setf r2 r1)(setf c2 (+ c1 1)))
     ;; move West
     ((eq move W)(setf r2 r2)(setf c2 (- c1 1)))
     ;; move South East
     ((eq move SE)(setf r2 (+ r1 1))(setf c2 (if (evenp r1)
						 (+ c1 1)
					       c1)))
     ;; move South West
     ((eq move SW)(setf r2 (+ r1 1))(setf c2 (if (evenp r1)
						 c1
					       (- c1 1)))))
    ;; if the destination space has a marble in it, jump it
    (if (typep (aref (chequers-board game) r2 c2) 'marble)
	(do-move! game T r1 c1 
		  (first (get-row-col-jumped-to r1 c1 (list r2 c2)))
		  (second (get-row-col-jumped-to r1 c1 (list r2 c2))))
      ;; otherwise just do the move
      (do-move! game T r1 c1 r2 c2))))


;;; COMP-DO-MOVE!
;;; ---------------------------------------------------
;;; INPUT: G, a chequers struct
;;; OUTPUT: N/A
;;; SIDE EFFECT: The AI does a move

(defun comp-do-move! (g use-rand?)
  (let ((move (compute-move g use-rand?)))
    (do-move! g nil
	      (first move)
	      (second move)
	      (third move)
	      (fourth move)))
  g)


;;; COMP-PLAY-GAME

(defun comp-play-game (g rand-one? rand-two?)
  (dotimes (x 10)
    (comp-do-move! g rand-one?)
    (comp-do-move! g rand-two?)
    (print x))
  (print g))

;;(in-package :profiler)

