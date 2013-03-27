;;; Dan McArdle
;;; CSCI 276 - Lisp
;;; Connect Four!
;;; 20 Feb. 2013

;;;  _ _ _ _ _ _ _
;;; | | | | | | | |     
;;; | | | | | | | |     
;;; | |0|1|1| | | |     
;;; |0|1|1|1| | | |     
;;; |1|1|0|0| | | |     
;;; |1|0|0|1|0|0| |     
;;; ---------------

(defvar *player1Goes* t)
(defvar *roboPlayer* -1) ;; which player is robot?
(defvar *numRows* 6)
(defvar *numCols* 7)


(defun range (bot top)
    "Generates a list of numbers in range [bot top) -- that is, bot is
    included and top is excluded from the resulting list"
    (loop for i from bot to (- top 1) collect i))


(defun createBoard (cols rows)
    "Create a list of `rows` lists containng `cols` elements each"

    (defun createRow (cols)
        (cond
            ((eq cols 0) NIL)
            (T (append (createRow (- cols 1)) (list '~ ) ))))

    (cond
        ((eq rows 0) NIL)
        (T (append (createBoard cols (- rows 1))  (list (createRow cols)) ))))


(defun printRow (row)
    "Print a single list (x0 x1 ... xn) like  | x0 | x1 | ... | xn |"

    ;; printing using terminal colors
    ;; see http://wynnnetherland.com/journal/a-stylesheet-author-s-guide-to-terminal-colors

    (cond
        ((null row)
            (format t "~C[00m|~%" #\Esc))
        (t (progn
            (cond
                ((eq (car row) '~)
                    (format t "~C[00m| - " #\Esc))
                (t
		 ;(format t "| ~S " (car row))
		 (cond
		   ((eq (car row) 'X)
		    (format t "~C[00m| ~C[33mX " #\Esc #\Esc))
		   ((eq (car row) 'O)
		    (format t "~C[00m| ~C[31mO " #\Esc #\Esc))
		   (t (format t "~C[00m| ~S " #\Esc (car row))))
		 ))
            (printRow (cdr row))))))


(defun printBoard (board)
    "Print the whole board out using `printRow`"
    (cond
        ((not (eq (length board) 0))
            (progn
                (printRow (car board))
                (printBoard (cdr board))))
        (t
            (progn 
                (printRow (list 1 2 3 4 5 6 7) )
                (format t "~%")))))


(defun getColumn (board colNum)
 "get column `colNum` from `board`"
 (reverse
  (map 'list #'(lambda (row) (nth colNum row)) board)))

(defun highestEmptyPos (col)
 "find the height at which to put a new piece"
 (position '~ col))

(defun colFull (col)
 "is the given column full?"
 (null (highestEmptyPos col)))


(defun makeMove (board colNum simulate)
    "Make a move on the given board in the given column. If `simulate` is T, we~
    will return 'FULL if column is full. Otherwise, we will notify the user~
    that the column is full and ask for a new column."
    ;; decide which symbol to use
    (setf sym (cond
        (*player1Goes* 'X)
        (t 'O)))

    ;; extract column from board
    (setf col (getColumn board colNum))
    
    ;; find highest empty element in column
    (setf rowNum (highestEmptyPos col))

    ;; set the value at the position
    (if (null rowNum)
        (if simulate
            'FULL
            (progn        
                
            ;; if col is full, repeat without switching to other player's turn
            (format t "Column is full~%")
            (takeTurn board)))

        ;; else
        (progn
            (setf rowNum (- (- *numRows* rowNum) 1)) 
            (setf (nth colNum (nth rowNum board)) sym))))


(defun checkGameOver (board)
    (defun checkList (l)
        "checks a list of pieces for a group of four in a row"
        (if (>= (length l) 4)
            (progn
                (setf seq (subseq l 0 4))
                (or
                    (equal seq '(X X X X))
                    (equal seq '(O O O O))
                    (checkList (cdr l))))))

    (defun checkDiagonal (b)
        "checks all downward diagonals for groups of four"
        (if (and
             (>= (length b) 4)
             (>= (length (car b)) 4))
                (or
                    (checkList
                        (map 'list #'(lambda (i) (nth i (nth i b))) (range 0 4)))
                    (checkDiagonal (cdr b))
                    (checkDiagonal (map 'list #'cdr b)))))

            ;(map 'list #'(lambda (row) (nth colNum row)) board)))

    (defun checkHorizontal (b)
        "checks each row of board b"
        (if (not (null b))
            (or
                (checkList (car b))
                (checkHorizontal (cdr b)))))
            
    ;(defun checkVertical (b)
    ;    "checks each column of board b"
    ;    (if (not (null (car b)))
    ;        (or
    ;            (checkList (map 'list #'car b))
    ;            (checkVertical (map 'list #'cdr b))))) 

    (defun checkVertical (b)
        (checkHorizontal (transpose b)))

    (defun transpose (board)
        (cond
            ((eq (length (car board)) 0)
                NIL)
            (t
                (cons
                    (mapcar #'car board)
                    (transpose (mapcar #'cdr board)) ))))

    (defun checkForDraw (b)
        "returns true if board is full"
        (cond
            ((not (null b))
                (and
                    ;; if this row is full
                    (null (position '~ (car b)))
                    (checkForDraw (cdr b))))
            (t t)))


    (cond
        ((checkForDraw board) 'DRAW)
        (t 

            ;; check all types of winning conditions
            (or
                (checkHorizontal board)
                (checkVertical board)
                (checkDiagonal board)

                ;; also check horizontally-flipped board so we catch the upward
                ;; diagonals
                (checkDiagonal (map 'list #'reverse board))))))


(defun askUserForCol ()
 (format t "Enter col num: ")
 (let ((val (read)))
  (if (and
        (numberp val)
        (> val 0)
        (<= val *numCols*))
      (progn
        ;; other player's turn now
        ;;(setf *player1Goes* (not *player1Goes*))
        (- val 1))

    ;; otherwise
       (progn
        (format t "Please enter a number between 1 and ~S.~%" *numCols*)
        (askUserForCol)))))

(defun askRobotForCol (board)

  ;;                           D E F E N S E
  ;;    take the move that will make us win
  ;;        OR
  ;;    take the move that will prevent the other player from winning
  (defun findWinningCol ()
    "For each column, move there and see if the game ends in a win"

    (defun winningCol (n)
      ;; create a copy of the current board to experiment on
      (setf sandbox (mapcar #'copy-list board))

      ;; if we are not testing row 0
      (if (not (eq n -1))
	  (progn
	    
	    (makeMove sandbox n T)
	    
	    (setf gameOverP (checkGameOver sandbox))
	    
	    ; if the game is not a draw and this move ended the game
	    (if (and (not (eq gameOverP 'DRAW)) gameOverP)
    		;; return this column
		(progn
		  (format t "[Steal]~%")
		  n)
		    ; otherwise, try again on the next column
    		(winningCol (- n 1))))

	  ; otherwise, admit that we don't know what to do
	  -1))
    
    (winningCol *numCols*))


  ; check for winning move for us
  (setf immediateWin (findWinningCol))  



  ;; switch move to other player, make moves on pretend board as him
  ;; check for winning move for other player
  ;; switch move back to us to leave the variable the way we found it
  (setf *player1Goes* (not *player1Goes*))
  (setf blockingMove (findWinningCol))
  (setf *player1Goes* (not *player1Goes*))

  ;;;                           O F F E N S E

  ;;; choose a random non-full column
  (defun offenseChoiceRand ()
    (format t "[Offense]~%")
    (setf choice (random *numCols*))
    (cond
      ((not (colFull (getColumn board choice)))
        choice)
      (t
        (offenseChoiceRand))))



  ;;; TODO implement Victor Allis' rules for perfect play
  (defun VICTOR ()
    

    ;;; CLAIMEVEN
    ;;;
    ;;; Required:   Two squares, directly above each other. Both squares should
    ;;;             be empty. The upper square must be even.
    ;;;
    ;;; Solutions:  All groups which contain the upper square.
    (defun claimeven ()
    )


    ;;; BASEINVERSE 
    ;;;
    ;;; Required:   Two directly playable squares.
    ;;;
    ;;; Solutions:  All groups which contain both squares.
    (defun baseinverse ()
    )

    ;;; VERTICAL 
    ;;;
    ;;; Required:   Two squares directly above each other. Both squares should
    ;;;             be empty. The upper square must be odd. 
    ;;;
    ;;; Solutions:  All groups which contain both squares. 
    (defun vertical ()
    )

    ;;; AFTEREVEN 
    ;;;
    ;;; Required:   A group which can be completed by the controller of the
    ;;;             Zugzwang, using only the even squares of a set of
    ;;;             Claimevens. This group is called the Aftereven group. The
    ;;;             columns in which the empty squares lie are called the
    ;;;             Aftereven columns.
    ;;;
    ;;; Solutions:  All groups which have at least one square in all Aftereven
    ;;;             columns, above the empty square of the Aftereven group in
    ;;;             that column. All groups which are solved by the Claimevens,
    ;;;             which are part of the Aftereven. 
    (defun aftereven ()
    )


    ;;; LOWINVERSE 
    ;;;
    ;;; Required:   Two different columns, called the Lowinverse columns. In
    ;;;             each Lowinverse column two squares, lying directly above
    ;;;             each other.
    ;;;             All four squares must be empty.
    ;;;             In both columsn the upper of the two squares is odd.
    ;;;
    ;;; Solutions:  All groups which contain both upper squares.
    ;;;             All groups which are solved by the Verticals, which are
    ;;;             part of the Lowinverse.
    (defun lowinverse ()
    )
    
    ;;; HIGHINVERSE 
    ;;;
    ;;; Required:   Two different columns, called the Highinverse columns. In
    ;;;             each Highinverse column three squares, lying directly above
    ;;;             each other.
    ;;;             All six squares are empty.
    ;;;             In both columns the upper square is even. 
    ;;;
    ;;; Solutions: 
    ;;;             All groups which contain the two upper squares.
    ;;;             All groups which contain the two middle squares.
    ;;;             All (vertical) groups which contain the two highest squares
    ;;;             of one of the Highinverse columns.
    ;;;
    ;;;             If the lower square of the first column is directly
    ;;;             playable:
    ;;;             All groups which contain both the lower square of the first
    ;;;             column and the upper square of the second column.
    ;;;
    ;;;             If the lower square of the second column is directly
    ;;;             playable:
    ;;;             All groups which contain both the lower square of the
    ;;;             second column and the upper square of the first column.
    (defun highinverse ()
    )

    ;;; BASECLAIM 
    ;;;
    ;;; Required:   Three directly playable squares and the square above the
    ;;;             second playable square. The non-playable square must be
    ;;;             even. 
    ;;;
    ;;; Solutions:  All groups which contain the first playable square and the
    ;;;             square above the second playable square.
    ;;;             All groups which contain the second and third playable
    ;;;             square. 
    (defun baseclaim ()
    )


    ;;; BEFORE
    ;;;
    ;;; Required:   A group without men of the opponent, which is called the
    ;;;             Before group.
    ;;;             All empty squares of the Before group should not lie in the
    ;;;             upper row of the board.  
    ;;;
    ;;; Solutions:  All groups which contain all squares which are successors
    ;;;             of empty squares in the Before group.
    ;;;             All groups which are solved by the Verticals which are part
    ;;;             of the Before. All groups which are solved by the
    ;;;             Claimevens which are part of the Before. 
    (defun before ()
    )


    ;;; SPECIALBEFORE
    ;;;
    ;;; Required:   A group without men of the opponent, which is called the
    ;;;             Specialbefore group.
    ;;;             A directly playable square in another column.
    ;;;             All empty squares of the Specialbefore group should not lie
    ;;;             in the upper row of the board. One empty square of the
    ;;;             Before group must be playable. 
    ;;;
    ;;; Solutions:  All groups which contain all successors of empty squares of
    ;;;             the Specialbefore group and the extra playable square.
    ;;;             All groups which contain the two playable squares.
    ;;;             All groups which are solved by one of the Claimevens.
    ;;;             All groups which are solved by one of the Verticals.
    (defun specialbefore ()
    )

  )
    


  (cond
    ;; first choice is the winning move for us, if it exists
    ((not (eq immediateWin -1))
      immediateWin)

    ;; otherwise, best choice is the move that prevents other player from winning
    ((not (eq blockingMove -1))
     blockingMove)
    
    ;; there's no way to win or screw the other guy, time for OFFENSE strategy
    (t (offenseChoiceRand))))

(defun takeTurn (board )

    (format t
        (cond
            (*player1Goes*
                "~%          PLAYER 1 (X)~%")
            (t  "~%          PLAYER 2 (O)~%")))

    (printBoard board)



    (cond
      ;; robot's turn
      ((or
        (and (eq *roboPlayer* 1) *player1Goes*)
        (and (eq *roboPlayer* 2) (not *player1Goes*)))
           (progn
             (setf colChoice (askRobotForCol board))
             (format t "Robbie plays column ~D~%" (+ 1 colChoice))))

      ;; user's turn, subtract one from user's input
      (t (setf colChoice (askUserForCol) )))
  
    (makeMove board colChoice NIL)
    ;; other player's turn now
    (setf *player1Goes* (not *player1Goes*))


    ;; recurse
    (setf gameStatus (checkGameOver board))

    (cond
        ((eq gameStatus 'DRAW)
            (progn
                (format t "~%~%DRAW: Nobody wins!~%~%"))
                (printBoard board))
        (gameStatus 
            (progn
                (format t "~%~%GAME OVER, ")
                (if *player1Goes*
                    (format t "PLAYER 2 WINS~%")
                    (format t "PLAYER 1 WINS~%"))
                (printBoard board)))
        (t (takeTurn board))))


(defun decideRobot ()
  "Ask user who should be the robot player. Sets value in global *roboPlayer* variable."
  (format t "Choose which player is robot.~%~
   Enter 0 for human vs. human game.~%~
   Enter 1 for robot vs. human game.~%~
   Enter 2 for human vs. robot game.~%::")
  (setf robbie (read))
  (if (not (and (>= robbie 0) (<= robbie 2)))
      (progn
	(format t "--- Invalid input.  Enter either 0, 1, or 2~%")
	(decideRobot))
      ;; otherwise
      (setf *roboPlayer* robbie)))


(defun main ()

  ;; seed the random number generator
  (setf *random-state* (make-random-state t))

  (format t "~%~%     SETUP~%~%")
  (decideRobot)

  (format t "~%~%     LET THE GAMES BEGIN!~%~%")
  (setf board (createBoard *numCols* *numRows*))
  (takeTurn board))


(main)
