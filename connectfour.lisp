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


;(defun createBoard (cols rows)
;    "Create a list of `rows` lists containng `cols` elements each"
;
;    (defun createRow (cols)
;        (cond
;            ((eq cols 0) NIL)
;            (T (append (createRow (- cols 1)) (list '~ ) ))))
;
;    (cond
;        ((eq rows 0) NIL)
;        (T (append (createBoard cols (- rows 1))  (list (createRow cols)) ))))

(defun createBoard (cols rows)
    (make-array (list rows cols) :initial-element '~))


;(defun printRow (row)
;    "Print a single list (x0 x1 ... xn) like  | x0 | x1 | ... | xn |"
;
;    ;; printing using terminal colors
;    ;; see http://wynnnetherland.com/journal/a-stylesheet-author-s-guide-to-terminal-colors
;
;    (cond
;        ((null row)
;            (format t "~C[00m|~%" #\Esc))
;        (t (progn
;            (cond
;                ((eq (car row) '~)
;                    (format t "~C[00m| - " #\Esc))
;                (t
;         ;(format t "| ~S " (car row))
;         (cond
;           ((eq (car row) 'X)
;            (format t "~C[00m| ~C[33mX " #\Esc #\Esc))
;           ((eq (car row) 'O)
;            (format t "~C[00m| ~C[31mO " #\Esc #\Esc))
;           (t (format t "~C[00m| ~S " #\Esc (car row))))
;         ))
;            (printRow (cdr row))))))


;(defun printBoard (board)
;    "Print the whole board out using `printRow`"
;    (cond
;        ((not (eq (length board) 0))
;            (progn
;                (printRow (car board))
;                (printBoard (cdr board))))
;        (t
;            (progn 
;                (printRow (range 1 (+ 1 *numCols*))  )
;                (format t "~%")))))

(defun printBoard (board)

    ;; printing using terminal colors
    ;; see http://wynnnetherland.com/journal/a-stylesheet-author-s-guide-to-terminal-colors
    (defun printPiece( p )
        (cond
            ((eq p '~)
                (format t "~C[00m| - " #\Esc))
            ((eq p 'X)
                (format t "~C[00m| ~C[33mX " #\Esc #\Esc))
            ((eq p 'O)
                (format t "~C[00m| ~C[31mO " #\Esc #\Esc))))

    ;; print the elements in the board
    (dotimes (i *numRows*)
        (dotimes (j *numCols*)
            (printPiece (aref board i j)))
        (format t "|~%"))
    
    ;; print the column numbers at the bottom
    (defun printColNums (start i end)

        (cond ((eq i start)
            (format t "|")))
        
        (format t " ~S |" i)
            
        (cond ((eq i end)
            (format t "~%")))
        
        (cond ((< i end)    
            (printColNums start (+ 1 i) end))))

        
    (printColNums 1 1 *numCols*))





;(defun getColumn (board colNum)
; "get column `colNum` from `board`"
; (reverse
;  (map 'list #'(lambda (row) (nth colNum row)) board)))


(defun getColumn (board colNum)
    "get column `colNum` from `board`"
    (map 'list #'(lambda (row) (aref board colNum row)) (range 0 *numRows*)))


(defun highestEmptyPos (col)
 "find the height at which to put a new piece"
 (position '~ col))

(defun colFull (col)
 "is the given column full?"
 (null (highestEmptyPos col)))


(defun makeMove (board colNum player1goes simulate)
    "Make a move on the given board in the given column. If `simulate` is T, we~
    will return 'FULL if column is full. Otherwise, we will notify the user~
    that the column is full and ask for a new column."
    ;; decide which symbol to use
    (setf sym (cond
        (player1Goes 'X)
        (t 'O)))

    ;; extract column from board
    (setf col (getColumn board colNum))
    
    ;; find highest empty element in column
    (setf rowNum (highestEmptyPos col))

    ;; set the value at the position
    (if (null rowNum)
        (if simulate
            'FULL
            
        ;; otherwise
        (progn
          ;; if col is full, repeat without switching to other player's turn
          (format t "Column is full~%")
          (takeTurn board NIL)))

    ;; else
    (progn
     (setf rowNum (- (- *numRows* rowNum) 1)) 
     ;(setf (nth colNum (nth rowNum board)) sym)
     (setf (aref board colNum rowNum) sym))))


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

    ;(defun checkDiagonal (b)
    ;    "checks all downward diagonals for groups of four"
    ;    (if (and
    ;         (>= (length b) 4)
    ;         (>= (length (car b)) 4))
    ;            (or
    ;                (checkList
    ;                    (map 'list #'(lambda (i) (nth i (nth i b))) (range 0 4)))
    ;                (checkDiagonal (cdr b))
    ;                (checkDiagonal (map 'list #'cdr b)))))


    (defun checkFourInDiag (i j direction sym)

        (setf cVals (range i (+ i 4)))
        (setf rVals (range j (+ j 4)))

        (cond
            ((eq direction 'pos)
                (setf rVals (map 'list #'(lambda (x) (- *numRows* x)) rVals))))

        (defun check (cVals rVals)
            (cond
                ((not (null cVals))
                    (and 
                        (eq sym (aref board (car cVals) (car rVals)))
                        (check (cdr cVals) (cdr rVals))))))

        (check cVals rVals))


    (defun checkDiagonal ()
        (defun check (i j)
            (cond
                ((and (< i (- *numRows* 4)) (< j (- *numCols* 4)))
                    (or
                        (checkFourInDiag i j 'pos 'O)
                        (checkFourInDiag i j 'pos 'X)
                        (checkFourInDiag i j 'neg 'O)
                        (checkFourInDiag i j 'neg 'X)
                        (check (+ 1 i) j)
                        (check i (+ 1 j))))))
        (check 0 0))
            
        

     
    (defun checkFourInRow (i j sym)
        (and
            (eq sym (aref board i j))
            (eq sym (aref board i (+ 1 j)))
            (eq sym (aref board i (+ 2 j)))
            (eq sym (aref board i (+ 3 j)))))

    (defun checkFourInCol (i j sym)
        (and
            (eq sym (aref board i j))
            (eq sym (aref board (+ 1 i) j))
            (eq sym (aref board (+ 2 i) j))
            (eq sym (aref board (+ 3 i) j))))


    (defun checkFour (#'func rows cols)
        ;; supply either checkFourInRow or checkFourInCol as func

        (defun check (i j)
            (or 
                (funcall #'func i j 'O)
                (funcall #'func i j 'X)

                (cond
                    ((and (< i *numRows*) (< j (- *numCols* 4)))                
                        (check (+ 1 i) (+ 1 j)) )

                    (t NIL))))

        (check 0 0))
                    
     
    (defun checkHorizontal ()
        (checkFour #'checkFourInRow *numRows* (- *numCols* 4)))

    (defun checkVertical ()
        (checkFour #'checkFourInCol (- *numRows* 4) *numCols*))

    (defun checkForDraw ()
        "board is full if all the elements in the top row are full"
        (defun check (col)
            (cond
                ((< col *numCols*)
                    (and
                        (not (eq '~ (aref board col 0)))
                        (check (+ 1 col))))
                (T T)))
        
        (check 0))
                



    (cond
        ((checkForDraw) 'DRAW)
        (t 

            ;; check all types of winning conditions
            (or
                (checkHorizontal)
                (checkVertical)
                (checkDiagonal board)

                ;; also check horizontally-flipped board so we catch the upward
                ;; diagonals
                (checkDiagonal (map 'list #'reverse board))))))


(defun askUserForCol ()

  (format t "
                    ||\\||
                    |   |
                    |_ _|
                    | _ |
                    | ^ |
                     \\_/~%")



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

  (defun copyBoard (b)
    (mapcar #'copy-list b))

  ;;                           D E F E N S E
  ;;    take the move that will make us win
  ;;        OR
  ;;    take the move that will prevent the other player from winning
  (defun findWinningCol (player1goes)
    "For each column, move there and see if the game ends in a win"

    (defun winningCol (n)
      ;; create a copy of the current board to experiment on
      (setf sandbox (copyBoard board))

      ;; if we are not testing row 0
      (if (not (eq n -1))
        (progn
        
          (makeMove sandbox n player1goes T)
        
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
  (setf immediateWin (findWinningCol *player1goes*))  

  ;; check for winning move for other player
  (setf blockingMove (findWinningCol (not *player1goes*)))

  ;;;                           O F F E N S E

  ;;; choose a random non-full column
  (defun offenseChoiceRand ()
    (setf choice (random *numCols*))
    (cond
      ((not (colFull (getColumn board choice)))
       choice)
      (t
        (offenseChoiceRand))))


  (defun offenseChoiceThinkAhead ()

    ;; Create a list where the integer at position i represents
    ;; the number of nodes in the game tree which result in a win
    ;; for the current player following a move in column i
    (setf moves (map 'list #'(lambda (x) (* 0 x)) (range 0 *numCols*)))

    (defun thinkAhead (n board p1go tryCol)
      (cond
       ((> n 0) ;; we should recurse
        (progn 

         (loop for c in (range 0 *numCols*) do
          (progn
           ;; create a new copy of the board to try a move in column c
           (setf sandbox (copyBoard board))
           (setf tryMove (makeMove sandbox c p1go T))

           ;(format t "Trying ~S~%" c)
           ;(printBoard sandbox)

           (cond
            ;; column is full -- dead end, so its score is zero
            ((eq tryMove 'FULL)
             (progn
              ;(format t "column was full~%")
              ))

            (t ;; if we didn't hit a dead end

             (progn
               (setf gameOverStatus (checkGameOver sandbox))
             (cond
              ;; if game is over, this column is a success, so its score is one
              ((and
                (not (eq gameOverStatus 'DRAW))
                gameOverStatus)

                (cond 
                  ((or 
                    (and (eq *roboPlayer* 1)  p1go)
                    (and (eq *roboPlayer* 2) (not p1go)))
                
                   (progn
                    ;(format t "game is over and robot won!~%")
                    ;(printBoard sandbox)

                    ;; when making move in tryCol helps the robot, add one to
                    ;; tryCol's score
                    (setf (nth tryCol moves) (+ n (nth tryCol moves)))))

                  (t
                    (progn
                      ;(format t "game is over and human won!~%")

                      ;; when making move in tryCol helps the human, subtract
                      ;; one from tryCol's score
                      (setf (nth tryCol moves) (- (nth tryCol moves) n ))))))

              ;; if game is not over, switch to next player and recurse on this sandbox
              (t
               (progn
                
                ;(format t "recurse for next move~%")
                (thinkAhead (- n 1) sandbox (not p1go) tryCol)
                ;(format t "~%n = ~S~%MOVES = ~S~%~%" n moves)
                ))))))))))
       ;; if we should not recurse any more
       ;(t
       ; (progn
          ;(format t "end of the line~%")
       ;   -1))
       ))

    (format t"
    ___ O
 O-/   \\|
  /|||||\\
  |-----|
 / ----- \\
/|\\_____/|\\~%~%")

    (format t "Robbie is T H I N K I N G~%          ")
    (loop for tryCol from 0 to (- *numCols* 1) do
        
        (progn
         (setf sandbox (copyBoard board))
         (makeMove sandbox tryCol *player1goes* T)

         ;(format t "~S/~S~%" (+ 1 tryCol) *numCols* )
         (format t ". ")
         (thinkAhead 3 sandbox (not *player1goes*) tryCol)))

    (format t ".~%~%")
    ;(format t "THINKAHEAD ~S~%" moves)

    ;; return a random column until thinkAhead works correctly
    ;(offenseChoiceRand)



    (defun allPositions (needle haystack)
      "Find the position of all occurrences of needle in haystack"
      (defun searchFun (needle haystack pos)
        (if (not (null haystack))
          (cond
            ((eq needle (car haystack))
              (cons pos (searchFun needle (cdr haystack) (+ 1 pos))))
            (t (searchFun needle (cdr haystack) (+ 1 pos))))))

       (searchFun needle haystack 0))

    (setf bestCols (allPositions (reduce #'max moves) moves))

    (setf fullColumns (map 'list #'(lambda (c) (colFull (getColumn board c))) (range 0 *numCols* )))
    (setf fullColumns (allPositions t fullColumns))

    ;; remove the elements from bestCols which are full

    ;(format t "FULLCOLS = ~S~%" fullColumns)

    ;(format t "BESTCOLS = ~S~%" bestCols)

    (loop for c in fullColumns do
        (setf bestCols (remove c bestCols)))

    ;(format t "BESTCOLS = ~S~%" bestCols)

     
    ;; choose a random element out of the bestCols list
    (cond
        ((null bestCols)
            (offenseChoiceRand))
        (t
          (nth (random (length bestCols)) bestCols))))



  (cond
    ;; first choice is the winning move for us, if it exists
    ((not (eq immediateWin -1))
      immediateWin)

    ;; otherwise, best choice is the move that prevents other player from winning
    ((not (eq blockingMove -1))
     blockingMove)
    
    ;; there's no way to win or screw the other guy, time for OFFENSE strategy
    (t (offenseChoiceThinkAhead))))

(defun takeTurn (board verbose)

    (format t
        (cond
            (*player1Goes*
                "~%          PLAYER 1 (X)~%")
            (t  "~%          PLAYER 2 (O)~%")))

    (printBoard board)



    (cond
      ;; robot's turn
      ((or
        (eq *roboPlayer* 3)
        (and (eq *roboPlayer* 1) *player1Goes*)
        (and (eq *roboPlayer* 2) (not *player1Goes*)))
           (progn
             (setf colChoice (askRobotForCol board))
             (format t "Robbie plays column ~D~%" (+ 1 colChoice))))

      ;; user's turn, subtract one from user's input
      (t (setf colChoice (askUserForCol) )))
  
    (makeMove board colChoice *player1goes* NIL)
    ;; other player's turn now
    (setf *player1Goes* (not *player1Goes*))


    (setf gameStatusOver (checkGameOver board))
    (cond
        ( (eq gameStatusOver 'DRAW)
            (if verbose
              (progn
                (format t "~%~%DRAW: Nobody wins!~%~%")))
                (printBoard board))

        ( gameStatusOver 
            (if verbose
                (progn
                    (format t "~%~%GAME OVER: ")
                    (if *player1Goes*
                        (format t "PLAYER 2 (O) WINS~%")
                        (format t "PLAYER 1 (X) WINS~%"))
                    (printBoard board))))
        ;; recurse
        ( T (takeTurn board T))))


(defun decideRobot ()
  "Ask user who should be the robot player. Sets value in global *roboPlayer* variable."
  (format t "Choose which player is robot.~%~
   Enter 0 for human vs. human game.~%~
   Enter 1 for robot vs. human game.~%~
   Enter 2 for human vs. robot game.~%~
   Enter 3 for robot vs. robot game.~%::")
  (setf robbie (read))
  (if (not (and (>= robbie 0) (<= robbie 3)))
      (progn
    (format t "--- Invalid input.  Enter either 0, 1, 2, or 3~%")
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
  (takeTurn board T))


(main)
