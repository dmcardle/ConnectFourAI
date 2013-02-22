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
(defvar *numRows* 6)
(defvar *numCols* 7)

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
    (cond
        ((null row)
            (format t "|~%"))
        (t (progn
            (format t "| ~S " (car row))
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

(defun makeMove (board colNum)
    ;; decide which symbol to use
    (setf sym (cond
        (*player1Goes* 'A)
        (t 'B)))

    ;; adjust for numbers displayed to user starting at 1
    (setf colNum (- colNum 1))
    
    ;; extract column from board
    (setf col
        (reverse
            (map 'list #'(lambda (row) (nth colNum row)) board)))
    
    ;; find highest empty element in column
    (setf rowNum (position '~ col))

    ;; set the value at the position
    (if (null rowNum)
        (progn
            ;; if col is full, repeat without switching to other player's turn
            (format t "Column is full~%")
            (takeTurn board))

        ;; else
        (progn
            (setf rowNum (- (- *numRows* rowNum) 1)) 
            (setf (nth colNum (nth rowNum board)) sym)))

    ;; other player's turn now
    (setf *player1Goes* (not *player1Goes*)))



(defun gameOver-p (board)
    (defun checkList (l)
        "checks a list of pieces for a group of four in a row"
        (if (>= (length l) 4)
            (progn
                (setf seq (subseq l 0 4))
                (or
                    (equal seq '(A A A A))
                    (equal seq '(B B B B))
                    (checkList (cdr l))))))

    ;; check diagonal
    (defun checkDiagonal (b)
        (defun range (bot top)
          "Generates a list of numbers in range [bot top) -- that is, bot is
          included and top is excluded from the resulting list"
          (loop for i from bot to (- top 1) collect i))

        (if (and
             (>= (length b) 4)
             (>= (length (car b)) 4))
                (or
                    (checkList
                        (map 'list #'(lambda (i) (nth i (nth i b))) (range 0 4)))
                    (checkDiagonal (cdr b))
                    (checkDiagonal (map 'list #'cdr b)))))

            ;(map 'list #'(lambda (row) (nth colNum row)) board)))

    ;; check horizontal
    (defun checkHorizontal (b)
        "checks each row of board b"
        (if (not (null b))
            (or
                (checkList (car b))
                (checkHorizontal (cdr b)))))
            
    ;; check vertical
    (defun checkVertical (b)
        "checks each column of board b"
        (if (not (null (car b)))
            (or
                (checkList (map 'list #'car b))
                (checkVertical (map 'list #'cdr b))))) 

    (or
        (checkHorizontal board)
        (checkVertical board)
        (checkDiagonal board)
        (checkDiagonal (map 'list #'reverse board))))


(defun askUserForCol ()
 (format t "Enter col num: ")
 (let ((val (read)))
  (if (and
        (numberp val)
        (> val 0)
        (<= val *numCols*))  val

    ;; otherwise
       (progn
        (format t "Please enter a number between 1 and ~S.~%" *numCols*)
        (askUserForCol)))))

(defun takeTurn (board )

    (format t
        (cond
            (*player1Goes*
                "~%          PLAYER 1~%")
            (t  "~%          PLAYER 2~%")))

    (printBoard board)
    (setf colChoice (askUserForCol))
    (makeMove board colChoice)

    ;; recurse
    (cond
        ((gameOver-p board)
            (progn
                (format t "~%~%GAME OVER, ")
                (if *player1Goes*
                    (format t "PLAYER 2 WINS~%")
                    (format t "PLAYER 1 WINS~%"))
                (printBoard board)))
        (t (takeTurn board))))


(format t "~%~%     LET THE GAMES BEGIN!!!!!!!!~%~%")
(setf board (createBoard *numCols* *numRows*))
(takeTurn board)

