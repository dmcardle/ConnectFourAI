; Dan McArdle
; CSCI 276 - Lisp
; Connect Four!
; 20 Feb. 2013

; _ _ _ _ _ _ _
;| | | | | | | |     
;| | | | | | | |     
;| |0|1|1| | | |     
;|0|1|1|1| | | |     
;|1|1|0|0| | | |     
;|1|0|0|1|0|0| |     
;---------------

(defvar *player1Goes* t)
(defvar *numRows* 6)
(defvar *numCols* 7)

(defun createBoard (cols rows)
    "Create a list of `rows` lists containng `cols` elements each"

    (defun createRow (cols)
        (cond
            ((eq cols 0) NIL)
            (T (append (createRow (- cols 1)) '(~)))))

    (cond
        ((eq rows 0) NIL)
        (T (append (createBoard cols (- rows 1))  (list (createRow cols)) ))))

(defun printRow (row)
    "Print a single list like  | x0 | x1 | ... | xn |"

    (cond
        ((eq (length row) 0)
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
    ;; adjust for numbers displayed to user starting at 1
    (setf colNum (- colNum 1))
    
    ;; get column from board
    (setf col
        (reverse
            (map 'list #'(lambda (row) (nth colNum row)) board)))
    
    ;; find highest empty element
    (setf rowNum (position '~ col))

    ;; decide which symbol to use
    (setf sym (cond
        (*player1Goes* 'A)
        (t 'B)))


    ;; set the value at the position
    (if (null rowNum)
        (progn
            (format t "Column is full~%")
            (takeTurn board))

        ;; else
        (progn
            (setf rowNum (- (- *numRows* rowNum) 1)) 
            (setf (nth colNum (nth rowNum board)) sym)))

    (setf *player1Goes* (not *player1Goes*)))


(defun gameOver-p (board)
    ;; check diagonal
    ;; check horizontal
    ;; check vertical
)


(defun askUserForCol ()
 (format t "Enter col num: ")
 (let ((val (read)))
  (if (and
        (numberp val)
        (> val 0)
        (< val *numCols*))  val

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
    (takeTurn board))


(setf board (createBoard *numCols* *numRows*))
(takeTurn board)
