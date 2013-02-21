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


; create a board that can hold 0s and 1s
(setq board (make-array '(7 6) :element-type '(mod 2))) 


