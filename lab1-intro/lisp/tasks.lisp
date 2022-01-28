;; code for Task 1.2(a):
(print (1 + 2 - 3))
;; the above expression throws an error
;; Answer:
(print(- (+ 1 2) 3))

;; code for Task 1.2(b)
(defvar x 55)
(cond 
  ((< x 10) (format t "number below 10"))
  ((< x 50) (format t "number below 50"))
  (t (format t "number greater or equal 50"))
)
; TODO: convert the cond macro in a nested if-then-else
; Answer:
(defvar x 55)
(if (< x 10))
    (format t "number below 10")
(if (< x 50))
    (format t "number below 50")
(format t "number greater or equal 50")))

;; code for Task 1.2(c):
(setf prg '(+ 1 n)) ; define a very simple program
(print prg) ; print the program
; TODO: executethe program with n = 1 and print its result
; Answer:
; this sets variable n to 1
(setf n 0)
(setf prg (+ 1 n))
(print prg)