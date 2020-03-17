;; Set variable called small to a default small value
(defparameter *small* 1)
;; Set variable called big to a default big value
(defparameter *big* 100)

;; Main function, starts the game and returns the floor of ((big + small)/2)
(defun guess-my-number ()
     (ash (+ *small* *big*) -1))

;; Sets the big variable to the returned value of guess-my-number minus one
;; Then calls guess-my-number
(defun smaller ()
     (setf *big* (1- (guess-my-number)))
     (guess-my-number))

;; Sets the small variable to the returned value of guess-my-number plus one
;; Then calls guess-my-number
(defun bigger ()
     (setf *small* (1+ (guess-my-number)))
     (guess-my-number))

;; Resets both variables to their defualt values and calls guess-my-number
(defun start-over ()
   (setf *small* 1)
   (setf *big* 100)
  (guess-my-number))

