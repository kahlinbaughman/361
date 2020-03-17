(defvar *movesj* '(f5 f3 e5 e3 3t5 5t3)) ;; Variable for farmer moves
(defvar *startj* '(0 0)) ;; Farmer start state
(defvar *goalj* '(0 4)) ;; Goal for farmer puzzle
(defvar *heuristicj* 'weightj)

;; Empty 5 gallon
(defun e5 (state)
    (list (list (car state) 0))
)

;; Empty 3 gallon
(defun e3 (state)
    (list (list 0 (cadr state)))
)

;; Fill 5 gallon
(defun f5 (state)
    (list (list (car state) 5))
)

;; Fill 3 gallon
(defun f3 (state)
    (list (list 3 (cadr state)))
)

;; Fill the 3 gallon from the five gallon
(defun 5t3 (state)
    (cond
        ((null state) nil)
        ((< (car state) 0) nil)
        ((< (cadr state) 0) nil)
        ((<= 3 (+ (car state) (cadr state))) (list (list 3 (- (+ (cadr state) (car state)) 3))))
        (T (list (list (+ (car state)(cadr state)) 0)))
    )
)

(defun 3t5 (state)
    (cond
        ((null state) nil)
        ((< (car state) 0) nil)
        ((< (cadr state) 0) nil)
        ((<= 5 (+ (car state) (cadr state))) (list (list (- (+ (cadr state) (car state)) 5) 5)))
        (T (list (list 0 (+ (car state)(cadr state)))))
    )
)

(defun weightj (state)
    (+ (caar state) (caaadr state))
)
