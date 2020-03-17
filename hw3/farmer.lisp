(defvar *movesf* '(L R LC LG LW RC RG RW)) ;; Variable for farmer moves
(defvar *startf* '(L 1 1 1)) ;; Farmer start state
(defvar *goalf* '(R 0 0 0)) ;; Goal for farmer puzzle
(defvar *heuristicf* 'weightf)


;; Moves the farmer to left of the river
(defun L (state)
    (if
        (null state)
        nil
        (let ((moved (cons 'L (cdr state))))
            (cond
                ((equal 'L (car state)) nil)
                ((equal moved '(L 0 0 0)) nil)
                ((equal moved '(L 0 0 1)) nil)
                ((equal moved '(L 1 0 0)) nil)
                (T (list moved))
            )
        )
    )
)

;; Moves the cabbage to the left of the river
(defun LC (state)
    (if
        (null state)
        nil
        (let ( (moved (list 'l  1 (caddr state) (cadddr state)) ) )
            (cond
                ((equal 'l (car state)) nil)
                ((equal '1 (cadr state)) nil)
                ((equal moved '(l 1 0 0)) nil)
                (T (list moved))
            )
        )
    )
)

;; Moves the goat to the left of river
(defun LG (state)
    (if
        (null state)
        nil
        (let ( (moved (list 'l (cadr state) 1 (cadddr state))) )
            (cond
                ((equal 'l (car state)) nil)
                ((equal '1 (caddr state)) nil)
                (T (list moved))
            )
        )
    )
)

;; Moves wolf to left of the river
(defun LW (state)
    (if
        (null state)
        nil
        (let ( (moved (list 'r (cadr state) (caddr state) 1)) )
            (cond
                ((equal 'l (car state)) nil)
                ((equal '1 (cadddr state)) nil)
                ((equal '(l 0 0 1) moved) nil)
                (T (list moved))
            )
        )
    )
)

;; Moves the farmer to the right of the river
(defun R (state)
    (if
        (null state)
        nil
        (let ((moved (cons 'r (cdr state))))
            (cond
                ((equal 'r (car state)) nil)
                ((equal moved '(r 1 1 1)) nil)
                ((equal moved '(r 1 1 0)) nil)
                ((equal moved '(r 0 1 1)) nil)
                (T (list moved))
            )
        )
    )
)

;; Moves the cabbage to the right of the river
(defun RC (state)
    (if
        (null state)
        nil
        (let ( (moved (list 'r  0 (caddr state) (cadddr state)) ) )
            (cond
                ((equal 'r (car state)) nil)
                ((equal '0 (cadr state)) nil)
                ((equal moved '(r 0 1 1)) nil)
                (T (list moved))
            )
        )
    )
)

;; Moves the goat to the right of the river
(defun RG (state)
    (if
        (null state)
        nil
        (let ( (moved (list 'r (cadr state) 0 (cadddr state))) )
            (cond
                ((equal 'r (car state)) nil)
                ((equal '0 (caddr state)) nil)
                (T (list moved))
            )
        )
    )
)

;; Moves the wolf to the right of the river
(defun RW (state)
    (if
        (null state)
        nil
        (let ( (moved (list 'r (cadr state) (caddr state) 0)) )
            (cond
                ((equal 'r (car state)) nil)
                ((equal '0 (cadddr state)) nil)
                ((equal '(r 1 1 0) moved) nil)
                (T (list moved))
            )
        )
    )
)

(defun weightf (state)
    (+ (caar state) (caaadr state))
)
