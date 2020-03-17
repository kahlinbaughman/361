;; Checks to see if an item is a part of a list
;; Similar to what was done in class
(defun memberof (currn nodes)
   (cond
       ((null nodes) nil)
       ((equal currn (car nodes)) t)
       (t (memberof currn (cdr nodes)))
   )
)

;; Generates the children of a certain state, if the generated child or children
;; are already generated it ignores that child
(defun generate-kids (state moves open closed)
    (cond
        ((null moves) nil)
        ((null (funcall (car moves) state)) (generate-kids state (cdr moves) open closed)) ;; If the child generates an invalid state, ignore it
        ((memberof (funcall (car moves) state) closed) (generate-kids state (cdr moves) open closed)) ;; Check to see if the child has been generated and is on the closed list
        ((memberof (funcall (car moves) state) open) (generate-kids state (cdr moves) open closed)) ;; Checks to see if the child  has been generated and is on the open list
        (T (cons (funcall (car moves) state) (generate-kids state (cdr moves) open closed) )) ;; Recursively generates child or children
    )
)

;; BFS Helper function
(defun bfs (open closed goal moves)
    (format t "First on open list: ~A" (car open))
    (terpri) ;; Formates the output
    (cond
        ((null open) nil)
        ((equal (car open) goal) open) ;; Finds the goal
        (t
            (bfs (append (cdr open) (generate-kids (car open) moves open closed)) (cons (car open) closed) goal moves) ;; Recursively calls bfs
        )
    )
)

;; Function to call breadth first search
(defun breadth (start goal moves)
    (cond
        ((null start) (print "Start state is empty") nil)
        ((null goal) (print "Goal state is empty") nil)
        ((null moves) (print "Moves list is empty") nil)
        (T (bfs (list start) () goal moves))

    )
)

;; DFS helper function
(defun dfs (open closed goal moves)
    (format t "First on open list: ~A" (car open))
    (terpri) ;; Formatting
    (cond
        ((null open) nil)
        ((equal (car open) goal) ;; Finds the goal
            (format t "Length off open list: ~D" (list-length open))
            (terpri) ;; Formatting
        )
        (t
            (dfs (append (generate-kids (car open) moves open closed) (cdr open)) (cons (car open) closed) goal moves) ;; Recursively calls dfs
        )
    )
)

;; Function to call the depth first search
(defun depth (start goal moves)
    (cond
        ((null start) (print "Start state is empty") nil)
        ((null goal) (print "Goal state is empty") nil)
        ((null moves) (print "Moves list is empty") nil)
        (T (dfs (list start) () goal moves))
    )
)


(defun star (open closed goal moves)


)

(defun a-star (start goal moves)
    (cond
        ((null start) (print "Start state is empty") nil)
        ((null goal) (print "Goal state is empty") nil)
        ((null moves) (print "Moves list is empty") nil)
        (T (star (list start) () goal moves))
    )
)
