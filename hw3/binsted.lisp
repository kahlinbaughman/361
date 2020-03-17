(defun depth-first-search (start goal moves)
  (dfs (list (list start nil)) nil goal moves))

(defun dfs (open closed goal moves)
  (format t "Open list: ~A.~%" open)
  (cond
   ((null open) (format t "Open list is empty.~%") nil)
   ((equal (first (first open)) goal) (format t "Found goal: ~A.~% Solution path: ~A.~% Length of open list: ~A.~% Length of closed list: ~A.~%" goal (reverse (cons goal (nth 1 (first open)))) (length open) (length closed)) t)
   ((mymember (first (first open)) closed) (dfs (rest open) closed goal moves))
   (t (dfs (append (generate-children (first open) moves) (rest open)) (cons (first (first open)) closed) goal moves))))

(defun breadth-first-search (start goal moves)
 (bfs (list (list start nil)) nil goal moves))

(defun bfs (open closed goal moves)
 (format t "Open list: ~A.~%" open)
 (cond
  ((null open) (format t "Open list is empty.~%") nil)
  ((equal (first (first open)) goal) (format t "Found goal: ~A.~% Solution path: ~A.~% Length of open list: ~A.~% Length of closed list: ~A.~%" goal (reverse (cons goal (nth 1 (first open)))) (length open) (length closed)) t)
  ((mymember (first (first open)) closed) (bfs (rest open) closed goal moves))
  (t (bfs (append (rest open) (generate-children (first open) moves)) (cons (first (first open)) closed) goal moves))))

(defun a-star (start goal moves heuristic)
    (star (list (list start nil)) nil goal moves heuristic)
)

(defun star (open closed goal moves heuristic)
    (format t "Open list: ~A.~%" open)
    (cond
        ((null open) (format t "Open list is empty.~%") nil)
        ((equal (first (first open)) goal) (format t "Found goal: ~A.~% Solution path: ~A.~% Length of open list: ~A.~% Length of closed list: ~A.~%" goal (reverse (cons goal (nth 1 (first open)))) (length open) (length closed)) t)
        ((mymember (first (first open)) closed) (star (rest open) closed goal moves heuristic))
        (t (star (insert-by-weight (generate-children (first open) moves) (rest open) heuristic) (cons (first (first open)) closed) goal moves heuristic))))


(defun insert-by-weight (children sorted-list heuristic)
  (cond ((null children) sorted-list)
        (t (insert (car children)
           (insert-by-weight (cdr children) sorted-list heuristic) heuristic))))

(defun insert (item sorted-list heuristic)
  (cond ((null sorted-list) (list item))
        ((< (funcall heuristic item) (funcall heuristic (car sorted-list)))
         (cons item sorted-list))
        (t (cons (car sorted-list) (insert item (cdr sorted-list) heuristic)))))


(defun mymember (item list)
  (cond
   ((null list) nil)
   ((equal item (first list)) t)
   (t (mymember item (rest list)))))

(defun generate-children (node moves)
  (cond
   ((null moves) nil)
   ((null (funcall (first moves) (first node))) (generate-children node (rest moves)))
   (t (cons
       (list (first (funcall (first moves) (first node))) (cons (first node) (nth 1 node)))
       (generate-children node (rest moves))))))
