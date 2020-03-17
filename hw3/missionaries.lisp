(defconstant *startm* '(3 3 W))
(defconstant *goalm* '(0 0 E))
(defconstant *movesm* '(1C-CROSSES 2C-CROSS 1C1M-CROSS 1M-CROSSES 2M-CROSS))
(defvar *heuristicm* 'weightm)

(defun 1c-crosses (state)
  (cond
   ((equal (nth 2 state) 'w) (mc-state-test (list (- (nth 0 state) 1) (nth 1 state) 'e)))
   (t (mc-state-test (list (+ (nth 0 state) 1) (nth 1 state) 'w)))))

(defun 2c-cross (state)
  (cond
   ((equal (nth 2 state) 'w) (mc-state-test (list (- (nth 0 state) 2) (nth 1 state) 'e)))
   (t (mc-state-test (list (+ (nth 0 state) 2) (nth 1 state) 'w)))))

(defun 1c1m-cross (state)
  (cond
   ((equal (nth 2 state) 'w) (mc-state-test (list (- (nth 0 state) 1) (- (nth 1 state) 1) 'e)))
   (t (mc-state-test (list (+ (nth 0 state) 1) (+ (nth 1 state) 1) 'w)))))

(defun 1m-crosses (state)
  (cond
   ((equal (nth 2 state) 'w) (mc-state-test (list (nth 0 state) (- (nth 1 state) 1) 'e)))
   (t (mc-state-test (list (nth 0 state) (+ (nth 1 state) 1) 'w)))))

(defun 2m-cross (state)
  (cond
   ((equal (nth 2 state) 'w) (mc-state-test (list (nth 0 state) (- (nth 1 state) 2) 'e)))
   (t (mc-state-test (list (nth 0 state) (+ (nth 1 state) 2) 'w)))))

(defun mc-state-test (state)
  (cond
   ((> (nth 0 state) 3) nil)
   ((< (nth 0 state) 0) nil)
   ((> (nth 1 state) 3) nil)
   ((< (nth 1 state) 0) nil)
   ((and (> (nth 0 state) (nth 1 state)) (> (nth 1 state) 0)) nil)
   ((and (> (- 3 (nth 0 state)) (- 3 (nth 1 state))) (> 3 (nth 1 state))) nil)
   (t (list state))))

(defun weightm (state)
   (+ (caar state) (caaadr state))
)
