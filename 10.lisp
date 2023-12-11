(ql:quickload :cl-ppcre)

(defun seq-to-list (seq)
  (let ((lst))
    (dotimes (i (length seq) (reverse lst))
      (setf lst (cons (elt seq i) lst)))))

(defun get-lines (filename)
  (let ((lines nil))
    (with-open-file (stream filename)
      (loop
	(let ((line (read-line stream nil)))
	  (if line
	      (setf lines (cons (seq-to-list line) lines))
	      (return)))))
    (reverse lines)))

(defun get-tile (map x y)
  (if (and
       (<= 0 y)
       (<  y (length map)))
      (let ((line (nth y map)))
	(if (and
	     (<= 0 x)
	     (<  x (length line)))
	    (elt line x)))))

(defun set-tile (map x y c)
  (if (and
       (<= 0 y)
       (<  y (length map)))
      (let ((line (nth y map)))
	(if (and
	     (<= 0 x)
	     (<  x (length line)))
	    (setf (elt line x) c)))))
  
(defun find-tile (map c)
  (block outer
    (do
     ((y 0 (1+ y)))
     ((= y (length map)))
      (do
       ((line (nth y map))
	(x 0 (1+ x)))
       ((= x (length line)))
	(if (eql c (elt line x))
	    (return-from outer (cons x y)))))))

(defun char-lookup (c)
  "(up right down left)"
  (case c
    (#\| '(t nil t nil))
    (#\- '(nil t nil t))
    (#\L '(t t nil nil))
    (#\J '(t nil nil t))
    (#\7 '(nil nil t t))
    (#\F '(nil t t nil))
    (#\S '(t t t t))
    (t '(nil nil nil nil))))

(defun connectedp (from to dir)
  (let ((from-lookup (char-lookup from))
	(to-lookup (char-lookup to)))
    (case dir
      (:up (and (nth 0 from-lookup) (nth 2 to-lookup)))
      (:right (and (nth 1 from-lookup) (nth 3 to-lookup)))
      (:down (and (nth 2 from-lookup) (nth 0 to-lookup)))
      (:left (and (nth 3 from-lookup) (nth 1 to-lookup))))))

(defun neighbour-coords (x y)
  (list
   (cons x (1- y))
   (cons (1+ x) y)
   (cons x (1+ y))
   (cons (1- x) y)))

(defun zip (a b)
  (mapcar (lambda (ai bi) (cons ai bi)) a b))

(defun get-connected-coords (map coord)
  (let* ((x (car coord))
	(y (cdr coord))
	(current-tile (get-tile map x y))
	(new-coords))
    (loop for dir in (zip (list :up :right :down :left) (neighbour-coords x y))
	  do
	     (destructuring-bind (dir . (newx . newy)) dir
	       (if (connectedp current-tile (get-tile map newx newy) dir)
		   (setf new-coords (cons (cons newx newy) new-coords)))))
    new-coords))

(defun apply-step (map coords n)
  (let ((next-coords))
    (loop for coord in coords do
      (setf next-coords (concatenate 'list next-coords (get-connected-coords map coord)))
      (set-tile map (car coord) (cdr coord) n))
    next-coords))

(defun line-to-string (line)
  (coerce
   (mapcar (lambda (c) (if (characterp c) c (code-char (+ 48 c)))) line)
   'string))

(defun print-map (map)
  (loop for ls in (mapcar (lambda (line) (line-to-string line)) map) do
    (format t "~A~%" ls)))

(defun part1 (filename &optional do-print-map)
  (let ((map (get-lines filename)))
    (do
     ((n 0 (1+ n))
      (coords (list (find-tile map #\S)) (apply-step map coords n)))
     ((not coords) (progn
		     (when do-print-map (print-map map))
		     (cons (1- n) map))))))

(defun pipe-to-expanded (c)
  (let ((lookup (char-lookup c)))
    (list (list #\. (if (car lookup) #\| #\.) #\.)
	  (list (if (cadddr lookup) #\- #\.) #\+ (if (cadr lookup) #\- #\.))
	  (list #\. (if (caddr lookup) #\| #\.) #\.))))

(defun char-to-expanded (walledc originalc)
  (if (numberp walledc)
      (pipe-to-expanded originalc)
      '((#\. #\. #\.)
	(#\. #\. #\.)
	(#\. #\. #\.))))

(defun add-3list (lsts-a lsts-b)
  (list
   (concatenate 'list (car lsts-a) (car lsts-b))
   (concatenate 'list (cadr lsts-a) (cadr lsts-b))
   (concatenate 'list (caddr lsts-a) (caddr lsts-b))))

(defun expand-map (original walled)
  (let ((expanded))
    (do
     ((y 0 (1+ y)))
     ((= y (length walled)) expanded)
      (let ((line (nth y walled))
	    (ex-lines (list nil nil nil)))
	(do
	 ((x 0 (1+ x)))
	 ((= x (length line)))
	  (setf ex-lines (add-3list ex-lines (char-to-expanded (nth x line) (get-tile original x y)))))
	(setf expanded (concatenate 'list expanded ex-lines))))))

(defun filter (p lst)
  (let ((filtered))
    (dolist (i lst filtered)
      (when (funcall p i)
	(setf filtered (cons i filtered))))))

(defun tile-empty (map x y)
  (eql #\. (get-tile map x y)))

(defun apply-fill (expanded pending)
  (let ((next-pending))
    (dolist (coord pending next-pending)
      (let ((x (car coord))
	    (y (cdr coord)))
	(set-tile expanded x y #\O)
	(dolist
	    (new-coord (filter
			(lambda (coord)
			  (tile-empty expanded (car coord) (cdr coord)))
			(neighbour-coords x y)))
	  (when (not (find new-coord next-pending :test #'equal))
	    (setf next-pending (cons new-coord next-pending))))))))

(defun fill-map (expanded)
  (do ((pending (list (cons 0 0)) (apply-fill expanded pending)))
      ((not pending) expanded)))

(defun shrink-map (expanded)
  (do ((y 1 (+ 3 y))
       (shrunk nil (cons (do* ((line (nth y expanded))
	    (x 1 (+ 3 x))
	    (shrunk-line))
	   ((>= x (length line)) (reverse shrunk-line))
			   (setf shrunk-line (cons (nth x line) shrunk-line)))
			 shrunk)))
      ((>= y (length expanded)) shrunk)))

(defun count-2d (c map)
  (do ((y 0 (1+ y))
       (count 0))
      ((= y (length map)) count)
    (do* ((line (nth y map))
	  (x 0 (1+ x)))
	 ((= x (length line)))
      (when (eql c (nth x line))
	(incf count)))))

(defun part2 (filename)
  (let ((original (get-lines filename)))
    (destructuring-bind (steps . walled) (part1 filename)
      (count-2d #\. (shrink-map (fill-map (expand-map original walled)))))))
