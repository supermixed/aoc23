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

(defun all (p lst)
  (if lst
      (if (funcall p (car lst))
	  (all p (cdr lst))
	  nil)
      t))

(defun expand-vertical (lines)
  (when lines
    (let ((head (car lines)))
      (cons head
	    (if (all (lambda (c) (eql #\. c)) head)
		(cons head (expand-vertical (cdr lines)))
		(expand-vertical (cdr lines)))))))

(defun get-tile (lines x y)
  (nth x (nth y lines)))

(defun append-tile (line c)
  (if line
      (cons (car line) (append-tile (cdr line) c))
      (cons c nil)))

(defun append-all (lines c)
  (mapcar (lambda (line) (append-tile line c)) lines))

(defun zip (a b)
  (mapcar (lambda (ai bi) (cons ai bi)) a b))

(defun append-col (lines col)
  (mapcar #'append-tile lines col))

(defun get-col (lines x)
  (mapcar (lambda (line) (nth x line)) lines))

(defun nx (n x)
  (when (not (zerop n))
    (cons x (nx (1- n) x))))

(defun expand-horizontal (lines)
  (let ((newlines (nx (length lines) nil)))
    (dotimes (x (length (car lines)) newlines)
      (let ((col (get-col lines x)))
	(setf newlines (append-col newlines col))
	(when (all (lambda (c) (eql #\. c)) col)
	  (setf newlines (append-all newlines #\.)))))))

(defun line-to-string (line)
  (coerce
   (mapcar (lambda (c) (if (characterp c) c (code-char (+ 48 c)))) line)
   'string))

(defun print-map (map)
  (loop for ls in (mapcar (lambda (line) (line-to-string line)) map) do
    (format t "~A~%" ls)))

(defun get-galaxy-coords (map)
  (let ((coords))
    (do
     ((y 0 (1+ y)))
     ((= y (length map)) coords)
      (let ((line (nth y map)))
	(do
	 ((x 0 (1+ x)))
	 ((= x (length line)))
	  (when (eql #\# (nth x line))
	    (setf coords (cons (cons x y) coords))))))))

(defun get-coord-dist (a b)
  (+ (abs (- (car a) (car b)))
     (abs (- (cdr a) (cdr b)))))

(defun get-coord-dists (coord acc lst fn)
  (if lst
      (get-coord-dists coord (+ acc (funcall fn (car lst) coord)) (cdr lst) fn)
      acc))

(defun get-all-coord-dists (acc lst fn)
  (if lst
      (get-all-coord-dists
       (+ acc
	  (get-coord-dists (car lst) 0 (cdr lst) fn))
       (cdr lst)
       fn)
      acc))

(defun part1 (filename)
  (get-all-coord-dists 0 (get-galaxy-coords (expand-horizontal (expand-vertical (get-lines filename)))) #'get-coord-dist))

(defun dotp (c)
  (eql #\. c))

(defun get-empty-rows (map)
  (let ((empty))
    (dotimes (y (length map) empty)
      (when (all #'dotp (nth y map))
	(setf empty (cons y empty))))))

(defun get-empty-cols (map)
  (let ((empty))
    (dotimes (x (length (car map)) empty)
      (when (all #'dotp (get-col map x))
	(setf empty (cons x empty))))))

(defun filter (p lst)
  (let ((filtered))
    (dolist (i lst filtered)
      (when (funcall p i)
	(setf filtered (cons i filtered))))))

(defun in-range (lst a b)
  (let ((low (min a b))
	(high (max a b)))
    (filter (lambda (n) (and (< low n)
			     (< n high)))
	    lst)))

(defun dist-expanded (a b empty-rows empty-cols n)
  (+ (get-coord-dist a b)
     (* n
	(length (in-range empty-cols (car a) (car b))))
     (* n
	(length (in-range empty-rows (cdr a) (cdr b))))))

(defun part2 (filename n)
  (let* ((map (get-lines filename))
	 (empty-rows (get-empty-rows map))
	 (empty-cols (get-empty-cols map)))
    (get-all-coord-dists
     0
     (get-galaxy-coords map)
     (lambda (a b) (dist-expanded a b empty-rows empty-cols n)))))
