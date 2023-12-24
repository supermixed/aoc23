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

(defun get-tile (lines x y &optional (default #\O))
  (if (and (<= 0 y)
	     (< y (length lines)))
    (let ((line (nth y lines)))
      (if (and (<= 0 x)
		 (< x (length line)))
	  (nth x line)
	  default))
    default))

(defun find-coords (lines c)
  (let ((res))
    (dotimes (y (length lines) (reverse res))
      (let ((line (nth y lines)))
	(dotimes (x (length line))
	  (if (eq c (nth x line))
	      (setf res (cons (cons x y) res))))))))

(defun find-roll-stop (lines x y dx dy)
  (do ((ny (+ y dy) (+ ny dy))
       (nx (+ x dx) (+ nx dx)))
      ((not (eq (get-tile lines nx ny) #\.))
       (cons (- nx dx) (- ny dy)))))

(defparameter *dirs* '((0 . -1)
		       (-1 . 0)
		       (0 . 1)
		       (1 . 0)))

(defun dot-product (a b)
  (+ (* (car a) (car b))
     (* (cdr a) (cdr b))))

(defun sort-coords (coords dir)
  (sort coords (lambda (a b)
		 (> (dot-product a dir)
		    (dot-product b dir)))))

(defun roll-rock (lines x y dir)
  (setf (nth x (nth y lines)) #\.)
  (let ((stop-coord (find-roll-stop lines x y (car dir) (cdr dir))))
    (setf (nth (car stop-coord) (nth (cdr stop-coord) lines)) #\O))
  lines)

(defun roll-all-rocks (lines dir)
  (dolist (coord (sort-coords (find-coords lines #\O) dir) lines)
    (roll-rock lines (car coord) (cdr coord) dir)))

(defun rock-load (lines)
  (let ((row-count (length lines)))
    (apply #'+
	   (mapcar (lambda (coord)
		     (- row-count (cdr coord)))
		   (find-coords lines #\O)))))

(defun part1 (filename)
  (rock-load (roll-all-rocks (get-lines filename) (car *dirs*))))

(defun cycle (lines)
  (dotimes (i (length *dirs*) lines)
    (roll-all-rocks lines (nth i *dirs*))))

(defun part2 (filename &optional (n 1000000000))
  (let ((lines (get-lines filename))
	(seen))
    (dotimes (i n)
      (let ((samei (position lines seen :test #'equal)))
	(if samei
	    (return (rock-load (nth (+ samei (mod (- n samei) (- i samei))) seen)))
	    (progn
	      (setf seen (concatenate 'list seen (list (copy-tree lines))))
	      (cycle lines)))))))
