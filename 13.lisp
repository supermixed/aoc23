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

(defun split (lst delim &optional acc res)
  (if lst
      (let ((head (car lst))
	    (rest (cdr lst)))
	(if (eq head delim)
	    (split rest delim nil (cons (reverse acc) res))
	    (split rest delim (cons head acc) res)))
      (reverse (cons (reverse acc) res))))

(defun flip-2d (lines)
  (let ((res))
    (dotimes (y (length (car lines)) (reverse res))
      (let ((line))
	(dotimes (x (length lines))
	  (setf line (cons (nth y (nth x lines)) line)))
	(setf res (cons (reverse line) res))))))

(defun find-repeated-lines (lines)
  (let ((res))
    (dotimes (y (1- (length lines)) res)
      (when (equal (nth y lines) (nth (1+ y) lines))
	(setf res (cons y res))))))

(defun get-sides (lines y)
  (let* ((nbefore (1+ y))
	 (nafter (- (length lines) nbefore)))
    (if (< nbefore nafter)
	(cons (subseq lines 0 (1+ y)) (subseq lines (1+ y) (+ 1 y nbefore)))
	(cons (subseq lines (1+ (- y nafter)) (1+ y)) (subseq lines (1+ y) (length lines))))))
    
(defun validate-mirror (lines y)
  (let ((sides (get-sides lines y)))
    (equal (car sides) (reverse (cdr sides)))))

(defun get-mirror (lines)
  (dolist (y (find-repeated-lines lines))
    (when (validate-mirror lines y)
      (return y))))

(defun score (lines get-line)
  (let ((hmirror (funcall get-line lines)))
    (if hmirror
	(* 100 (1+ hmirror))
	(1+ (funcall get-line (flip-2d lines))))))

(defun part1 (filename &optional (get-line #'get-mirror))
  (apply #'+ (mapcar (lambda (problem) (score problem get-line)) (split (get-lines filename) nil))))

(defun only-one-diff (s1 s2 &optional (ndiffs 0))
  (if s1
      (let ((h1 (car s1))
	    (h2 (car s2))
	    (r1 (cdr s1))
	    (r2 (cdr s2)))
	(if (eq h1 h2)
	    (only-one-diff r1 r2 ndiffs)
	    (when (zerop ndiffs)
	      (only-one-diff r1 r2 1))))
      (= 1 ndiffs)))

(defun flat-lists (lst)
  (apply #'concatenate 'list lst))

(defun get-smudge (lines)
  (dotimes (y (1- (length lines)))
    (let* ((sides (get-sides lines y))
	   (before (flat-lists (car sides)))
	   (after (flat-lists (reverse (cdr sides)))))
      (when (only-one-diff before after)
	(return y)))))

(defun part2 (filename)
  (part1 filename #'get-smudge))
