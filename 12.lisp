(ql:quickload :cl-ppcre)

(defun get-lines (filename)
  (let ((lines nil))
    (with-open-file (stream filename)
      (loop
	(let ((line (read-line stream nil)))
	  (if line
	      (setf lines (cons line lines))
	      (return)))))
    (reverse lines)))

(defun parse-line (line)
  (destructuring-bind (springs rec) (cl-ppcre:split " " line)
    (list (seq-to-list springs) (rec-to-spec rec))))

(defun append-if-positive (lst n)
  (if (> n 0)
      (cons n lst)
      lst))

(defun rle (s &optional (cnt 0) acc)
    (if (not s)
	(reverse (append-if-positive acc cnt))
	(let ((rst (cdr s)))
	  (case (car s)
	    (#\# (rle rst (1+ cnt) acc))
	    (#\. (rle rst 0 (append-if-positive acc cnt)))))))

(defun rec-to-spec (s)
  (mapcar #'parse-integer (cl-ppcre:split "," s)))

(defun seq-to-list (seq &optional acc)
  (if (zerop (length seq))
      (reverse acc)
      (seq-to-list (subseq seq 1) (cons (elt seq 0) acc))))

(defun validate (springs spec)
  (equal spec (rle springs)))

(defun apply-no (s n &optional acc)
  (if (not s)
      (reverse acc)
      (let ((head (car s))
	    (rst (cdr s)))
	(case head
	  (#\? (apply-no rst (ash n -1) (cons (if (zerop (logand 1 n))
						  #\.
						  #\#)
					      acc)))
	  (t   (apply-no rst n (cons head acc)))))))

(defun permutations (s)
  (let ((acc))
    (dotimes (n (expt 2 (count #\? s)) acc)
      (setf acc (cons (apply-no s n) acc)))))

(defun filter (p lst &optional acc)
  (if lst
      (let ((head (car lst))
	    (rst (cdr lst)))
	(if (funcall p head)
	    (filter p rst (cons head acc))
	    (filter p rst acc)))
      acc))

(defun valid-permutations (s spec)
  (length (filter (lambda (ex) (validate ex spec)) (permutations s))))

(defun part1 (filename)
  (apply #'+
	 (mapcar (lambda (problem)
		   (apply #'valid-permutations problem))
		 (mapcar #'parse-line (get-lines filename)))))
