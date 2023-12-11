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

(defun apply-step (location direction map-ht)
  (let ((dests (gethash location map-ht)))
    (if dests
	(case direction
	  (#\L (car dests))
	  (#\R (cdr dests))))))

(setq test-map (make-hash-table :test #'equal))
(setf (gethash "AAA" test-map) (cons "BBB" "CCC"))

(defun parse-map-lines (lines)
  (let ((map-ht (make-hash-table :test #'equal)))
    (loop for line in lines do
      (let ((split (cl-ppcre:split " " line)))
	(setf (gethash (car split) map-ht)
	      (cons (subseq (nth 2 split) 1 4)
		    (subseq (nth 3 split) 0 3)))))
    map-ht))

(defun seq-to-list (seq)
  (let ((lst))
    (dotimes (i (length seq) (reverse lst))
      (setf lst (cons (elt seq i) lst)))))

(defun parse-input (lines)
  (cons (seq-to-list (car lines)) (parse-map-lines (cddr lines))))
    
(defun part1 (filename)
  (let ((location "AAA")
	(num-steps 0))
    (destructuring-bind (dirs . map-ht) (parse-input (get-lines filename))
      (loop
	(format t "~A~%" location)
	(if (or (not location) (equal "ZZZ" location))
	    (return num-steps)
	    (progn
	      (setf location (apply-step location (car dirs) map-ht))
	      (setf dirs (concatenate 'list (cdr dirs) (list (car dirs))))
	      (incf num-steps)))))))

(defun hash-table-keys (ht)
  (let ((keys))
    (maphash (lambda (k v) (setf keys (cons k keys))) ht)
    (reverse keys)))

(defun last-elem (seq)
  (elt seq (1- (length seq))))

(defun seq-endswith (seq c)
  (eql c (last-elem seq)))

(defun locations-ending-in-a (map-ht)
  (let ((locations))
    (dolist (loc (hash-table-keys map-ht) (reverse locations))
      (if (seq-endswith loc #\A)
	  (setf locations (cons loc locations))))))

(defun all-end-in-z (locations)
  (dolist (location locations t)
    (if (not (eql #\Z (last-elem location)))
	(return nil))))

(defun part2 (filename)
  (destructuring-bind (dirs . map-ht) (parse-input (get-lines filename))
    (let ((locations (locations-ending-in-a map-ht))
	  (num-steps 0))
      (loop
	; (format t "~A~%" locations)
	(if (all-end-in-z locations)
	    (return num-steps)
	    (progn
	      (setf locations (mapcar (lambda (location) (apply-step location (car dirs) map-ht)) locations))
	      (setf dirs (concatenate 'list (cdr dirs) (list (car dirs))))
	      (incf num-steps)))))))

(defun rotate-list (lst)
  (concatenate 'list (cdr lst) (list (car lst))))

(defun part2-part (dirs map-ht starting-location)
  (let ((location starting-location)
	(num-steps 0))
    (loop
	  (if (seq-endswith location #\Z)
	      (return num-steps)
	      (progn
		(setf location (apply-step location (car dirs) map-ht))
		(setf dirs (rotate-list dirs))
		(incf num-steps))))))

(defun part2-take2 (filename)
  (destructuring-bind (dirs . map-ht) (parse-input (get-lines filename))
    (apply #'lcm (mapcar (lambda (location) (part2-part dirs map-ht location)) (locations-ending-in-a map-ht)))))
