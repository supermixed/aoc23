(defun get-line (filename)
  (with-open-file (stream filename)
		  (read-line stream nil)))

(defun position-all (seq c)
  (let ((positions))
    (dotimes (i (length seq) (reverse positions))
      (when (eq c (elt seq i))
	(setf positions (cons i positions))))))

(defun split-string (s c)
  (let ((positions (concatenate 'list (list -1) (position-all s c) (list (length s))))
	(split))
    (dotimes (i (1- (length positions)) (reverse split))
      (setf split (cons (subseq s (1+ (nth i positions)) (nth (1+ i) positions)) split)))))

(defun hash (s)
  (let ((result 0))
    (dotimes (i (length s) result)
      (setf result (mod (* 17 (+ result (char-code (elt s i)))) 256)))))

(defun part1 (filename)
  (apply #'+ (mapcar #'hash (split-string (get-line filename) #\,))))

(defun remove-step (boxes instruction)
  (let* ((label (subseq instruction 0 (1- (length instruction))))
	 (boxn (hash label)))
    (setf (nth boxn boxes) (remove label (nth boxn boxes) :test #'equal :key #'car))))

(defun add-lens (boxes label strength)
  (let* ((boxn (hash label)))
    (if (assoc label (nth boxn boxes) :test #'equal)
	(setf (cdr (assoc label (nth boxn boxes) :test #'equal)) strength)
	(setf (nth boxn boxes) (append (nth boxn boxes) (list (cons label strength)))))))

(defun add-step (boxes instruction)
  (destructuring-bind (label strength-str) (split-string instruction #\=)
    (let ((strength (parse-integer strength-str)))
      (add-lens boxes label strength))))

(defun do-step (boxes instruction)
  (if (find #\- instruction)
      (remove-step boxes instruction)
      (add-step boxes instruction))
  boxes)

(defun iota (n)
  (let ((lst))
    (dotimes (i n (reverse lst))
      (setf lst (cons i lst)))))

(defun lens-power (boxes)
  (apply #'+
	 (mapcar
	  (lambda (box boxi)
	    (apply #'+
		   (mapcar (lambda (lens lensi)
			     (* (cdr lens) (1+ lensi) (1+ boxi)))
			   box
			   (iota (length box)))))
	  boxes
	  (iota (length boxes)))))

(defun part2 (filename)
  (let ((boxes (make-list 256)))
    (dolist (instruction (split-string (get-line filename) #\,) (lens-power boxes))
      (do-step boxes instruction))))
