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

(defun parse-input (lines)
  (mapcar (lambda (line) (mapcar #'parse-integer (cl-ppcre:split " " line))) lines))

(defun diff-list (lst)
  (let ((diffs))
    (do
     ((i 0 (1+ i)))
     ((= i (1- (length lst))) (reverse diffs))
      (setf diffs (cons (- (nth (1+ i) lst) (nth i lst)) diffs)))))

(defun all-seq (seq pred)
  (dolist (item seq t)
    (unless (funcall pred item)
      (return nil))))

(defun gen-diffs (lst)
  (do
   ((lsts (list lst) (cons (diff-list (car lsts)) lsts)))
   ((all-seq (car lsts) #'zerop) lsts)))

(defun extrapolate (lst diff)
  (+ (car (last lst)) diff))

(defun extrapolate-back (lst diff)
  (- (car lst) diff))

(defun extrapolate-all (lsts diff)
  (if lsts
      (extrapolate-all (cdr lsts) (extrapolate (car lsts) diff))
      diff))

(defun extrapolate-all-back (lsts diff)
  (if lsts
      (extrapolate-all-back (cdr lsts) (extrapolate-back (car lsts) diff))
      diff))

(defun part1 (filename)
  (apply #'+ (mapcar (lambda (lst) (extrapolate-all (cdr (gen-diffs lst)) 0)) (parse-input (get-lines filename)))))

(defun part2 (filename)
  (apply #'+ (mapcar (lambda (lst) (extrapolate-all-back (cdr (gen-diffs lst)) 0)) (parse-input (get-lines filename)))))
