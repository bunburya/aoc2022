(require 'uiop)

(defun parse-tokens (tokens sizes path)
  (if (string= (first tokens) "$")
    ; command
    (if (string= (second tokens) "cd")
      (if (string= (third tokens) "..")
	; cd to parent directory
	(vector-pop path)
	; cd to child directory
	(let ((dirname (third tokens)))
	  (vector-push-extend dirname path)
	  (setf (gethash dirname sizes) 0))))
    ; output
    (if (not (string= (first tokens) "dir"))
      ; fize size and name
      (let ((size (parse-integer (first tokens))))
	(loop for d across path do
	      (incf (gethash d sizes) size))))))

(defun table-values (table)
  (loop for key being the hash-keys of table collect (gethash key table)))

(with-open-file (stream "input/day7.txt" :direction :input)
  (let ((sizes (make-hash-table))
	(path (make-array 10 :fill-pointer 0 :adjustable t)))
    (loop for line = (read-line stream nil) while line do
	  (parse-tokens (uiop:split-string line) sizes path))
    (let ((sizelist (table-values sizes)))
      ; Part 1
      (format t "~d~%" (reduce #'+ (remove-if (lambda (x) (> x 100000)) sizelist)))
      ; Part 2
      ; using (first sizelist) is a bit hacky and depends on "/" being the first
      ; key in the table, but (gethash "/" sizes) seems to always return nil and I
      ; don't know why
      (let ((space-needed (- 30000000 (- 70000000 (first sizelist)))))
	(format t "~d~%" (apply #'min
				(remove-if (lambda (x) (< x space-needed)) sizelist)))))))
