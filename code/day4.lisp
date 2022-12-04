; https://adventofcode.com/2022/day/4

(defun parse-range (range)
  "Parse string representing range of numbers and return start and end of range."
  (let ((dash (position #\- range)))
    (list
      (parse-integer (subseq range 0 dash))
      (parse-integer (subseq range (+ dash 1) (length range))))))

(defun parse-line (line)
  "Parse a line and return the two ranges (as lists of form (START END))."
  (let ((comma (position #\, line)))
    (list
      (parse-range (subseq line 0 comma))
      (parse-range (subseq line (+ comma 1) (length line))))))

(defun total-overlap (range-1 range-2)
  "Check whether one range completely contains the other."
  (or (and (<= (first range-1) (first range-2))
	   (>= (second range-1) (second range-2)))
      (and (>= (first range-1) (first range-2))
	   (<= (second range-1) (second range-2)))))

(defun any-overlap (range-1 range-2)
  "Check whether there is any overlap between the given ranges."
  (or (and (<= (first range-1) (second range-2))
           (>= (second range-1) (first range-2)))
      (and (>= (first range-2) (second range-1))
           (<= (second range-2) (first range-1)))))

(let ((total-1 0) (total-2 0))
  (with-open-file (stream "input/day4.txt" :direction :input)
    (loop for line = (read-line stream nil) while line do
          (let ((ranges (parse-line line)))
            (let ((range-1 (first ranges)) (range-2 (second ranges)))
              (if (total-overlap range-1 range-2)
                (incf total-1 1))
	      (if (any-overlap range-1 range-2)
                (incf total-2 1))))))
  (format t "~d~%~d~%" total-1 total-2))

