; https://adventofcode.com/2022/day/3

(defun char-priority (c)
  "Return the priority of a given alphabetical character.
  Assumes char is in [A-Za-z]."
  (- (char-code c) (if (char< c #\a) 38 96)))

(defun common-char (string-list)
  "Return the character that is common to all strings in the given list.
  Assumes only one such character exists."
  (loop named outer for c across (first string-list) do
        (loop for s in (rest string-list) do
              (if (not (find c s)) (return))
              finally (return-from outer c))))

(defun split-string (str)
  "Split a string into two halves. Assumes the string is of even length."
  (let ((len (length str)))
    (let ((mid (/ len 2)))
      (list (subseq str 0 mid) (subseq str mid len)))))

(defun get-n-lines (stream n)
  "Return a list of n lines from stream."
  (loop for _ from 1 to n collect (read-line stream nil)))

(let ((total-1 0) (total-2 0))
  (with-open-file (stream "input/day3.txt" :direction :input)
    (loop for lines = (get-n-lines stream 3) while (not (position nil lines)) do
          ; Part 1
          (loop for line in lines do
                (incf total-1 (char-priority (common-char (split-string line)))))
          ; Part 2
          (incf total-2 (char-priority (common-char lines)))))
          
  (format t "~d~%~d~%" total-1 total-2))
