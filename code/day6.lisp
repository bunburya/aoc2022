; https://adventofcode.com/2022/day/6/input

(defun find-marker (input n)
  "Find the number of characters that need to processed before the last n
  processed characters are all unique."
  (let ((table (make-array 26 :initial-element 0))
        (codes (make-array (length input)))
        (in-char)
        (out-char))
    (loop for i from 0 to (- (length input) 1) do
          (setf (aref codes i) (- (char-code (aref input i)) 97)))
    (loop for i from (- n) to (- (length input) (+ n 1)) do
          (setf in-char (aref codes (+ i n)))
          (if (>= i 0) (setf out-char (aref codes i)))
          (if out-char (decf (aref table out-char) 1))
          (incf (aref table in-char) 1)
          (if
            (and
              (>= i 0)
              (not
                (loop for c across table when c do (if (> c 1) (return t)))))
            (return (+ i n 1))))))

(with-open-file (stream "input/day6.txt" :direction :input)
  (let ((input (read-line stream nil)))
    (format t "~d~%~d~%" 
            ; Part 1
            (find-marker input 4)
            ; Part 2
            (find-marker input 14))))
