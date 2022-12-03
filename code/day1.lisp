; https://adventofcode.com/2022/day/1

(defun append-in-place (*list* *item*)
  "Append an item to a list, in-place."
  (setf (cdr (last *list*)) (cons *item* nil)))

(defun incr-last-in-place (*list* n)
  "Increment the last (numeric) item in the given list by n."
  (setf (car (last *list*)) (+ (car (last *list*)) n)))

(let ((cals (list 0)))
  (with-open-file (stream "input/day1.txt" :direction :input)
    (loop for line = (read-line stream nil) while line do 
          (if (= (length line) 0)
            (append-in-place cals 0)
            (incr-last-in-place cals (parse-integer line)))))
  (let ((sorted (sort cals #'>)))
    ; Problem 1: Find the highest number of calories carried by a single elf
    (format t "~d~%" (first sorted))
    ; Problem 2: Find the highest number of calories carried by any three elves
    (format t "~d~%" (+ (first sorted) (second sorted) (third sorted)))))
