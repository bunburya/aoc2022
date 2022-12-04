; https://adventofcode.com/2022/day/2

(defun move-score (move)
  "Calculate the score attributable to a player's move."
  (cond
    ((string= move "X") 1)     ; rock
    ((string= move "Y") 2)     ; paper
    ((string= move "Z") 3)))   ; scissors

(defun outcome-score (my-move their-move)
  "Calculate the score attributable to the outcome of a game."
  (cond
    ((string= my-move "X") 
     (cond ((string= their-move "A") 3)     ; rock
           ((string= their-move "B") 0)     ; paper
           ((string= their-move "C") 6)))   ; scissors
    ((string= my-move "Y")
     (cond ((string= their-move "A") 6)
           ((string= their-move "B") 3)
           ((string= their-move "C") 0)))
    ((string= my-move "Z")
     (cond ((string= their-move "A") 0)
           ((string= their-move "B") 6)
           ((string= their-move "C") 3)))))

(defun score (my-move their-move)
  "Calculate the score for a single match."
  (+ (move-score my-move) (outcome-score my-move their-move)))

(defun get-move (their-move result)
  "Calculate the required move based on the other player's move and the desired result."
  (cond
    ((string= their-move "A")
     (cond
       ((string= result "X") "Z")       ; lose
       ((string= result "Y") "X")       ; draw
       ((string= result "Z") "Y")))     ; win
    ((string= their-move "B")
     (cond ((string= result "X") "X")
           ((string= result "Y") "Y")
           ((string= result "Z") "Z")))
    ((string= their-move "C")
     (cond ((string= result "X") "Y")
           ((string= result "Y") "Z")
           ((string= result "Z") "X")))))

(let ((total-1 0) (total-2 0))
  (with-open-file (stream "input/day2.txt" :direction :input)
    (loop for line = (read-line stream nil) while line do
          (let ((col-1 (subseq line 0 1)) (col-2 (subseq line 2 3)))
            (incf total-1 (score col-2 col-1))
            (incf total-2 (score (get-move col-1 col-2) col-1)))))
  (format t "~d~%~d~%" total-1 total-2))

