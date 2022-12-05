; https://adventofcode.com/2022/day/5

(require 'uiop)

(defun init-stacks (lines)
  "Initialise a sequence of stacks based on the first few lines of input."
  (let ((stacks (make-array 9 :fill-pointer 0)))
    (dotimes (_ 9)
      (vector-push-extend (make-array 10 :fill-pointer 0 :adjustable t) stacks))
    (loop for line in (rest lines) do
          (let ((loc nil))
            (dotimes (i 9)
              (setf loc (+ (* i 4) 1)) ; Location of letter representing crate
              (let ((c (char line loc)))
                (if (not (eq c #\Space))
                  (vector-push-extend (char line loc) (aref stacks i))))))
          finally (return stacks))))

(defun move-crates (line stacks multi)
  (let ((tokens (uiop:split-string line)))
    (let ((how-many (parse-integer (nth 1 tokens)))
          (from-stack (aref stacks (- (parse-integer (nth 3 tokens)) 1)))
          (to-stack (aref stacks (- (parse-integer (nth 5 tokens)) 1))))
      (if multi
        (let ((temp-stack (make-array how-many :fill-pointer 0)))
          (dotimes (_ how-many)
            (vector-push-extend (vector-pop from-stack) temp-stack))
          (dotimes (_ how-many)
            (vector-push-extend (vector-pop temp-stack) to-stack)))
        (dotimes (_ how-many)
          (vector-push-extend (vector-pop from-stack) to-stack))))))

(with-open-file (stream "input/day5.txt" :direction :input)
  (let ((initial-crates
          (reverse 
            (loop
              for line = (read-line stream nil)
              while (> (length line) 0)
              collect line)))
        (instructions
          (loop for line = (read-line stream nil)
                while line
                collect line)))
    
    (dotimes (i 2)
      (let ((stacks (init-stacks initial-crates)))
        ; Equality test here means that we pass multi=T on the first run and
        ; multi=nil on the second run.
        (loop for line in instructions do (move-crates line stacks (eq i 1)))
        (write-line
          (coerce
            (loop for s across stacks collect (aref s (- (length s) 1)))
            'string))))))

