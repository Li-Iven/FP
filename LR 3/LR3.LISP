(defun spiral-matrix(n)
    (let ((matrix (make-array (list n n)))
          (lvl (+ (floor n 2) (mod n 2)))
          (curr 1)
          (border))
          (loop for i upfrom 0 to (- lvl 1)
              do
              (setq border (- n (+ i 1)))
              (loop for j upfrom i to border
                  do
                  (setf (aref matrix i j) curr)
                  (setq curr (+ curr 1)))
              (loop for j upfrom (+ i 1) to border
                  do
                  (setf (aref matrix j border) curr)
                  (setq curr (+ curr 1)))
              (loop for j downfrom (- border 1) to i
                  do
                  (setf (aref matrix border j) curr)
                  (setq curr (+ curr 1)))
              (loop for j downfrom (- border 1) to (+ i 1)
                  do
                  (setf (aref matrix j i) curr)
                  (setq curr (+ curr 1))))
          matrix))

(defun print-matrix (matrix &optional (chars 3) stream)
  (let ((*print-right-margin* (+ 6 (* (1+ chars)
                                      (array-dimension matrix 1)))))
    (pprint matrix stream)
    (values)))