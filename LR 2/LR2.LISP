(defun myremove (item lst)
  (cond ((atom lst)
         lst)
        ((equal item (first lst))
         (myremove item (rest lst)))
        (t
         (cons (myremove item (first lst))
               (myremove item (rest lst))))))

(defun ����������� (lst)
  (if (null lst) nil
      (if (member (first lst)(rest lst))
          (����������� (myremove (first lst) (rest lst)))
          (cons (first lst)(����������� (rest lst))))))