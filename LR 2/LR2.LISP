(defun myremove (item lst)
  (cond ((atom lst)
         lst)
        ((equal item (first lst))
         (myremove item (rest lst)))
        (t
         (cons (myremove item (first lst))
               (myremove item (rest lst))))))

(defun индивидуумы (lst)
  (if (null lst) nil
      (if (member (first lst)(rest lst))
          (индивидуумы (myremove (first lst) (rest lst)))
          (cons (first lst)(индивидуумы (rest lst))))))