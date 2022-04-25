(defun russian-upper-case-p (char)
  (position char "ÀÁÂÃÄÅ¨ÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖ×ØÙÚÛÜİŞß"))

(defun russian-char-downcase (char)
  (let ((i (russian-upper-case-p char)))
    (if i 
        (char "àáâãäå¸æçèéêëìíîïğñòóôõö÷øùúûüışÿ" i)
        (char-downcase char))))   

(defun russian-string-downcase (string)
  (map 'string #'russian-char-downcase string))

(defun russian-char-equal (char1 char2)
  (char-equal (russian-char-downcase char1)
              (russian-char-downcase char2)))

(defun russian-string-equal (str1 str2)
  (and (= (length str1) (length str2))
       (loop for char1 across str1
             and char2 across str2
             always (russian-char-equal char1 char2))))

(defun whitespace-char-pp (char)
  (member char '(#\Space #\Tab #\Newline)))

(defun word-list (string)
  (loop with len = (length string)
        for left = 0 then (1+ right)
        for right = (or (position-if #'whitespace-char-p string
                                     :start left)
                        len)
        unless (= right left)
          collect (subseq string left right)
        while (< right len)))

(defun find-word (word source)
  (let ((f -1)
        (s -1))
    (dolist (sentence source)
      (dolist (w (word-list sentence))
        (let ((wword (string-right-trim ",.;:?!" (russian-string-downcase word))))
          (setq w (string-right-trim ",.;:?!" (russian-string-downcase w)))
          (if (and (russian-string-equal w wword) (= f -1)) 
              (let () (setq f (search (russian-string-downcase word) (russian-string-downcase sentence))) 
                      (setq s (position sentence source)))))))
   (if (= f -1) nil (values f s))))
