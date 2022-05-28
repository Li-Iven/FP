(defclass cart ()        
 ((x :initarg :x :reader cart-x)  
  (y :initarg :y :reader cart-y)))

(defmethod print-object ((c cart) stream)
  (format stream "[CART x ~d y ~d]"
          (cart-x c) (cart-y c)))

(defclass polar ()
 ((radius :initarg :radius :accessor radius) 
  (angle  :initarg :angle  :accessor angle)))

(defmethod print-object ((p polar) stream)
  (format stream "[POLAR radius ~d angle ~d]"
          (radius p) (angle p)))

(defmethod cart-x ((p polar))
  (* (radius p) (cos (angle p))))

(defmethod cart-y ((p polar))
  (* (radius p) (sin (angle p))))

(defclass line ()
 ((start :initarg :start :accessor line-start)
  (end   :initarg :end   :accessor line-end)))

(defmethod print-object ((lin line) stream)
  (format stream "[ÎÒÐÅÇÎÊ ~s ~s]"
          (line-start lin) (line-end lin)))

(defun eqcart (f s) 
  (let ((x1 (cart-x f))
        (y1 (cart-y f))
        (x2 (cart-x s))
        (y2 (cart-y s)))
       (and (= x1 x2) (= y1 y2))))

(defun line-intersections (lines)
  (let ((tmp lines)
        (int)
        (res))
    (dolist (i lines)
      (setq tmp (rest tmp))
       (dolist (j tmp)
         (setq int (intersect i j))
         (if int
         (if (null res)
          (setq res (list int))
          (if (not (member int res :test #'eqcart))
              (setq res (append res (list int))))))))
    res))

(defun intersect (i j) 
  (let ((x11 (cart-x (line-start i)))
        (y11 (cart-y (line-start i)))
        (x12 (cart-x (line-end i)))
        (y12 (cart-y (line-end i)))
        (x21 (cart-x (line-start j)))
        (y21 (cart-y (line-start j)))
        (x22 (cart-x (line-end j)))
        (y22 (cart-y (line-end j)))
        (k1)
        (k2)
        (b1)
        (b2)
        (x)
        (y))
       (if (eql x11 x12)
           (let ()
            (setq x x11)
            (if (eql y21 y22)
               (setq y y21)
               (let()
                (setq k2 (/ (- y22 y21) (- x22 x21)))
                (setq b2 (/ (- (* x22 y21) (* x21 y22)) (- x22 x21)))
                (setq y (+ (* x k2) b2)))))
           (if (eql x21 x22)
               (let()
                (setq x x21)
                (if (eql y11 y12)
                  (setq y y11)
                  (let()
                   (setq k1 (/ (- y12 y11) (- x12 x11)))
                   (setq b1 (/ (- (* x12 y11) (* x11 y12)) (- x12 x11)))
                   (setq y (+ (* x k1) b1)))))
               (if (eql y11 y12)
                   (let()
                    (setq y y11)
                    (if (eql x21 x22)
                        (setq x x21)
                        (let()
                         (setq k2 (/ (- y22 y21) (- x22 x21)))
                         (setq b2 (/ (- (* x22 y21) (* x21 y22)) (- x22 x21)))
                         (setq x (/ (- y b2) k2)))))
                   (if (eql y21 y22)
                    (let()
                     (setq y y21)
                     (if (eql x11 x12)
                        (setq x x11)
                        (let()
                         (setq k1 (/ (- y12 y11) (- x12 x11)))
                         (setq b1 (/ (- (* x12 y11) (* x11 y12)) (- x12 x11)))
                         (setq x (/ (- y b1) k1)))))
                    (let()
                      ;there are no lines parallel to OX or OY
                      (setq k1 (/ (- y12 y11) (- x12 x11)))
                      (setq k2 (/ (- y22 y21) (- x22 x21)))
                      (setq b1 (/ (- (* x12 y11) (* x11 y12)) (- x12 x11)))
                      (setq b2 (/ (- (* x22 y21) (* x21 y22)) (- x22 x21)))
                      (setq x (/ (- b2 b1) (- k1 k2)))
                      (setq y (+ (* x k1) b1))))))) 
        ;we check that it is the lines (segments) that intersect, and not their continuations
        (if (and (or (and (and (>= x x11) (<= x x12)) (and (>= x x21) (<= x x22)))   ;x in [x11,x12] and [x21,x22]
                     (and (and (>= x x12) (<= x x11)) (and (>= x x21) (<= x x22)))   ;x in [x12,x11] and [x21,x22]
                     (and (and (>= x x11) (<= x x12)) (and (>= x x22) (<= x x21)))   ;x in [x11,x12] and [x22,x21]
                     (and (and (>= x x12) (<= x x11)) (and (>= x x22) (<= x x21))))  ;x in [x12,x11] and [x22,x21]
                 (or (and (and (>= y y11) (<= y y12)) (and (>= y y21) (<= y y22)))   ;y in [y11,y12] and [y21,y22]
                     (and (and (>= y y12) (<= y y11)) (and (>= y y21) (<= y y22)))   ;y in [y12,y11] and [y21,y22]
                     (and (and (>= y y11) (<= y y12)) (and (>= y y22) (<= y y21)))   ;y in [y11,y12] and [y22,y21]
                     (and (and (>= y y12) (<= y y11)) (and (>= y y22) (<= y y21))))) ;y in [y12,y11] and [y22,y21]
        (make-instance 'cart :x x :y y))))