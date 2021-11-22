;;;; This is a small package of miscellaneous functions

(in-package :allegro-test)

(defun random-color ()
  "Generate a random RGB Allegro Color"
  (al:map-rgb (random 256)
              (random 256)
              (random 256)))


(defun HSL->RGB (H S L &optional A)
  "Computes an Allegro RGB color or RGBA"
  (let* ((C (* (- 1.0
		  (abs (-
			(* 2
			   L)
			1)))
	       S))
	 (X (* C
	       (- 1.0
		  (abs (- (mod (/ H
				  60.0)
			       2)
			  1)))))
	 (M (- L
	       (/ C 2.0)))
	 (R 0.0)
	 (G 0.0)
	 (B 0.0))
    ;; Find the hue
    (cond ((and (<= 0 H) (< H 60))
	   (setf R C  G X  B 0))
	  ((and (<= 60 H) (< H 120))
	   (setf R X  G C  B 0))
	  ((and (<= 120 H) (< H 180))
	   (setf R 0  G C  B X))
	  ((and (<= 180 H) (< H 240))
	   (setf R 0  G X  B C))
	  ((and (<= 240 H) (< H 300))
	   (setf R X  G 0  B C))
	  ((and (<= 300 H) (< H 360))
	   (setf R C  G 0  B X)))
    ;; Convert RGB to byte values
    (setf R (floor (* (+ R M) 255))
	  G (floor (* (+ G M) 255))
	  B (floor (* (+ B M) 255)))
    (if A
	(al:map-rgba R G B A)
	(al:map-rgb R G B))))


(eval-when (:compile-toplevel :load-toplevel)
  ;; must be defined before the macro in a different file, or if in the same
  ;; file, then wrapped in the eval-when.
  (defun line-it-harder (x1 y1 x2 y2 func)
    (let* ((dist-x (abs (- x1 x2)))
           (dist-y (abs (- y1 y2)))
           (steep (> dist-y dist-x)))
      ;; Below flip coordinates to orient them into a single case
      (when steep
        (psetf x1 y1 y1 x1
               x2 y2 y2 x2))
      
      (when (> x1 x2)
        (psetf x1 x2 x2 x1
               y1 y2 y2 y1))

      (let* ((delta-x (- x2 x1))
             (delta-y (abs (- y1 y2)))
             (err (floor delta-x 2))
             (y-step (if (< y1 y2) 1 -1))
             (y y1))

        (let ((arg-x 0)
              (arg-y 0))
          (loop
            :for x :upfrom x1 :to x2
            :do (if steep
                    (setf arg-x y
                          arg-y x)
                    (setf arg-x x
                          arg-y y))
		
                (funcall func arg-x arg-y)
                (setf err (- err delta-y))

                (when (< err 0)
                  (incf y y-step)
                  (incf err delta-x)))))))

  
  (defmacro with-positions-between ((x1 y1 x2 y2 var-x var-y) &body body)
    "Step through x y positions between x1 y1 x2 y2 with user defined variables"
    `(line-it-harder ,x1 ,y1 ,x2 ,y2
                     (lambda (,var-x ,var-y)
                       ,@body))))
