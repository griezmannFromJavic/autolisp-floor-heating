(defun intersect-lines (point1 vector1 point2 vector2 / A B C D E F det u v intersection)
  ; Define the coefficients for the system of equations
  (setq A (car vector1))
  (setq B (- (car vector2)))
  (setq C (cadr vector1))
  (setq D (- (cadr vector2)))

  ; Right-hand side of the system
  (setq E (- (car point2) (car point1)))
  (setq F (- (cadr point2) (cadr point1)))

  ; Solve for u and v
  (setq det (- (* A D) (* B C))) ; Determinant

  ; Ensure the lines are not parallel
  (if (/= det 0)
    (progn
      (setq u (/ (- (* E D) (* B F)) (float det)))
      (setq v (/ (- (* A F) (* E C)) (float det)))

      ; Calculate intersection point
      (setq intersection (list (+ (car point1) (* u A)) (+ (cadr point1) (* u C))))
    )
    (setq intersection nil) ; Lines are parallel
  )
  intersection
)

(defun normalized-normal (vector / normal magnitude normalized-normal)
  ; Given vector (x, y), the normal vector using right-hand rule is (y, -x)
  (setq normal (list (cadr vector) (- (car vector))))
  (setq magnitude (sqrt (+ (* (car normal) (car normal)) (* (cadr normal) (cadr normal)))))
  (if (/= magnitude 0)
    (setq normalized-normal (list (/ (car normal) magnitude) (/ (cadr normal) magnitude)))
    (setq normalized-normal nil) ; Handle the zero vector case
  )
  normalized-normal
)

(defun line-center (A B)
	(mapcar (lambda (x y) (/ (+ x y) 2.0)) A B)
		)

(defun line-vector (A B)
	(mapcar - B A)
)

(defun offseted-line-center (A B offset)
	(mapcar +
		(line-center A B)
		(mapcar (lambda (x) (* offset x)) (normalized-normal (line-vector B A)))
		))

(defun offseted-intersection (A B C offset)
; intersection of offseted lines AB and BC
	(intersect-lines
		(offseted-line-center A B offset)
		(line-vector A B)
		(offseted-line-center B C offset)
		(line-vector B C)
		))
