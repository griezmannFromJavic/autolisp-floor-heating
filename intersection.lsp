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

; Example usage:
(defun c:TestIntersection ()
  (setq point1 (list 0.0 0.0))
  (setq vector1 (list 1.0 1.0))
  (setq point2 (list 0.0 1.0))
  (setq vector2 (list 1.0 -1.0))

  (setq intersection (intersect-lines point1 vector1 point2 vector2))
  
  (if intersection
    (princ (strcat "\nIntersection Point: " (vl-princ-to-string intersection)))
    (princ "\nLines are parallel and do not intersect.")
  )
  (princ)
)
