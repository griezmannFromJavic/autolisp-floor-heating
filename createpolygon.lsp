(defun c:StorePoints ( / pt)
  (if (not (boundp 'pointList))
    (setq pointList '()) ; Initialize pointList if not already defined
  )
  (while (setq pt (getpoint "\nSpecify a point (Press Enter to finish): "))
    (setq pointList (cons pt pointList)) ; Add the point to the list
  )
  (setq pointList (reverse pointList)) ; Reverse the list to maintain input order
  (princ "\nPoints stored successfully.")
  (princ) ; Exit quietly
)

(defun c:ShowPoints ()
  (if (boundp 'pointList)
    (progn
      (princ "\nStored Points: ")
      (princ pointList) ; Display the stored points
    )
    (princ "\nNo points stored.") ; Message if no points are stored
  )
  (princ) ; Exit quietly
)

(defun c:ClearPoints ()
  (setq pointList '()) ; Clear the stored points
  (princ "\nPoint list cleared.")
  (princ) ; Exit quietly
)

(defun c:CreatePolygon ( / ptList)
  (if (and (boundp 'pointList) pointList)
    (progn
      (setq ptList (append pointList (list (car pointList)))) ; Close the polygon by adding the first point at the end
      (command "_.PLINE") ; Start the PLINE command
      (foreach pt ptList
        (command pt) ; Add each point
      )
      (command "") ; End the PLINE command
      (princ "\nPolygon created successfully.")
    )
    (princ "\nNo points stored.") ; Message if no points are stored
  )
  (princ) ; Exit quietly
)

(princ "\nType StorePoints to start capturing points.")
(princ "\nType ShowPoints to display captured points.")
(princ "\nType ClearPoints to clear the stored points.")
(princ "\nType CreatePolygon to create a polygon from stored points.")
(princ)
