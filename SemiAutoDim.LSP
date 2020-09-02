;;; start the program
(defun c:semi_auto (/)
  
  (defun *error* ( msg )
    (setvar "osmode" osm)
    (if osm (setvar 'osmode osm))
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
        (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  
  ;;; user input
  (setq line (ssget '((0 . "LINE"))))
  
  (setq osm (getvar "osmode")) ; gets osnap settings and assigns to osm
  (setvar "osmode" 0) ; turns osnap settings off
  
  (setq plc (getpoint "\nPick the placement point "))

  ;; loop to check if there is a line with inclination
  (setq incl 1)
  (setq count 0)
  (repeat (sslength line)
    (setq entitydata (entget (ssname line count)))
    (setq startpoint (cdr (assoc 10 entitydata))
          endpoint   (cdr (assoc 11 entitydata))
    )
    ;; If it's not vertical neither horizontal
    (if (and (not (equal (car startpoint) (car endpoint) 0.1))
            (not (equal (cadr startpoint) (cadr endpoint) 0.1)))
      (setq incl 0)
    )
    (setq count (1+ count))
  ) ;; End REPEAT
  
  ;; if there is an inclination in any of the lines, ask the user for orientation
  (if (equal incl 0)
    (progn
      ;; ask user if the dimensions will be in the current layer or the default
      (initget 1 "Horizontal Vertical")
      (setq orient (getkword "\nChoose [Horizontal/Vertical]: "))
      (if (equal orient "Horizontal")
        (setq orient "H")
        (setq orient "V")
      )
    )
  )
  
  (setq counter 0)
  (repeat (sslength line)
    (setq entitydata2 (entget (ssname line counter)))
    (setq startpt (cdr (assoc 10 entitydata2))
          endpt   (cdr (assoc 11 entitydata2))
    )
    
    (if (equal incl 0)
      (command "_dimlinear" startpt endpt orient plc)
      (command "_dimlinear" startpt endpt plc)
    )
    
    (setq counter (1+ counter))
    ) ;; End REPEAT

  (setvar "osmode" osm) ;; sets osnap on again
  ;;; end code
  (princ)
)
