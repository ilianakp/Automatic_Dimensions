;; Returns a list with duplicate elements removed.
(defun LM:Unique ( l / x r )
    (while l
        (setq x (car l)
              l (vl-remove x (cdr l))
              r (cons x r)
        )
    )
    (reverse r)
)

;;; start the program
(defun c:auto_dim (/)
  
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
  
  ;; ask the user to type offset value
  (setq offset (getreal "\nWhat is the offset value? "))

  (setq osm (getvar "osmode")) ; gets osnap settings and assigns to osm
  (setvar "osmode" 0) ; turns osnap settings off

  ;;initialize the lists of start and end points
  (setq xs (list ))
  (setq ys (list ))
  (setq pts (list ))
  
  (setq counter 0)
  (repeat (sslength line)
    (setq entitydata (entget (ssname line counter)))

    (setq startpt (cdr (assoc 10 entitydata))
          endpt   (cdr (assoc 11 entitydata))
      )
    (setq xs (append xs (list (car startpt))))
    (setq ys (append ys (list (cadr startpt))))
    (setq xs (append xs (list (car endpt))))
    (setq ys (append ys (list (cadr endpt))))
    
    (setq pts (append pts (list (list (car startpt) (cadr startpt)))))
    (setq pts (append pts (list (list (car endpt) (cadr endpt)))))
    ;; Increment the counter to move on to the next line in the Selection Set
    (setq counter (1+ counter))
  ) ;; End REPEAT
  
  (setq xmin (apply 'min xs))
  (setq xmax (apply 'max xs))
  (setq ymin (apply 'min ys))
  (setq ymax (apply 'max ys))
  (setq xmid (/ (+ xmin xmax) 2))
  (setq ymid (/ (+ ymin ymax) 2))
  
  ;; make 4 sets of dimensions
  (setq up (list ))
  (setq down (list ))
  (setq right (list ))
  (setq left (list ))
  (setq j 0)
  (while (< j (vl-list-length pts))
    (if (>= (cadr (nth j pts)) ymid)
      (setq up (append up (list (nth j pts))))
      (setq down (append down (list (nth j pts))))
    )
    (if (>= (car (nth j pts)) xmid)
      (setq right (append right (list (nth j pts))))
      (setq left (append left (list (nth j pts))))
    )
    (setq j (+ j 1))
  )
    
  (setq up (vl-sort up '(lambda (A B) (< (car A) (car B)))))
  (setq down (vl-sort down '(lambda (A B) (< (car A) (car B)))))
  (setq right (vl-sort right '(lambda (A B) (< (cadr A) (cadr B)))))
  (setq left (vl-sort left '(lambda (A B) (< (cadr A) (cadr B)))))
  
  ;; find the y value of xmax
  (if (> (car (last up)) (car (last down)))
    (setq xmax_y (cadr (last up)))
    (setq xmax_y (cadr (last down)))
  )
  ;; find the x value of ymax
  (if (> (cadr (last right)) (cadr (last left)))
    (setq ymax_x (car (last right)))
    (setq ymax_x (car (last left)))
  )
  ;; find the y value of xmin
  (if (< (car (car up)) (car (car down)))
    (setq xmin_y (cadr (car up)))
    (setq xmin_y (cadr (car down)))
  )
  ;; find the x value of ymin
  (if (< (cadr (car right)) (car (cadr left)))
    (setq ymin_x (car (car right)))
    (setq ymin_x (car (car left)))
  )
  
  ;; loop into the 4 sets to make dimensions
  (setq i 0)
  (while (< i (- (vl-list-length up) 1))
    (if (not (equal (car (nth i up)) (car (nth (+ i 1) up)) 0.1))
      (progn
        (setq start_up (list (car (nth i up)) (cadr (nth i up)) 0))
        (setq end_up  (list (car (nth (+ i 1) up)) (cadr (nth (+ i 1) up)) 0))
        (setq place_up (list (car (nth (+ i 1) up)) (+ ymax offset) 0))
        (command "dimlinear" start_up end_up "H" place_up)
      )
    )
    (setq i (+ i 1))
  )
  (setq k 0)
  (while (< k (- (vl-list-length down) 1))
    (if (not (equal (car (nth k down)) (car (nth (+ k 1) down)) 0.1))
      (progn
        (setq start_down (list (car (nth k down)) (cadr (nth k down)) 0))
        (setq end_down  (list (car (nth (+ k 1) down)) (cadr (nth (+ k 1) down)) 0))
        (setq place_down (list (car (nth (+ k 1) down)) (- ymin offset) 0))
        (command "dimlinear" start_down end_down "H" place_down)
      )
    )
    (setq k (+ k 1))
  )
    (setq l 0)
  (while (< l (- (vl-list-length right) 1))
    (if (not (equal (cadr (nth l right)) (cadr (nth (+ l 1) right)) 0.1))
      (progn
        (setq start_right (list (car (nth l right)) (cadr (nth l right)) 0))
        (setq end_right  (list (car (nth (+ l 1) right)) (cadr (nth (+ l 1) right)) 0))
        (setq place_right (list (+ xmax offset) (cadr (nth (+ l 1) right)) 0))
        (command "dimlinear" start_right end_right "V" place_right)
      )
    )
    (setq l (+ l 1))
  )
  (setq s 0)
  (while (< s (- (vl-list-length left) 1))
    (if (not (equal (cadr (nth s left)) (cadr (nth (+ s 1) left)) 0.1))
      (progn
        (setq start_left (list (car (nth s left)) (cadr (nth s left)) 0))
        (setq end_left  (list (car (nth (+ s 1) left)) (cadr (nth (+ s 1) left)) 0))
        (setq place_left (list (- xmin offset) (cadr (nth (+ s 1) left)) 0))
        (command "dimlinear" start_left end_left "V" place_left)
      )
    )
    (setq s (+ s 1))
  )
  ;; check if the max value from the list is the overall max
  ;; if not draw extra dimension
  ;;up
  (if (and (< (car (last up)) xmax) (not (equal (car (last up)) xmax 0.1)))
    (progn
      (setq start (list (car (last up)) (cadr (last up)) 0))
      (setq end  (list xmax xmax_y 0))
      (setq place (list xmax (+ ymax offset) 0))
      (command "dimlinear" start end "H" place)
    )
  )
  (if (and (> (car (car up)) xmin) (not (equal (car (car up)) xmin 0.1)))
    (progn
      (setq start (list xmin xmin_y 0))
      (setq end  (list (car (car up)) (cadr (car up)) 0))
      (setq place (list xmin (+ ymax offset) 0))
      (command "dimlinear" start end "H" place)
    )
  )
  ;;down
  (if (and (< (car (last down)) xmax) (not (equal (car (last down)) xmax 0.1)))
    (progn
      (setq start (list (car (last down)) (cadr (last down)) 0))
      (setq end  (list xmax xmax_y 0))
      (setq place (list xmax (- ymin offset) 0))
      (command "dimlinear" start end "H" place)
    )
  )
  (if ( and (> (car (car down)) xmin) (not (equal (car (car down)) xmin 0.1)))
    (progn
      (setq start (list xmin xmin_y 0))
      (setq end  (list (car (car down)) (cadr (car down)) 0))
      (setq place (list xmin (- ymin offset) 0))
      (command "dimlinear" start end "H" place)
    )
  )
  ;;right
  (if (and (< (cadr (last right)) ymax) (not (equal (cadr (last right)) ymax 0.1)))
    (progn
      (setq start (list (car (last right)) (cadr (last right)) 0))
      (setq end  (list ymax_x ymax 0))
      (setq place (list (+ xmax offset) ymax 0))
      (command "dimlinear" start end "V" place)
    )
  )
  (if (and (> (cadr (car right)) ymin) (not (equal (cadr (car right)) ymin 0.1)))
    (progn
      (setq start (list ymin_x ymin 0))
      (setq end  (list (car (car right)) (cadr (car right)) 0))
      (setq place (list (+ xmax offset) ymin 0))
      (command "dimlinear" start end "V" place)
    )
  )
  ;;left
  (if (and (< (cadr (last left)) ymax) (not (equal (cadr (last left)) ymax 0.1)))
    (progn
      (setq start (list (car (last left)) (cadr (last left)) 0))
      (setq end  (list ymax_x ymax 0))
      (setq place (list (- xmin offset) ymax 0))
      (command "dimlinear" start end "V" place)
    )
  )
  (if (and (> (cadr (car left)) ymin) (not (equal (cadr (car left)) ymin 0.1)))
    (progn
      (setq start (list ymin_x ymin 0))
      (setq end  (list (car (car left)) (cadr (car left)) 0))
      (setq place (list (- xmin offset) ymin 0))
      (command "dimlinear" start end "V" place)
    )
  )
  ;; ask user for overall height and width
  (initget "Height Width Both None")
  (setq overall (cond ((getkword "\nOverall height and width? [Height/Width/Both/None] <Both>: ")) ("Both")))
  (if (not (equal overall "None"))
    (progn
      (if (equal overall "Height")
       (progn
         (setq start_height (list ymin_x ymin 0))
         (setq end_height  (list ymax_x ymax 0))
         (setq place_height (list (+ xmax (* offset 2)) ymax 0))
         (command "dimlinear" start_height end_height "V" place_height)
       )
      )
      (if (equal overall "Width")
       (progn
         (setq start_width (list xmin xmin_y 0))
         (setq end_width  (list xmax xmax_y 0))
         (setq place_width (list xmax (+ ymax (* offset 2))))
         (command "dimlinear" start_width end_width "H" place_width)
       )
      )
      (if (equal overall "Both")
       (progn
         ;; width
         (setq start_width (list xmin xmin_y 0))
         (setq end_width  (list xmax xmax_y 0))
         (setq place_width (list xmax (+ ymax (* offset 2))))
         (command "dimlinear" start_width end_width "H" place_width)
         ;; height
         (setq start_height (list ymin_x ymin 0))
         (setq end_height  (list ymax_x ymax 0))
         (setq place_height (list (+ xmax (* offset 2)) ymax 0))
         (command "dimlinear" start_height end_height "V" place_height)
       )
      )
    )
  )
  (setvar "osmode" osm) ;; sets osnap on again
  ;;; end code
  (princ)
)
