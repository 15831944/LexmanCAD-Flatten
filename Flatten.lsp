;; FUNCTION TO FLATTEN A 3D PLINE TO A 2D PLINE AT A USER DEFINED ELEVATION
(defun C:LCAD_Flatten ( / kw ss i pl vl pts pt elev el avgEl *error*)
   
  ;; DEFINE ERROR HANDLING
  (defun *error* ( msg / )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    );_if
    (exit)
    (princ)
  );_defun *error*
    
  ;;INITIATE
  (setq avgEl nil)
  
  ;; SELECT A 3DPOLY
  (princ "\nSelect 3D polyline(s) to flatten: ")

  ;; DFX CODE 70 > 8 MEANS 3D PLINE
  (setq ss (ssget '((0 . "POLYLINE") (-4 . "&=") (70 . 8))))

  ;; IF SOMETHING WAS SELECTED
  (if ss
    (progn
        
      (setq kw (getreal "\nSpecify target elevation <0> : "))
      
      (cond
        ((= kw nil) (setq elev 0.0))
        ((setq elev kw))
      );_cond
    );_progn

    ;; OTHERWISE...
    (princ "No 3D Polyline(s) detected")
  );_if
      
  ;; PROCESS EACH POLYLINE
  (setq i 0)
  (while (and ss (setq pl (ssname ss i)))

    ;; GET 3DPOLY E LIST
    (setq el (entget pl (list "*")))
    
    ;; CHECK IF IT IS A 3DPOLY
    (if (/= 0 (logand (cdr (assoc 70 el)) 8))
      (progn
  
        ;; MAKE 2DPOLY HEADER
        (entmake (list (cons 0 "POLYLINE")
                       (if (assoc 6 el) (assoc 6 el) (cons 6 "BYLAYER"))
                       (assoc 8 el)
		       (cons 38 elev)
                       (if (assoc 62 el) (assoc 62 el) (cons 62 256))
                       (assoc 67 el)
                       (cons 70 (- (cdr (assoc 70 el)) 8))
                       (if (assoc 48 el) (assoc 48 el) (cons 48 1.0))
                       (if (assoc 75 el) (assoc 75 el) (cons 75 0))
                  ));_list entmake

        ;; MAKE VERTICIES
        (setq en (entnext pl))
        (setq vl (entget en))
        ;; WHILE THERE IS VERTEX IN THE 3DPOLY
        (while (= "VERTEX" (cdr (assoc 0 vl)))
 	  (setq pt (cdr (assoc 10 vl)))

          ;; MAKE 2DPOLY VERTEX
          (entmake (list (cons 0 "VERTEX")
                         (assoc 8 vl)
                         (cons 10 (list (car pt) (cadr pt) elev))
                         ;; SUBTRACT 32 FROM THE VERTICIE'S CODE 70
                         (cons 70 (- (cdr (assoc 70 vl)) 32 ))
          )        );_list _entmake

          ;; STEP TO NEXT VERTEX
          (setq en (entnext en))
          (setq vl (entget en))

        );_while

        ;; FINISH 2DPOLY MAKING
        (entmake (list (cons 0 "SEQEND")
                       (assoc 8 el)))

        ;; ERASE 3DPOLY
        (entdel pl)

        ;; REDRAW CONVERTED POLYLINE
        (redraw (entlast))

      );_progn
    );_if

    (setq i (1+ i))

  );_while

  (princ)

);_defun C:LCAD_Flatten



(defun C:LCAD_Raise ( / ss i pl vl pts pt elev el *error*)

  ;; DEFINE ERROR HANDLING
  (defun *error* ( msg / )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    );_if
    (exit)
    (princ)
  );_defun *error*

  ;; SELECT A 2DPLINE TO CONVERT INTO A 3D PLINE
  (princ "\nSelect 2D polyline(s) to raise: ")

  ;; ONLY SELECT POLYLINES WHOSE 70 CODE EXCLUDES 8 16 32 64
  (setq ss (ssget '((-4 . "<and")
                      (-4 . "<or")
                        (0 . "LWPOLYLINE")
                        (0 . "POLYLINE")
                        (0 . "SPLINE")
                      (-4 . "or>")
                      (-4 . "<not")
                        (-4 . "<or")
                          (-4 . "&") (70 . 8)
                          (-4 . "&") (70 . 16)
                          (-4 . "&") (70 . 32)
                          (-4 . "&") (70 . 64)
                        (-4 . "or>")
                      (-4 . "not>")
                    (-4 . "and>"))))
  
  ;; DETERMINE PROCESSING METHOD IF A 2D POLY LINE WAS SELECTED
  (if ss
    (progn
    
      ;; PROCESS EACH POLYLINE
      (setq i 0)
      (while (and ss (setq pl (ssname ss i)))

        ;; LINE POINTS VARIABLE
        (setq linePts nil)
          
        ;; ENSURE A "HEAVY" POLYLINE
        (if (= "LWPOLYLINE" (cdr (assoc 0 (entget pl))))
          (command "._CONVERTPOLY" "H" pl ""));_if

        ;; GET 2DPOLY ENTITY LIST
        (setq el (entget pl (list "*")))

        ;; GET CURRENT ELEVATION OF 2D POLY IF currh IS T USING THE FIRST VERTEX
        (setq elev (cadddr (assoc 10 (entget (entnext (cdr (car el)))))))
    
        ;; CHECK IF IT IS A 2DPOLY
        (if (/= 1 (logand (cdr (assoc 70 el)) 8))
          (progn
  
            ;; MAKE 3DPOLY HEADER
            (entmake (list
                       (cons 0 "POLYLINE")
                       (if (assoc 6 el) (assoc 6 el) (cons 6 "BYLAYER"))
                       (assoc 8 el)
		       (cons 38 elev)
                       (if (assoc 62 el) (assoc 62 el) (cons 62 256))
                       (assoc 67 el)
                       (cons 70 (+ (cdr (assoc 70 el)) 8))
                       (if (assoc 48 el) (assoc 48 el) (cons 48 1.0))
                       (if (assoc 75 el) (assoc 75 el) (cons 75 0))));_list entmake

            ;; MAKE VERTICIES
            (setq en (entnext pl))
            (setq vl (entget en))
            ;; WHILE THERE IS VERTEX IN THE 3DPOLY
            (while (= "VERTEX" (cdr (assoc 0 vl)))
    	      (setq pt (cdr (assoc 10 vl)))

              ;; MAKE 2DPOLY VERTEX
              (entmake (list
                         (cons 0 "VERTEX")
                         (assoc 8 vl)
                         (cons 10 (list (car pt) (cadr pt) elev))
                         ;; ADD 32 FROM THE VERTICIE'S CODE 70
                         (cons 70 (+ (cdr (assoc 70 vl)) 32 ))));_list _entmake

              ;; STEP TO NEXT VERTEX
              (setq en (entnext en))
              (setq vl (entget en))

            );_while

            ;; FINISH 2DPOLY MAKING
            (entmake (list
                       (cons 0 "SEQEND")
                       (assoc 8 el)))

            ;; ERASE 2DPOLY
            (entdel pl)

            ;; REDRAW CONVERTED POLYLINE
            (redraw (entlast))

          );_progn
        );_if

        (setq i (1+ i))

      );_while
    );_progn
    (princ "\nNothing Selected")
  );_if

  (princ)

);_defun C:LCAD_Raise

;; PRINT A LOAD MESSAGE
(princ "\n FLATTEN v.2.1 Loaded Successfully")
(princ "\n Like us in the app store or on facebook.com/LexmanCAD\n")
(princ)
(princ)
