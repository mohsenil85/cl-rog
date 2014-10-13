(in-package :cl-user)
(ql:quickload "cl-charms")
(defpackage term
  (:use :cl
        :cl-charms))
(in-package :term)


;
;(dotimes '10
;  (progn
;  (sleep (random .3))
;  (princ "foo")))
;  

(defstruct player x y hp)

(defun move (player dir)
  (format t "~A~A~%" player dir))

(defun erase-old ()
  (with-restored-cursor *standard-window*
    (write-char-at-cursor *standard-window* #\Space)))
(defun paint ()
  (with-restored-cursor *standard-window*
    (write-char-at-cursor *standard-window* #\@)))
;(let ((p (make-player :x 10 :y  7 :hp 10))) 
  ;(move p :up))

(defun main ()
  (with-curses  ()
    (disable-echoing)
    (enable-raw-input :interpret-control-characters t)
    (enable-non-blocking-mode *standard-window*)

    (let ((p (make-player :x 10 :y 7 :hp 10)))

    ;(multiple-value-bind (width height)
      ;(window-dimensions *standard-window*)

      (progn
        (loop :named main-loop
              :for c := (get-char *standard-window* :ignore-error t) 
              :with x := (player-x p)
              :with y := (player-y p)

              :do (progn
                    (refresh-window *standard-window*)
                    (erase-old)
                    (case c
                      ((nil) nil)
                      ((#\k) (decf y))
                      ((#\j) (incf y))
                      ((#\h) (decf x))
                      ((#\l) (incf x))

                      ((#\q) (return-from main-loop)))
                    
                    (move-cursor *standard-window* x y)
                    (paint)
                    
                    
                    )
              
              )
        ))));)

(main)
