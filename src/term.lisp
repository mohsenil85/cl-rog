(in-package :cl-user)
(ql:quickload "cl-charms")
(defpackage term
  (:use :cl
        :cl-charms))
(in-package :term)



(defstruct player x y hp)

(defparameter *player* (make-player  :x 20 :y 20 :hp 20))

(defun move (player dir)
  (format t "~A~A~%" player dir))


(defun paint (ch)
  (with-restored-cursor *standard-window*
    (write-char-at-cursor *standard-window* ch)))

(defun main ()
  (with-curses  ()
    (disable-echoing)
    (cl-charms/low-level:curs-set 0)

    (enable-raw-input :interpret-control-characters t)
    (enable-non-blocking-mode *standard-window*)

    ;(let ((p (make-player :x 10 :y 7 :hp 10)))


      (progn
        (loop :named main-loop
              :do (progn

                    (refresh-window *standard-window*)
                    (paint #\Space)

                    (let ((c (get-char *standard-window* :ignore-error t) )) 
                      (case c
                      ((nil) nil)
                      ((#\k) (decf (player-y *player*  )))
                      ((#\j) (incf (player-y *player*  )))
                      ((#\h) (decf (player-x *player*  )))
                      ((#\l) (incf (player-x *player*  )))

                      ((#\q) (return-from main-loop))))
                    
                    (move-cursor *standard-window* 
                                 (player-x *player*) 
                                 (player-y *player*))
                    (paint #\@)
                    
                    
                    )
              
              ))));)

(main)
