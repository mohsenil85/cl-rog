(in-package :cl-user)
(ql:quickload "cl-charms")
(defpackage term
  (:use :cl
        :cl-charms))
(in-package :term)



(defstruct player x y hp)

(defparameter *player* (make-player  :x 20 :y 20 :hp 20))
(defparameter *running* t)
(defparameter *world-age* 0)

(defun move (player dir)
  (format t "~A~A~%" player dir))

(defun init ()
  (disable-echoing)
  (cl-charms/low-level:curs-set 0)
  (enable-raw-input :interpret-control-characters t)
  (enable-non-blocking-mode *standard-window*))

(defun paint (ch)
  (with-restored-cursor *standard-window*
    (write-char-at-cursor *standard-window* ch)))

(defun quit ()
  (progn
    (setf *running* nil)
    (sb-sys:os-exit 0)))

(defun get-input ()
  (let ((c (get-char *standard-window* :ignore-error t) )) 
    (case c
      ((nil) nil)
      ((#\k) (decf (player-y *player*  )))
      ((#\j) (incf (player-y *player*  )))
      ((#\h) (decf (player-x *player*  )))
      ((#\l) (incf (player-x *player*  )))
      ((#\q) (quit)))))

(defun update-world ()
  (progn

    (incf *world-age*  )
    (move-cursor *standard-window* 
                 (player-x *player*) 
                 (player-y *player*))
    (paint #\@)
    (refresh-window *standard-window*)
    (sleep .1)
    (paint #\Space)))

(defun main ()
  (with-curses  ()
    (init)
    (loop :named main-loop
          :while *running*
          :do 

          (get-input)
          (update-world)
          )))

(main)
