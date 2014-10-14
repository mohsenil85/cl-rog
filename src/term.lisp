(in-package :cl-user)
(ql:quickload "cl-charms")
(defpackage term
  (:use :cl
        :cl-charms))
(in-package :term)



(defstruct player x y hp)
(defstruct plant x y age)

(defparameter *player* (make-player  :x 20 :y 20 :hp 20))
(defparameter *running* t)
(defparameter *world-age* 0)
(defparameter *plants* '() )


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

(defun plant ()
  (multiple-value-bind (x y)
    (charms:cursor-position *standard-window*)
     (push (make-plant :x x :y y :age *world-age*) *plants*)))

(defun draw-plants ()
  (loop :for p 
        :in *plants*
        :do
        (with-restored-cursor *standard-window*
          (charms:write-char-at-point *standard-window* 
                                      #\* 
                                      (plant-x p) 
                                      (plant-y p)))))

(defun get-input ()
  (let ((c (get-char *standard-window* :ignore-error t) )) 
    (case c
      ((nil) nil)
      ((#\k) (decf (player-y *player*  )))
      ((#\j) (incf (player-y *player*  )))
      ((#\h) (decf (player-x *player*  )))
      ((#\l) (incf (player-x *player*  )))
      ((#\Space) (plant))
      ((#\q) (quit)))))

(defun update-world ()
  (progn
    (incf *world-age*  )
    (if (char-equal #\@ 
                    (char-at-cursor *standard-window* )) 
      (paint #\Space))
    (move-cursor *standard-window* 
                 (player-x *player*) 
                 (player-y *player*))
    (draw-plants)
    (paint #\@)
    (refresh-window *standard-window*)
    (sleep .1)))

(defun main ()
  (with-curses  ()
    (init)
    (loop :named main-loop
          :while *running*
          :do 
          (get-input)
          (update-world))))

(main)
