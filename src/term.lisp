;;;todo
;;;monsters
;;;draw terrain
;;;impl pause
;;;impl harvest from plants

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
  (enable-non-blocking-mode *standard-window*)
  (cl-charms/low-level:start-color)
  )

(defun paint (ch)
  (with-restored-cursor *standard-window*
    (write-char-at-cursor *standard-window* ch)))

(defun paint-at-point (ch x y)
  (multiple-value-bind (width height)
    (window-dimensions *standard-window*)
    (with-restored-cursor *standard-window*
    (write-char-at-point *standard-window* ch 
                         (mod x width) 
                         (mod y height)))) )

(defun string-point (str x y)
  (with-restored-cursor *standard-window*
     (write-string-at-point *standard-window* str x y)))

(defun quit ()
  (progn
    (setf *running* nil)
    (sb-sys:os-exit 0)))

(defun plant ()
  (multiple-value-bind (x y)
    (charms:cursor-position *standard-window*)
     (push (make-plant :x x :y y :age 0) *plants*)))


(defmacro draw-plant* (p char)
  `(with-restored-cursor *standard-window*
    (write-char-at-point *standard-window*
                         ,char
                         (plant-x ,p)
                         (plant-y ,p))))

(defun plant-1 (p)
   (let ((x (plant-x p))
        (y (plant-y p)))
     (progn
       (paint-at-point #\- (1- x) y))
       (paint-at-point #\- (- x 2) y) 
       (paint-at-point #\- (1+ x) y) 
       (paint-at-point #\- (+ x 2) y) 
       (paint-at-point #\|  x (1- y))
       (paint-at-point #\|  x (1+ y))))

(defun plant-2 (p)
  (let ((x (plant-x p))
        (y (plant-y p)))
    (progn
      (paint-at-point #\* (1- x) (- y 2))
      (paint-at-point #\* (1+ x) (- y 2))
      (paint-at-point #\* (1- x) (+ y 2))
      (paint-at-point #\* (1+ x) (+ y 2))

      (paint-at-point #\* (+ x 4) (1+ y))
      (paint-at-point #\* (- x 4) (1+ y))
      (paint-at-point #\* (+ x 4) (1- y))
      (paint-at-point #\* (- x 4) (1- y))

      (paint-at-point #\* (- x 5)  y)
      (paint-at-point #\* (+ x 5)  y))))

(defun draw-plant (p)
  (let ((age (plant-age p)))
    (case age
      ((nil) nil)
      ((0) (draw-plant* p #\*))
      ((1) (plant-1 p))
      ((2) (plant-2 p))
      (otherwise (draw-plant* p #\Space)))))



(defun plant-too-old (p)
  (< 5 (plant-age p)))

(defun cull-plants ()
  (setf *plants* (delete-if #'plant-too-old *plants*)))

(defun age-plants ()
  ;; should be 600ish
  (if (eql (mod *world-age* 600 ) 0)
    (loop :for p 
          :in *plants*
          :do
              (cull-plants)
              (incf (plant-age p)))))

(defun rand-char ()
  (let ((num (random 1000))) 
    (cond
    ((evenp num ) #\, )
    (t #\.))))


(defun draw-map ()
  (multiple-value-bind (width height)
    (window-dimensions *standard-window*)
    (loop :for i 
          :from 0 
          :to (- width 1)
          :do (loop :for j 
                    :from 0 
                    :to (- height 2 )
                    :do (paint-at-point (rand-char) i j )))))

(defun draw-plants ()
    (progn
      (age-plants)
      (loop :for p 
        :in *plants*
        :do
         (draw-plant p))))


(defun draw-player ()
    (if (char-equal #\@ 
                    (char-at-cursor *standard-window* )) 
      (paint #\Space))
    (move-cursor *standard-window* 
                 (player-x *player*) 
                 (player-y *player*))
    (paint #\@))

(defun draw-hud ()
  (string-point (write-to-string *world-age*) 0 0 ))

(defun get-input ()
  (multiple-value-bind (width height)
    (window-dimensions *standard-window*)
    (let ((c (get-char *standard-window* :ignore-error t) )
          (x (player-x *player*))
          (y (player-y *player*))) 
    (case c
      ((nil) nil)
      ((#\k) (decf y))
      ((#\j) (incf y))
      ((#\h) (decf x))
      ((#\l) (incf x))
      ((#\Space) (plant))
      ((#\q) (quit)))
    (setf x (mod x width)
          y (mod y height)
          (player-x *player*) x
          (player-y *player*) y))))

(defun update-world ()
  (progn
    (incf *world-age*  )
    (if (eq 0 (mod *world-age* 61)) 
      (draw-map))
    (draw-hud)
    (draw-plants)
    (draw-player)
    (refresh-window *standard-window*)
    (sleep .01)))

(defun main ()
  (with-curses  ()
    (init)
    (loop :named main-loop
          :do 
          (update-world)
          (get-input)
          
          )))

(main)

