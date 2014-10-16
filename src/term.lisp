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
(defstruct monster x y age)

(defparameter *player* (make-player  :x 20 :y 20 :hp 20))
(defparameter *running* t)
(defparameter *world-age* 0)
(defparameter *plants* '() )
(defparameter *monsters* '() )
(defparameter msg "this is the message" )

(defparameter red 1)
(defparameter blue 2)
(defparameter green 3)
(defparameter cyan 4)

(defmacro with-color (color &body body)
  `(progn
    (cl-charms/low-level:attron (cl-charms/low-level:color-pair ,color))  
    ,@body
  (cl-charms/low-level:attroff (cl-charms/low-level:color-pair ,color)))  )


(defmacro draw-plant* (char p)
  `(with-restored-cursor *standard-window*
    (write-char-at-point *standard-window*
                         ,char
                         (plant-x ,p)
                         (plant-y ,p))))

(defun init ()
  (disable-echoing)
  (cl-charms/low-level:curs-set 0)
  (enable-raw-input :interpret-control-characters t)
  (enable-non-blocking-mode *standard-window*)
  (cl-charms/low-level:start-color)
  (cl-charms/low-level:init-pair red cl-charms/low-level:color_red charms/ll:color_black )
  (cl-charms/low-level:init-pair blue cl-charms/low-level:color_blue charms/ll:color_black )
  (cl-charms/low-level:init-pair green cl-charms/low-level:color_green charms/ll:color_black )
  (cl-charms/low-level:init-pair cyan cl-charms/low-level:color_magenta charms/ll:color_black )
  )

(defun paint (ch)
  (with-restored-cursor *standard-window*
    (write-char-at-cursor *standard-window* ch)))

(defun paint-at-point (ch x y)
  (multiple-value-bind (width height)
    (window-dimensions *standard-window*)
    (with-restored-cursor *standard-window*
    (write-char-at-point *standard-window* ch 
                         (mod x (1- width)) 
                         (mod y (1- height))))) )

(defun string-point (str x y)
  (with-restored-cursor *standard-window*
     (write-string-at-point *standard-window* str x y)))

(defun quit ()
  (progn
    (setf *running* nil)
    (cl-charms/low-level:curs-set 2)
    (sb-sys:os-exit 0)))

(defun create-monster  ()
 (multiple-value-bind (width height)
   (window-dimensions *standard-window*)
   (let ((x (random (1- width)))
         (y (random (1- height))))
     (push (make-monster  :x x :y y :age 0) *monsters*))))

(defun draw-monsters ()
  (progn
    (update-monsters)
    (loop :for m
        :in *monsters*
        :do
        (paint-at-point #\Space (monster-x m) (monster-y m))
        (move-monster m)
        (with-color blue 
                    (paint-at-point #\m 
                                 (monster-x m)
                                 (monster-y m))))))


             

(defun monster-standing-on-plant (m)
  ;;810 is a green askterisk.
  ;;621 is a blue period
  (let ((i  
        (cl-charms/low-level:mvinch
                                            (monster-x m)
                                            (monster-y m)
                                            
                                            )))
    (case i
      ((-1) (setf msg "one"))
      (otherwise (setf msg (format t "~A ~A " (monster-x (car *monsters*)) (monster-y (car *monsters*)) )))
      )
  
  ))

(defun cull-monsters ()
        (loop :for m
              :in *monsters*
              :do
              (setf *monsters* (delete-if #'monster-standing-on-plant *monsters*))) )


(defun update-monsters ()
  (if (and (> 4 (length *monsters*))
       (eq (random 100) 3))
    (create-monster)))

(defun move-monster (m)
  (multiple-value-bind (width height)
    (window-dimensions *standard-window*)
   (let ((i (random 3)))
    (case i
      ((0)  (mod (incf (monster-x m)) (1- width)))
      ((1)  (mod (decf (monster-x m)) (1- width)))
      ((2)  (mod (incf (monster-y m)) (1- height)))
      ((3)  (mod (decf (monster-y m)) (1- height)))
      ))))

(defun plant ()
  (multiple-value-bind (x y)
    (charms:cursor-position *standard-window*)
     (push (make-plant :x x :y y :age 0) *plants*)))



(defun plant-1 (ch p)
   (let ((x (plant-x p))
        (y (plant-y p)))
     (progn
       (paint-at-point ch (1- x) y)
       (paint-at-point ch (- x 2) y) 
       (paint-at-point ch (1+ x) y) 
       (paint-at-point ch (+ x 2) y) 
       (paint-at-point ch  x (1- y))
       (paint-at-point ch  x (1+ y)))))

(defun plant-2 (ch p)
  (let ((x (plant-x p))
        (y (plant-y p)))
    (progn
      (paint-at-point ch (1- x) (- y 2))
      (paint-at-point ch (1+ x) (- y 2))
      (paint-at-point ch (1- x) (+ y 2))
      (paint-at-point ch (1+ x) (+ y 2))

      (paint-at-point ch (+ x 4) (1+ y))
      (paint-at-point ch (- x 4) (1+ y))
      (paint-at-point ch (+ x 4) (1- y))
      (paint-at-point ch (- x 4) (1- y))

      (paint-at-point ch (- x 5)  y)
      (paint-at-point ch (+ x 5)  y))))

(defun draw-plant (p)
  (with-color green
    (let ((age (plant-age p)))
    (case age
      ((nil) nil)
      ((0) (draw-plant* #\* p))
      ((1) (draw-plant* #\Space p)
           (plant-1 #\- p))
      ((2) (plant-1 #\Space p) 
           (plant-2 #\* p))
      (otherwise (plant-2  #\Space p))))))



(defun plant-too-old (p)
  (< 5 (plant-age p)))

(defun cull-plants ()
  (setf *plants* (delete-if #'plant-too-old *plants*)))

(defun age-plants ()
  ;; should be 600ish
  (if (eql (mod *world-age* 60 ) 0)
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
                    :do (with-color blue (paint-at-point #\. i j ))))))

(defun draw-plants ()
  (age-plants)
  (loop :for p 
        :in *plants*
        :do
        (draw-plant p)))


(defun draw-player ()
  (paint #\Space)
  (move-cursor *standard-window* 
               (player-x *player*) 
               (player-y *player*))
  (with-color red (paint #\@)))

(defun draw-hud ()
  (with-color green
    (progn
    (string-point (format nil "* Time: ~d X: ~A Y: ~A Plants Left: ~A Monsters: ~A Debug: ~A "
                          *world-age*
                          (player-x *player*)
                          (player-y *player*)
                          (- 11 (length *plants*))
                          (length *monsters*)
                          msg
                          ) 0 0 ))))

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
      ;((#\t) (monster-standing-on-plant ))
      ((#\Space) (unless (< 10 (length *plants*)) (plant)))
      ((#\q) (quit)))
    (setf x (mod x (1- width) )
          y (mod y (1- height))
          (player-x *player*) x
          (player-y *player*) y))))

(defun update-world ()
  (progn
    (incf *world-age*  )
    ;(if (eq 0 (mod *world-age* 61)) 
    ;  (draw-map))
    (draw-hud)
    (draw-monsters)
    (draw-plants)
    ;(cull-monsters ) 
    (draw-player)
    (if (> 0 (length *monsters*)) (setf msg *monsters*))
    (refresh-window *standard-window*)
    (sleep .01)))

(defun main ()
  (with-curses  ()
    (unwind-protect
      (init)
      (loop :named main-loop
            :do 
            (get-input)
            (update-world)
          
          )
      (quit)) ))

(main)

