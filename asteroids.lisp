;;;; ASTeroids

; vim: ts=2 sts=2 sw=2 et ai
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")

(defpackage :asteroids
  (:use :cl :sdl)
  (:export main))

(in-package :asteroids)

(defparameter *map-width* 640)
(defparameter *map-height* 480)

(defparameter *window* nil)
(defparameter *window-width* 640)

(defparameter *deceleration* 0.99)

(defparameter *ticks* 0)

(defparameter *powerup-max-age* 9)
(defparameter *explosion-max-radius* 0.5)

(defun vector-sum (a b)
  (mapcar #'+ a b))

(defun vector-scale (v factor)
  (mapcar #'* v (list factor factor)))

(defun vector-subtract (a b)
  (mapcar #'- a b))

;;; distance between point a and point b
;;; parameters must be lists of 2 numbers (x y)
(defun my-distance (a b)
  (sqrt (apply #'+
               (mapcar (lambda (x)
                         (expt x 2))
                       (vector-subtract a b)))))

(defun square-from-midpoint (point radius)
  (rectangle-from-midpoint-* (x point)
                             (y point)
                             (* radius 2)
                             (* radius 2)))

(defun deg->rad (degs)
  (* degs (/ pi 180)))

(defun rad->deg (rads)
  (* rads (/ 180 pi)))

(defun radial-point-from (p radius angle)
  (point :x (+ (* radius (sin (deg->rad angle))) (x p))
         :y (+ (* radius (cos (deg->rad angle))) (y p))))

(defun get-ticks ()
  (let ((ticks *ticks*))
    (setf *ticks* (sdl-get-ticks))
    (- *ticks* ticks)))

;;; represents an object on the game map
(defclass mob ()
  ((pos :initarg :pos :initform '(0.5 0.5) :accessor pos)
   (radius :initarg :radius :accessor radius)
   (velocity :initarg :velocity :initform '(0 0) :accessor velocity)))

(defclass asteroid (mob)
  ((size :initarg :size :initform 'big :reader size)
   (radii :initform nil :accessor radii)
   (rotation :initform (* (- (random 1.0) 0.5) 5) :reader rotation)
   (facing :initform 0 :accessor facing)
   (pos :initform `(,(random 1.0) ,(random 1.0)))))

(defclass bullet (mob)
  ((radius :initform 0.005)
   (ship :initarg :ship :accessor ship)))

(defclass explosion (mob)
  ((radius :initform 0)))

(defclass powerup (mob)
  ((radius :initform 0.03)
   (age :initform 0 :accessor age)))

(defclass bullet-powerup (powerup) ())

(defclass freeze-powerup (powerup) ())

(defclass shield-powerup (powerup) ())

(defclass ship (mob)
  ((timers :initform (make-hash-table) :accessor timers)
   (acceleration :initform '(0 0) :accessor acceleration)
   (facing :initform 0 :accessor facing)
   (radius :initform 0.04)))

(defclass timer ()
  ((remaining :initarg :seconds :initform 0 :accessor remaining)))

(defclass world ()
  ((mobs :initform nil :accessor mobs)
   (ship :initform nil :accessor ship)
   (bullet :initform nil :accessor bullet)
   (timers :initform (make-hash-table) :accessor timers)
   (level :initform 0 :accessor level)
   (num-asteroids :initform 0 :accessor num-asteroids)
   (score :initform 0 :accessor score)
   (max-level :initform 0 :accessor max-level)
   (high-score :initform 0 :accessor high-score)
   (lives :initform 0 :accessor lives)))

(defmethod collide ((mob mob) (other mob) (world world)) t)

(defmethod map-coords ((mob mob))
  (multiple-value-bind (x y) (values-list (pos mob))
    (point :x (round (* x *map-width*))
           :y (round (* y *map-height*)))))

(defmethod map-radius ((mob mob))
  (round (* (radius mob) *map-width*)))

(defmethod update ((mob mob) time-delta (world world))
  (setf (pos mob)
        (mapcar (lambda (x) (mod x 1))
                (vector-sum (pos mob)
                            (vector-scale (velocity mob)
                                          time-delta)))))

(defmethod intersects-p ((mob mob) (other mob))
  (< (my-distance (pos mob) (pos other))
     (+ (radius mob) (radius other))))

(defmethod render ((mob mob))
  (values))

(defmethod initialize-instance :after ((asteroid asteroid) &key)
  (let ((radius (cdr (assoc (size asteroid)
                            '((big . 0.1) (medium . 0.075) (small . 0.05)))))
        (spd (cdr (assoc (size asteroid)
                         '((big . 0.1) (medium . 0.15) (small . 0.25))))))
    (setf (radius asteroid) radius)
    (setf (radii asteroid)
          (loop for i from 0 below 20
            collect (round (* (+ 0.9 (random 0.2))
                              (map-radius asteroid)))))
    (setf (velocity asteroid)
          `(,(- (random (* 2 spd)) spd) ,(- (random (* 2 spd)) spd)))))

(defun random-powerup (&key pos)
  (make-instance (case (random 3)
                   (0 'bullet-powerup)
                   (1 'freeze-powerup)
                   (2 'shield-powerup))
                 :pos pos))

(defmethod break-down ((asteroid asteroid) (world world))
  (with-slots ((pos pos) (size size)) asteroid
    (if (eq size 'small)
      ;; gradually reduce the probability of powerups appearing
      (if (< (random 100) (/ 100 (+ 4 (* (level world) 0.3))))
          `(,(random-powerup :pos pos))
          nil)
      (let ((smaller (cond
                     ((eq size 'big) 'medium)
                     ((eq size 'medium) 'small))))
        `(,(make-instance 'asteroid :pos pos :size smaller)
          ,(make-instance 'asteroid :pos pos :size smaller))))))

(defmethod done ((timer timer))
  (<= (ceiling (remaining timer)) 0))

(defmethod frozen-p ((world world))
  (let ((timer (gethash 'freeze (timers world) nil)))
    (and timer
         (not (done timer)))))

(defmethod update ((asteroid asteroid) time-delta (world world))
  (when (not (frozen-p world))
    (incf (facing asteroid) (rotation asteroid))
    (call-next-method)))

(defmethod render ((asteroid asteroid))
  (draw-polygon (loop for i from 0
                      for r in (radii asteroid)
                  collect (radial-point-from (map-coords asteroid) r
                                             (+ (facing asteroid)
                                                (* i 18))))
                :color *white*))

(defmethod remove-from-world ((world world) (mob mob))
  (setf (mobs world) (remove mob (mobs world))))

(defmethod remove-from-world :after ((world world) (asteroid asteroid))
  (decf (num-asteroids world)))

(defmethod remove-from-world :after ((world world) (ship ship))
  (setf (ship world) nil))

(defmethod update ((powerup powerup) time-delta (world world))
  (when (> (ceiling (incf (age powerup) time-delta))
           *powerup-max-age*)
    (remove-from-world world powerup)))

(defmethod add-score ((world world) score)
  (when (numberp score)
    (setf (high-score world)
          (max (incf (score world) score)
               (high-score world)))))

(defmethod add-score ((world world) (powerup powerup))
  (add-score world (* (level world) 10)))

(defmethod add-score ((world world) (asteroid asteroid))
  (add-score world (cdr (assoc (size asteroid)
                               '((big . 1) (medium . 2) (small . 5))))))

(defmethod collide :before ((ship ship) (powerup powerup) (world world))
  (remove-from-world world powerup)
  (add-score world powerup))

(defmethod powerup-active-p ((ship ship) powerup)
  (let ((timer (gethash powerup (timers ship) nil)))
    (and timer
         (not (done timer)))))

(defmethod add-seconds ((timer timer) seconds)
  (incf (remaining timer) seconds))

(defmethod add-shield ((ship ship) &key (seconds 0))
  (if (powerup-active-p ship 'shield)
    (add-seconds (gethash 'shield (timers ship)) seconds)
    (setf (gethash 'shield (timers ship))
          (make-instance 'timer :seconds seconds))))

(defmethod collide :before ((ship ship) (powerup shield-powerup) (world world))
  (add-shield ship :seconds 6))

(defmethod render ((powerup shield-powerup))
  (let ((coords (map-coords powerup))
        (radius (map-radius powerup)))
    (draw-circle coords radius
                 :color *green*)
    (draw-polygon `(,(radial-point-from coords (round (* radius 0.8)) 40)
                    ,(radial-point-from coords (round (* radius 0.8)) 0)
                    ,(radial-point-from coords (round (* radius 0.8)) -40)
                    ,(radial-point-from coords (round (* radius 0.8)) -135)
                    ,(radial-point-from coords (round (* radius 0.8)) 135))
                  :color *white*)))

(defmethod add-super-bullets ((ship ship) &key (seconds 0))
  (if (powerup-active-p ship 'super-bullets)
    (add-seconds (gethash 'super-bullets (timers ship)) seconds)
    (setf (gethash 'super-bullets (timers ship))
          (make-instance 'timer :seconds seconds))))

(defmethod collide :before ((ship ship) (powerup bullet-powerup) (world world))
  (add-super-bullets ship :seconds 6))

(defmethod render ((powerup bullet-powerup))
  (let ((coords (map-coords powerup))
        (radius (map-radius powerup)))
    (draw-circle coords radius
                 :color *magenta*)
    (draw-circle coords (round (* radius 0.3))
                 :color *white*)))

(defmethod add-freeze ((world world) &key (seconds 0))
  (if (frozen-p world)
    (add-seconds (gethash 'freeze (timers world)) seconds)
    (setf (gethash 'freeze (timers world))
          (make-instance 'timer :seconds seconds))))

(defmethod collide :before ((ship ship) (powerup freeze-powerup) (world world))
  (add-freeze world :seconds 6))

(defmethod render ((powerup freeze-powerup))
  (let ((coords (map-coords powerup))
        (radius (map-radius powerup)))
    (draw-circle coords radius
                 :color *cyan*)
    (draw-polygon (loop for i from 0 to 11
                    collect (radial-point-from coords
                                               (round (* radius (if (= (mod i 2) 0)
                                                                       0.7
                                                                       0.2)))
                                               (* i 30)))
                  :color *white*)))

(defmethod add-to-world ((world world) (mob mob))
  (setf (mobs world) (cons mob (mobs world)))
  (values mob))

(defmethod collide :before ((ship ship) (asteroid asteroid) (world world))
  (unless (powerup-active-p ship 'shield)
    (remove-from-world world ship)
    (add-to-world world (make-instance 'explosion :pos (pos ship)))
    (decf (lives world))))

(defmethod in-world-p ((world world) (mob mob))
  (find mob (mobs world)))

(defmethod ship-moved ((world world) (ship ship))
  (dolist (mob (mobs world))
    (when (and (not (eq ship mob))
               (intersects-p ship mob))
      (collide ship mob world))
    ;; if a collision destroyed the ship, stop checking for collisions
    (when (not (in-world-p world ship))
      (return ship))))

(defmethod update-timer ((timer timer) time-delta)
  (unless (done timer)
    (decf (remaining timer) time-delta)))

(defmethod update :around ((ship ship) time-delta (world world))
  (setf (velocity ship)
        (vector-scale (vector-sum (velocity ship)
                                  (acceleration ship))
                      *deceleration*))
  (maphash (lambda (name timer)
             (declare (ignore name))
             (update-timer timer time-delta))
           (timers ship))
  (call-next-method)
  (ship-moved world ship))

(defmethod thrust-at ((ship ship) coords)
  (setf (acceleration ship)
        (vector-sum (acceleration ship)
                    (vector-scale (vector-subtract coords (pos ship))
                                  0.03))))

(defmethod stop-thrust ((ship ship))
  (setf (acceleration ship) '(0 0)))

(defmethod shoot-at ((ship ship) coords (world world))
  (let ((bullet (make-instance 'bullet :pos (pos ship)
                                       :ship ship)))
    (setf (velocity bullet)
          (vector-scale (vector-subtract coords (pos bullet))
                        3))
    (add-to-world world bullet)))

(defmethod render ((ship ship))
  (let* ((coords (map-coords ship))
         (radius (map-radius ship))
         (facing (facing ship))
         (nose (radial-point-from coords radius facing))
         (left (radial-point-from coords radius (- facing 130)))
         (right (radial-point-from coords radius (+ facing 130)))
         (tail (radial-point-from coords (round (* radius 0.5)) (+ facing 180))))
    (draw-polygon (list nose left tail right)
                  :color *white*)
    (when (powerup-active-p ship 'shield)
          (draw-circle coords
                      (round (+ radius (random 3)))
                      :color *green*))))

(defmethod super-p ((bullet bullet))
  (powerup-active-p (ship bullet) 'super-bullets))

(defmethod collide :before ((bullet bullet) (asteroid asteroid) (world world))
  (remove-from-world world asteroid)
  (when (not (super-p bullet))
    (remove-from-world world bullet))
  (mapcar (lambda (mob)
            (add-to-world world mob))
          (break-down asteroid world))
  (add-to-world world (make-instance 'explosion :pos (pos asteroid)))
  (add-score world asteroid))

(defmethod render ((bullet bullet))
  (let ((coords (map-coords bullet))
        (radius (map-radius bullet)))
    (draw-circle coords radius
                 :color *red*)
    (when (super-p bullet)
          (draw-circle coords (+ (random 3))
                       :color *magenta*))))

(defmethod bullet-moved ((world world) (bullet bullet))
  (dolist (mob (mobs world))
    (when (and (not (eq bullet mob))
               (intersects-p bullet mob))
      (collide bullet mob world))
    (when (not (in-world-p world bullet))
      (return bullet))))

(defmethod update ((bullet bullet) time-delta (world world))
  (setf (pos bullet)
        (vector-sum (pos bullet)
                    (vector-scale (velocity bullet)
                                  time-delta)))
  (multiple-value-bind (x y) (values-list (pos bullet))
    (when (or (not (< 0 x *map-width*))
              (not (< 0 y *map-height*)))
      (remove-from-world world bullet)))
  (bullet-moved world bullet))

(defmethod render ((explosion explosion))
  (let ((coords (map-coords explosion))
        (radius (map-radius explosion)))
    (draw-circle coords radius :color *red*)
    (draw-circle coords
                 (+ radius (random 3))
                 :color *red*)))

(defmethod update ((explosion explosion) time-delta (world world))
  (when (> (incf (radius explosion) time-delta)
           *explosion-max-radius*)
    (remove-from-world world explosion)))

(defmethod start-next-level ((world world))
  (with-accessors ((level level)
                   (max-level max-level)
                   (mobs mobs)
                   (timers timers)
                   (ship ship))
                   world
    (incf level)
    (setf max-level (max max-level level))
    (setf mobs nil)
    (setf timers (make-hash-table))
    (dotimes (i level)
      (add-to-world world (make-instance 'asteroid)))
    (add-to-world world (or ship (make-instance 'ship)))
    (add-shield (ship world) :seconds 6)))

(defmethod level-cleared-p ((world world))
  (< (num-asteroids world) 1))

(defmethod after ((world world) timer-name &key (seconds 0) do)
  (multiple-value-bind (timer exists) (gethash timer-name (timers world))
    (if exists
      (when (done timer)
        (remhash timer-name (timers world))
        (when (functionp do)
          (funcall do)))
      (setf (gethash timer-name (timers world))
            (make-instance 'timer :seconds seconds)))))

(defmethod update-world ((world world) time-delta)
  (maphash (lambda (name timer)
             (declare (ignore name))
             (update-timer timer time-delta))
           (timers world))
  (dolist (mob (mobs world))
    (update mob time-delta world))
  ;; start next level 3 seconds after clearing
  (when (level-cleared-p world)
    (after world
           'cleared
           :seconds 3
           :do (lambda ()
                 (incf (lives world))
                 (start-next-level world))))
  ;; restart level 3 seconds after death - game over if no more lives
  (unless (ship world)
    (after world
           'death
           :seconds 3
           :do (lambda ()
                 (if (< (lives world) 1)
                   (setf (level world) 0) ; game over
                   (let ((ship (make-instance 'ship)))
                     (add-to-world world ship)
                     (add-shield ship :seconds 6)))))))

(defmethod add-to-world :after ((world world) (asteroid asteroid))
  (incf (num-asteroids world)))

(defmethod add-to-world :after ((world world) (ship ship))
  (setf (ship world) ship))

(defmethod render-world ((world world) paused)
  (clear-display *black*)
  ;; hud
  (sdl-gfx:draw-string-solid-* (format nil "Level ~d" (level world))
                               10 10
                               :color *green*)
  (sdl-gfx:draw-string-solid-* (format nil "Lives ~d" (lives world))
                               10 (- *map-height* 28)
                               :color *green*)
  (sdl-gfx:draw-string-solid-* (format nil "Score ~d" (score world))
                               (- *map-width* 127) (- *map-height* 28)
                               :color *green*)
  (sdl-gfx:draw-string-solid-* (format nil
                                       "~a [Q]uit"
                                       (if (= (level world) 0)
                                           "[P]lay"
                                           "[P]ause"))
                               (- *map-width* 127) 10
                               :color *green*)
  (if (= (level world) 0)
    ;; title screen
    (progn
      (sdl-gfx:draw-string-solid-* "ASTeroids"
                                   (round (* 1/2 (- *map-width* 81)))
                                   (round (* 1/4 (- *map-height* 18)))
                                   :color *green*)
      (sdl-gfx:draw-string-solid-* (format nil
                                           "High score: ~d"
                                           (high-score world))
                                   (round (* 1/2 (- *map-width* 171)))
                                   (round (* 1/2 (- *map-height* 18)))
                                   :color *green*)
      (sdl-gfx:draw-string-solid-* (format nil "Max level: ~d" (max-level world))
                                   (round (* 1/2 (- *map-width* 135)))
                                   (round (* 3/4 (- *map-height* 18)))
                                   :color *green*))
    (progn
      ;; game world
      (set-clip-rect (rectangle :x 0 :y 0 :w *map-width* :h *map-height*)
                     :surface *default-display*)
      (dolist (mob (mobs world))
        (render mob))
      (set-clip-rect nil :surface *default-display*)
      ;; pause text
      (when paused
        (sdl-gfx:draw-string-solid-* "PAUSED"
                                     (round (* 1/2 (- *map-width* 54)))
                                     (round (* 1/2 (- *map-height* 18)))
                                     :color *green*)))))

(defun calc-angle (a b)
  (multiple-value-bind (x y) (values-list (vector-subtract b a))
    (rad->deg (atan x y))))

(defun main ()
  (with-init ()
    (setf *window*
          (window 640 480
                  :title-caption "ASTeroids"
                  :icon-caption "ASTeroids"))
    (sdl-gfx:initialise-default-font sdl-gfx:*font-9x18*)
    (setf (frame-rate) 60)
    (clear-display *black*)
    (let ((world (make-instance 'world))
          (paused nil))
      (with-events ()
        (:quit-event () t)
        (:mouse-motion-event (:x x :y y)
          (when (ship world)
            (setf (facing (ship world))
                  (calc-angle (pos (ship world))
                              `(,(/ x *map-width*) ,(/ y *map-height*))))))
        (:mouse-button-down-event (:x x :y y)
          (when (and (> (level world) 0)
                     (ship world)
                     (not paused))
            (shoot-at (ship world)
                      `(,(/ x *map-width*) ,(/ y *map-height*))
                      world)
            (thrust-at (ship world)
                       `(,(/ x *map-width*)
                         ,(/ y *map-height*)))))
        (:mouse-button-up-event ()
          (when (and (> (level world) 0)
                     (ship world))
            (stop-thrust (ship world))))
        (:key-up-event (:key key)
          (case key
            (:sdl-key-escape (push-quit-event))
            (:sdl-key-q (setf (level world) 0))
            (:sdl-key-p (if (= (level world) 0)
                          (progn
                            (setf (score world) 0)
                            (setf (lives world) 1)
                            (setf *ticks* (sdl-get-ticks))
                            (start-next-level world))
                          (setf paused (not paused))))))
        (:idle ()
          (when (and (> (level world) 0)
                     (not paused))
            (update-world world (* (get-ticks) 0.001)))
          (render-world world paused)
          (update-display))))))
