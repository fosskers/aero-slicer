;;; Everything involving the fighter and his beam weapon.
;;;
;;; The beam weapon comes in three strengths, represented by beam width. The
;;; initial beam is 4 pixels wide. The next is 8, and the final is 16, the full
;;; width of the ship.

(in-package :aero-fighter)

#+nil
(launch)

;; --- Types --- ;;

(defstruct beam
  "The beam shot from the ship."
  (animated  nil :type animated)
  (pos       nil :type raylib:vector2)
  (bbox      nil :type raylib:rectangle)
  (shooting? nil :type symbol)
  ;; The frame on which the shot was started.
  (shot-fc   0   :type fixnum)
  ;; The total duration, in frames, that the shot should be active for.
  (shot-dur  0   :type fixnum :read-only t))

(defstruct fighter
  "The player's fighter ship."
  (animated  nil :type animated)
  (pos       nil :type raylib:vector2)
  (bbox      nil :type raylib:rectangle)
  ;; Ok / Hit
  (status    'ok :type symbol)
  (status-fc 0   :type fixnum)
  (beam      nil :type beam))

(defun fighter (fighter-sprite beam-sprite)
  "A smart-constructor for `fighter'."
  (let* ((f-animated (make-animated :sprite fighter-sprite))
         (f-rect     (bounding-box f-animated))
         (b-animated (make-animated :sprite beam-sprite :default 'shooting :active 'shooting))
         (b-rect     (bounding-box b-animated)))
    (make-fighter :animated f-animated
                  :pos (raylib:make-vector2 :x +fighter-spawn-x+
                                            :y +fighter-spawn-y+)
                  :bbox (raylib:make-rectangle :x +fighter-spawn-x+
                                               :y +fighter-spawn-y+
                                               :width (raylib:rectangle-width f-rect)
                                               :height (raylib:rectangle-height f-rect))
                  :beam (make-beam :animated b-animated
                                   :pos (raylib:make-vector2 :x (+ 6 +fighter-spawn-x+)
                                                             :y (+ 15 +fighter-spawn-y+))
                                   :bbox (raylib:make-rectangle :x (+ 6 +fighter-spawn-x+)
                                                                :y (+ 15 +fighter-spawn-y+)
                                                                :width (raylib:rectangle-width b-rect)
                                                                :height (raylib:rectangle-height b-rect))))))

;; --- Status --- ;;

(defun kill-fighter (fighter fc)
  (setf (fighter-status fighter) 'hit)
  (setf (fighter-status-fc fighter) fc)
  (setf (animated-active (fighter-animated fighter)) 'damaged)
  ;; Move him back to the initial spawn position.
  (setf (raylib:vector2-x (fighter-pos fighter)) +fighter-spawn-x+)
  (setf (raylib:vector2-y (fighter-pos fighter)) +fighter-spawn-y+)
  (setf (raylib:rectangle-x (fighter-bbox fighter)) +fighter-spawn-x+)
  (setf (raylib:rectangle-y (fighter-bbox fighter)) +fighter-spawn-y+))

(defun update-fighter-status (fighter fc)
  "Alter the fighter's status depending on how much time has passed. This will then
be later reflected in animations."
  (cond ((and (eq 'hit (fighter-status fighter))
              (> (- fc (fighter-status-fc fighter)) +frame-rate+))
         (setf (fighter-status fighter) 'ok)
         (setf (fighter-status-fc fighter) fc)
         (setf (animated-active (fighter-animated fighter)) 'idle))))

;; --- Generics --- ;;

#+nil
(defmethod min-x ((fighter fighter))
  (raylib:vector2-x (fighter-pos fighter)))
#+nil
(defmethod max-x ((fighter fighter))
  (+ 15 (raylib:vector2-x (fighter-pos fighter))))
#+nil
(defmethod min-y ((fighter fighter))
  (raylib:vector2-y (fighter-pos fighter)))
#+nil
(defmethod max-y ((fighter fighter))
  (+ 15 (raylib:vector2-y (fighter-pos fighter))))

(defun draw-fighter (fighter fc)
  "Draw and animate the `fighter' based on the current frame count."
  (draw-animated (fighter-animated fighter) (fighter-pos fighter) fc))

(defmethod pos ((fighter fighter))
  (fighter-pos fighter))

(defmethod bbox ((fighter fighter))
  (fighter-bbox fighter))

(defmethod move ((fighter fighter))
  "Move the fighter depending on the current button presses."
  (let* ((pos  (fighter-pos fighter))
         (bbox (fighter-bbox fighter)))
    (when (raylib:is-key-down +key-right+)
      (let ((new (min +112.0 (+ +2.0 (raylib:vector2-x pos)))))
        (setf (raylib:vector2-x pos) new)
        (setf (raylib:rectangle-x bbox) new)))
    (when (raylib:is-key-down +key-left+)
      (let ((new (max -128.0 (+ -2.0 (raylib:vector2-x pos)))))
        (setf (raylib:vector2-x pos) new)
        (setf (raylib:rectangle-x bbox) new)))
    (when (raylib:is-key-down +key-down+)
      (let ((new (min +104.0 (+ +2.0 (raylib:vector2-y pos)))))
        (setf (raylib:vector2-y pos) new)
        (setf (raylib:rectangle-y bbox) new)))
    (when (raylib:is-key-down +key-up+)
      (let ((new (max -120.0 (+ -2.0 (raylib:vector2-y pos)))))
        (setf (raylib:vector2-y pos) new)
        (setf (raylib:rectangle-y bbox) new)))))
