(in-package :aero-fighter)

#+nil
(launch)

;; --- Types --- ;;

(defstruct fighter
  "The player's fighter ship."
  (animated  nil :type animated)
  (pos       nil :type raylib:vector2)
  (bbox      nil :type raylib:rectangle)
  (status    'ok :type symbol)
  (status-fc 0   :type fixnum))

(defun fighter (sprite)
  "A smart-constructor for `fighter'."
  (let* ((animated (animated :sprite sprite))
         (rect     (bounding-box animated)))
    (make-fighter :animated animated
                  :pos (raylib:make-vector2 :x 0.0 :y 0.0)
                  :bbox (raylib:make-rectangle :x 0.0
                                               :y 0.0
                                               :width (raylib:rectangle-width rect)
                                               :height (raylib:rectangle-height rect)))))

;; --- Status --- ;;

(defun damage-fighter (fighter fc)
  (setf (fighter-status fighter) 'damaged)
  (setf (fighter-status-fc fighter) fc)
  (setf (animated-active (fighter-animated fighter)) 'damaged))

(defun update-fighter-status (fighter fc)
  "Alter the fighter's status depending on how much time has passed. This will then
be later reflected in animations."
  (cond ((and (eq 'damaged (fighter-status fighter))
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
