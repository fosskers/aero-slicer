;;; The mechanics of various fighter powerups.

(in-package :aero-fighter)

#+nil
(launch)

;; --- Bombs --- ;;

(defstruct ammo
  "Extra bomb ammunition."
  (animated nil :type animated)
  (pos      nil :type raylib:vector2)
  (bbox     nil :type raylib:rectangle)
  (spawn-fc 0   :type fixnum))

(defun ammo (sprite fc)
  "A smart-consturctor for `ammo'."
  (let* ((pos      (random-position))
         (animated (make-animated :sprite sprite))
         (rect     (bounding-box animated)))
    (make-ammo :animated animated
               :pos pos
               :spawn-fc fc
               :bbox (raylib:make-rectangle :x (raylib:vector2-x pos)
                                            :y (raylib:vector2-y pos)
                                            :width (raylib:rectangle-width rect)
                                            :height (raylib:rectangle-height rect)))))

(defun maybe-spawn-ammo! (game)
  "Spawn some bomb ammo depending on the current frame."
  (let ((fighter (game-fighter game))
        (fc (game-frame game)))
    ;; TODO: 2024-11-08 Make more robust. Use randomness, etc.
    (when (and (< (fighter-bombs fighter) +bomb-max-capacity+)
               (zerop (mod fc (* 20 +frame-rate+))))
      (let ((ammo (ammo (sprites-bomb (game-sprites game)) fc)))
        (setf (gethash fc (game-powerups game)) ammo)))))

(defmethod pos ((ammo ammo))
  (ammo-pos ammo))

(defmethod bbox ((ammo ammo))
  (ammo-bbox ammo))

(defmethod draw ((ammo ammo) fc)
  (draw-animated (ammo-animated ammo)
                 (ammo-pos ammo)
                 fc))

(defmethod expired? ((ammo ammo) fc)
  (> (- fc (ammo-spawn-fc ammo))
     +bomb-ammo-spawn-timeout+))
