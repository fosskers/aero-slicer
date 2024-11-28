;;; The mechanics of various fighter powerups.

(in-package :aero-fighter)

#+nil
(launch)

;; --- Shield --- ;;

(defstruct shield
  "A protective shield powerup."
  (animated nil :type animated)
  (pos      nil :type raylib:vector2)
  (bbox     nil :type raylib:rectangle)
  (spawn-fc 0   :type fixnum))

(defun @shield (sprite fc)
  "A smart-constructor for `shield'."
  (let* ((pos      (random-position))
         (animated (make-animated :sprite sprite))
         (rect     (bounding-box animated)))
    (make-shield :animated animated
                 :pos pos
                 :spawn-fc fc
                 :bbox (raylib:make-rectangle :x (raylib:vector2-x pos)
                                              :y (raylib:vector2-y pos)
                                              :width (raylib:rectangle-width rect)
                                              :height (raylib:rectangle-height rect)))))

(defun maybe-spawn-shield! (game)
  "Spawn a `shield' powerup depending on the current score. However, it shouldn't
spawn if the fighter is already shielded."
  (when (and (->> game game-fighter fighter-shielded? not)
             (>= (game-score game) (game-shield-threshold game)))
    (incf (game-shield-threshold game) +shield-spawn-interval+)
    (setf (gethash (game-frame game) (game-powerups game))
          (@shield (->> game game-sprites sprites-shield) (game-frame game)))))

(defmethod pos ((shield shield))
  (shield-pos shield))

(defmethod bbox ((shield shield))
  (shield-bbox shield))

(defmethod draw ((shield shield) fc)
  (draw-animated (shield-animated shield)
                 (shield-pos shield)
                 fc))

(defmethod tick! ((shield shield) fc)
  "Start to despawn the `shield' if too much time has passed."
  (let ((animated (shield-animated shield)))
    (when (and (eq 'idle (animated-active animated))
               (>= (- fc (shield-spawn-fc shield))
                   +powerup-newness-timeout+))
      (set-animation! animated 'flashing fc))))

(defmethod expired? ((shield shield) fc)
  (>= (- fc (shield-spawn-fc shield))
      +powerup-spawn-timeout+))

;; --- Wide Laser --- ;;

(defstruct wide
  "A wide laser powerup."
  (animated nil :type animated)
  (pos      nil :type raylib:vector2)
  (bbox     nil :type raylib:rectangle))

(defun @wide (sprite)
  "A smart-consturctor for `wide'."
  (let* ((pos      (random-position))
         (animated (make-animated :sprite sprite))
         (rect     (bounding-box animated)))
    (make-wide :animated animated
               :pos pos
               :bbox (raylib:make-rectangle :x (raylib:vector2-x pos)
                                            :y (raylib:vector2-y pos)
                                            :width (raylib:rectangle-width rect)
                                            :height (raylib:rectangle-height rect)))))

(defun maybe-spawn-wide! (game)
  "Spawn a `wide' laser powerup depending on the current score. However, it
shouldn't spawn if the fighter is already at max beam width."
  (when (>= (game-score game) (game-widener-threshold game))
    (incf (game-widener-threshold game) +beam-widening-interval+)
    (when (not (eq (->> game game-fighter fighter-beam beam-animated animated-sprite)
                   (sprites-beam-18 (game-sprites game))))
      (let ((wide (@wide (sprites-wide (game-sprites game)))))
        (setf (gethash (game-frame game) (game-powerups game)) wide)))))

(defmethod pos ((wide wide))
  (wide-pos wide))

(defmethod bbox ((wide wide))
  (wide-bbox wide))

(defmethod draw ((wide wide) fc)
  (draw-animated (wide-animated wide)
                 (wide-pos wide)
                 fc))

(defmethod tick! ((wide wide) fc)
  nil)

(defmethod expired? ((wide wide) fc)
  "The beam widener can never expire."
  nil)

;; --- Bombs --- ;;

(defstruct ammo
  "Extra bomb ammunition."
  (animated nil :type animated)
  (pos      nil :type raylib:vector2)
  (bbox     nil :type raylib:rectangle)
  (spawn-fc 0   :type fixnum))

(defun @ammo (sprite fc)
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

(defmethod pos ((ammo ammo))
  (ammo-pos ammo))

(defmethod bbox ((ammo ammo))
  (ammo-bbox ammo))

(defmethod draw ((ammo ammo) fc)
  (draw-animated (ammo-animated ammo)
                 (ammo-pos ammo)
                 fc))

(defmethod tick! ((ammo ammo) fc)
  "Start to despawn the `ammo' if too much time has passed."
  (let ((animated (ammo-animated ammo)))
    ;; FIXME: 2024-11-17 Is it a hack to use the animation's state to model the
    ;; state of the parent object?
    ;;
    ;; Yes:
    ;; - Seems brittle to define _program_ states in an external file (the sprite).
    ;; - What if you need an entity state that associated with no animation?
    ;;
    ;; No:
    ;; - It means the number of states of the entity and sprite stay synced.
    ;; - It means the active state of the entity can't drift from the animation.
    ;; - Reduces a bit of boilerplate..
    (when (and (eq 'idle (animated-active animated))
               (>= (- fc (ammo-spawn-fc ammo))
                   +powerup-newness-timeout+))
      (set-animation! animated 'flashing fc))))

(defmethod expired? ((ammo ammo) fc)
  (>= (- fc (ammo-spawn-fc ammo))
      +powerup-spawn-timeout+))
