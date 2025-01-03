;;; The mechanics of various fighter powerups.

(in-package :aero-fighter)

#+nil
(launch)

(defun maybe-spawn-powerup! (game)
  "Perhaps spawn a strong powerup."
  (when (>= (game-score game) (game-powerup-threshold game))
    (incf (game-powerup-threshold game) +powerup-spawn-interval+)
    (let ((n (random 5))
          (fighter (game-fighter game)))
      (cond ((and (not (fighter-god-mode? fighter))
                  (max-beam? (game-sprites game)
                             (->> fighter fighter-beam beam-animated animated-sprite)))
             (setf (gethash (game-frame game) (game-powerups game))
                   (@god-mode (->> game game-sprites sprites-god-mode))))
            ((and (not (fighter-shielded? fighter))
                  (= 0 n))
             (setf (gethash (game-frame game) (game-powerups game))
                   (@shield (->> game game-sprites sprites-shield) (game-frame game))))
            ((not (fighter-god-mode? fighter))
             (setf (gethash (game-frame game) (game-powerups game))
                   (@wide (->> game game-sprites sprites-wide))))))))

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
    (when (and (eq :idle (animated-active animated))
               (>= (- fc (shield-spawn-fc shield))
                   +powerup-newness-timeout+))
      (set-animation! animated :flashing fc))))

(defmethod expired? ((shield shield) fc)
  (>= (- fc (shield-spawn-fc shield))
      +powerup-spawn-timeout+))

;; --- God Mode --- ;;

(defstruct god-mode
  "Fun mode!"
  (animated nil :type animated)
  (pos      nil :type raylib:vector2)
  (bbox     nil :type raylib:rectangle))

(defun @god-mode (sprite)
  "A smart-consturctor for `god-mode'."
  (let* ((animated (make-animated :sprite sprite))
         (rect     (bounding-box animated))
         (pos      (raylib:make-vector2 :x (- 0.0 (round (/ (raylib:rectangle-width rect) 2)))
                                        :y 0.0)))
    (make-god-mode :animated animated
                   :pos pos
                   :bbox (raylib:make-rectangle :x (raylib:vector2-x pos)
                                                :y (raylib:vector2-y pos)
                                                :width (raylib:rectangle-width rect)
                                                :height (raylib:rectangle-height rect)))))

(defmethod pos ((god-mode god-mode))
  (god-mode-pos god-mode))

(defmethod bbox ((god-mode god-mode))
  (god-mode-bbox god-mode))

(defmethod draw ((god-mode god-mode) fc)
  (draw-animated (god-mode-animated god-mode)
                 (god-mode-pos god-mode)
                 fc))

(defmethod tick! ((god-mode god-mode) fc)
  nil)

(defmethod expired? ((god-mode god-mode) fc)
  nil)

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
    (when (and (eq :idle (animated-active animated))
               (>= (- fc (ammo-spawn-fc ammo))
                   +powerup-newness-timeout+))
      (set-animation! animated :flashing fc))))

(defmethod expired? ((ammo ammo) fc)
  (>= (- fc (ammo-spawn-fc ammo))
      +powerup-spawn-timeout+))
