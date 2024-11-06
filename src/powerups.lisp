;;; The mechanics of various fighter powerups.

(in-package :aero-fighter)

#+nil
(launch)

;; --- Bombs --- ;;

(defstruct bomb-ammo
  (animated nil :type animated)
  (pos      nil :type raylib:vector2)
  (bbox     nil :type raylib:rectangle)
  (spawn-fc 0   :type fixnum))

(defun bomb-ammo-expired? (bomb-ammo fc)
  "Has too much time passed since the bomb spawned? Should it despawn?"
  (> (- fc (bomb-ammo-spawn-fc bomb-ammo))
     +bomb-ammo-spawn-timeout+))

(defmethod pos ((bomb-ammo bomb-ammo))
  (bomb-ammo-pos bomb-ammo))

(defmethod bbox ((bomb-ammo bomb-ammo))
  (bomb-ammo-bbox bomb-ammo))

(defmethod draw ((bomb-ammo bomb-ammo) fc)
  (draw-animated (bomb-ammo-animated bomb-ammo)
                 (bomb-ammo-pos bomb-ammo)
                 fc))
