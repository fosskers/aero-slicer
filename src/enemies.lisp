(in-package :aero-fighter)

#+nil
(launch)

;; --- Blobs --- ;;

(defstruct (blob (:constructor blob))
  "An amorphous blob enemy that moves in a sin-wave."
  (texture nil :read-only t)
  (pos (raylib:make-vector2 :x 0.0 :y 0.0)))

(defmethod draw ((blob blob))
  (raylib:draw-texture-v (blob-texture blob) (blob-pos blob) raylib:+white+))

(defmethod move ((blob blob))
  "Gradual sinusoidal movement down the screen."
  (incf (raylib:vector2-y (blob-pos blob)) 1.0))

(defun maybe-spawn-blob (game)
  "Spawn a blob depending on the current frame."
  (when (= 0 (mod (game-frame game) (* 3 +frame-rate+)))
    (let ((blob (blob :texture (textures-blob (game-textures game)))))
      (setf (gethash (game-frame game) (game-blobs game)) blob))))

(defun move-all-blobs (game)
  "Move all blobs currently spawned into the `game'."
  (with-hash-table-iterator (iter (game-blobs game))
    (labels ((recurse ()
               (multiple-value-bind (entry? key blob) (iter)
                 (declare (ignore key))
                 (when entry?
                   (move blob)
                   (recurse)))))
      (recurse))))

(defun draw-all-blobs (game)
  "Move all blobs currently spawned into the `game'."
  (with-hash-table-iterator (iter (game-blobs game))
    (labels ((recurse ()
               (multiple-value-bind (entry? key blob) (iter)
                 (declare (ignore key))
                 (when entry?
                   (draw blob)
                   (recurse)))))
      (recurse))))

;; --- Tanks --- ;;

;; --- Shadow Fighters --- ;;
