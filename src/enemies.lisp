(in-package :aero-fighter)

#+nil
(launch)

;; --- General --- ;;

(defun offscreen-vert? (guy)
  "Is the dude off the bottom end of the screen?"
  (let ((y (raylib:vector2-y (pos guy))))
    (> y +world-max-y+)))

;; --- Blobs --- ;;

(defstruct blob
  "An amorphous blob enemy that moves in a sin-wave."
  (texture nil :read-only t)
  (pos nil))
;; (pos (raylib:make-vector2 :x 0.0 :y 0.0)))

(defun blob (texture)
  "Spawn a `blob' somewhere off the top of the screen."
  (make-blob :texture texture
             :pos (raylib:make-vector2 :y (float (- +world-min-y+ 16))
                                       :x (float (- (random +world-pixels-x+)
                                                    +world-max-x+)))))

(defmethod pos ((blob blob))
  (blob-pos blob))

(defmethod draw ((blob blob))
  (raylib:draw-texture-v (blob-texture blob) (blob-pos blob) raylib:+white+))

(defmethod move ((blob blob))
  "Gradual sinusoidal movement down the screen."
  (incf (raylib:vector2-y (blob-pos blob)) 1.0))

(defun maybe-spawn-blob (game)
  "Spawn a blob depending on the current frame."
  (when (= 0 (mod (game-frame game) (* 2 +frame-rate+)))
    (let ((blob (blob (textures-blob (game-textures game)))))
      (setf (gethash (game-frame game) (game-blobs game)) blob))))

(defun move-all-blobs (game)
  "Move all blobs currently spawned into the `game'."
  (with-hash-table-iterator (iter (game-blobs game))
    (labels ((recurse ()
               (multiple-value-bind (entry? key blob) (iter)
                 (when entry?
                   (move blob)
                   (when (offscreen-vert? blob)
                     (remhash key (game-blobs game)))
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
