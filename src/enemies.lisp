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
  (animated nil :type animated)
  ;; We need to know the original X value of the spawn point in order to
  ;; consistently calculate the position following a sine wave. See `move'
  ;; below.
  (orig-x   nil :type single-float)
  (pos      nil :type raylib:vector2)
  (bbox     nil :type raylib:rectangle)
  (health   1   :type fixnum))

(defun blob (sprite)
  "Spawn a `blob' somewhere off the top of the screen."
  (let* ((pos (raylib:make-vector2 :y (float (- +world-min-y+ 16))
                                   :x (float (- (random +world-pixels-x+)
                                                +world-max-x+))))
         (animated (make-animated :sprite sprite))
         (rect (bounding-box animated)))
    (make-blob :animated animated
               :orig-x (raylib:vector2-x pos)
               :pos pos
               :bbox (raylib:make-rectangle :x (raylib:vector2-x pos)
                                            :y (raylib:vector2-y pos)
                                            :width (raylib:rectangle-width rect)
                                            :height (raylib:rectangle-height rect)))))

(defmethod pos ((blob blob))
  (blob-pos blob))

(defmethod bbox ((blob blob))
  (blob-bbox blob))

(defmethod move ((blob blob))
  "Gradual sinusoidal movement down the screen."
  (let* ((x-diff (coerce (* 16 (sin (* pi 1/32 (raylib:vector2-y (blob-pos blob))))) 'single-float))
         (new-x  (+ x-diff (blob-orig-x blob))))
    (setf (raylib:vector2-x   (blob-pos blob)) new-x)
    (setf (raylib:rectangle-x (blob-bbox blob)) new-x)
    (incf (raylib:vector2-y   (blob-pos blob)) 1.0)
    (incf (raylib:rectangle-y (blob-bbox blob)) 1.0)))

(defun maybe-spawn-blob (game)
  "Spawn a blob depending on the current frame."
  (when (= 0 (mod (game-frame game) (* 2 +frame-rate+)))
    (let ((blob (blob (sprites-blob (game-sprites game)))))
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

(defun draw-blob (blob)
  "Draw and animate a `blob'."
  (raylib:draw-texture-v (sprite-texture (animated-sprite (blob-animated blob)))
                         (blob-pos blob)
                         raylib:+white+))

(defun draw-all-blobs (game)
  "Move all blobs currently spawned into the `game'."
  (with-hash-table-iterator (iter (game-blobs game))
    (labels ((recurse ()
               (multiple-value-bind (entry? key blob) (iter)
                 (declare (ignore key))
                 (when entry?
                   (draw-blob blob)
                   (recurse)))))
      (recurse))))

;; --- Buildings --- ;;

(defstruct building
  "A building structure that the fighter shouldn't crash into."
  (animated nil :type animated)
  (pos      nil :type raylib:vector2)
  (bbox     nil :type raylib:rectangle))

(defun building (sprite)
  "Spawn a `building' somewhere off the top of the screen."
  (let* ((pos (raylib:make-vector2 :y (float (- +world-min-y+ 16))
                                   :x (float (- (random +world-pixels-x+)
                                                +world-max-x+))))
         (animated (make-animated :sprite sprite))
         (rect (bounding-box animated)))
    (make-building :animated animated
                   :pos pos
                   :bbox (raylib:make-rectangle :x (raylib:vector2-x pos)
                                                :y (raylib:vector2-y pos)
                                                :width (raylib:rectangle-width rect)
                                                :height (raylib:rectangle-height rect)))))

(defmethod pos ((building building))
  (building-pos building))

(defmethod bbox ((building building))
  (building-bbox building))

(defmethod move ((building building))
  "Straight movement down the screen."
  (incf (raylib:vector2-y   (building-pos building)) 0.5)
  (incf (raylib:rectangle-y (building-bbox building)) 0.5))

(defun move-all-buildings (game)
  "Move all buildings currently spawned into the `game'."
  (with-hash-table-iterator (iter (game-buildings game))
    (labels ((recurse ()
               (multiple-value-bind (entry? key buildings) (iter)
                 (when entry?
                   (move buildings)
                   (when (offscreen-vert? buildings)
                     (remhash key (game-buildings game)))
                   (recurse)))))
      (recurse))))

(defun maybe-spawn-building (game)
  "Spawn a building depending on the current frame."
  (when (= 0 (mod (game-frame game) (* 3 +frame-rate+)))
    (let ((building (building (sprites-building (game-sprites game)))))
      (setf (gethash (game-frame game) (game-buildings game)) building))))

;; TODO: 2024-11-01 Consider consolidating into something generic if this
;; pattern continues. Although I suspect that buildings will be the only things
;; that don't actually animate. For everything else, `draw-animated' would be
;; used.
;;
;; What about sprite-based UI elements, like Lives and Bombs? Those don't need
;; to animate either.
(defun draw-building (building)
  "Draw and animate a `building'."
  (raylib:draw-texture-v (sprite-texture (animated-sprite (building-animated building)))
                         (building-pos building)
                         raylib:+white+))

(defun draw-all-buildings (game)
  "Move all buildings currently spawned into the `game'."
  (with-hash-table-iterator (iter (game-buildings game))
    (labels ((recurse ()
               (multiple-value-bind (entry? key building) (iter)
                 (declare (ignore key))
                 (when entry?
                   (draw-building building)
                   (recurse)))))
      (recurse))))

;; --- Tanks --- ;;

;; --- Shadow Fighters --- ;;
