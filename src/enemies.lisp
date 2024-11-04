(in-package :aero-fighter)

#+nil
(launch)

;; --- General --- ;;

(defun offscreen-vert? (guy)
  "Is the dude off the bottom end of the screen?"
  (let ((y (raylib:vector2-y (pos guy))))
    (> y +world-max-y+)))

(defun move-enemies (enemies)
  "Move all enemies of a certain type. If they move off the end of the screen,
despawn them."
  (with-hash-table-iterator (iter enemies)
    (labels ((recurse ()
               (multiple-value-bind (entry? key enemy) (iter)
                 (when entry?
                   (move enemy)
                   (when (offscreen-vert? enemy)
                     (remhash key enemies))
                   (recurse)))))
      (recurse))))

;; --- Tanks --- ;;

(defstruct tank
  "Tanks that shoot back at the fighter."
  (animated nil :type animated)
  (pos      nil :type raylib:vector2)
  (bbox     nil :type raylib:rectangle)
  (health   1   :type fixnum)
  (beam     nil :type beam))

(defun tank (tank-sprite beam-sprite)
  "Spawn a `tank' with an associated `beam'."
  (let* ((pos (raylib:make-vector2 :y (float (- +world-min-y+ 16))
                                   :x (float (- (random +world-pixels-x+)
                                                +world-max-x+))))
         (t-animated (make-animated :sprite tank-sprite))
         (t-rect     (bounding-box t-animated))
         (b-animated (make-animated :sprite beam-sprite :default 'shooting :active 'shooting))
         (b-rect     (bounding-box b-animated)))
    (make-tank :animated t-animated
               :pos pos
               :bbox (raylib:make-rectangle :x (raylib:vector2-x pos)
                                            :y (raylib:vector2-y pos)
                                            :width (raylib:rectangle-width t-rect)
                                            :height (raylib:rectangle-height t-rect))
               :beam (make-beam :animated b-animated
                                :pos (raylib:make-vector2 :x (+ +tank-beam-x-offset+ (raylib:vector2-x pos))
                                                          :y (+ +tank-beam-y-offset+ (raylib:vector2-y pos)))
                                :bbox (raylib:make-rectangle :x (+ +tank-beam-x-offset+ (raylib:vector2-x pos))
                                                             :y (+ +tank-beam-y-offset+ (raylib:vector2-y pos))
                                                             :width (raylib:rectangle-width b-rect)
                                                             :height (raylib:rectangle-height b-rect))
                                :shot-dur (shot-duration (animated-sprite b-animated))))))

(defmethod pos ((tank tank))
  (tank-pos tank))

(defmethod bbox ((tank tank))
  (tank-bbox tank))

;; TODO: 2024-11-04 The reversals.
(defmethod move ((tank tank))
  "Steady movement down the screen with occasional reversals."
  (incf (raylib:vector2-y   (tank-pos tank)) 0.5)
  (incf (raylib:rectangle-y (tank-bbox tank)) 0.5)
  (incf (raylib:vector2-y   (beam-pos (tank-beam tank))) 0.5)
  (incf (raylib:rectangle-y (beam-bbox (tank-beam tank))) 0.5))

(defmethod draw ((tank tank) fc)
  (when (beam-shooting? (tank-beam tank))
    (draw (tank-beam tank) fc))
  (raylib:draw-texture-v (sprite-texture (animated-sprite (tank-animated tank)))
                         (tank-pos tank)
                         raylib:+white+))

(defun maybe-spawn-tank (game)
  "Spawn a tank depending on the current frame."
  (when (= 0 (mod (game-frame game) (* 4 +frame-rate+)))
    (let* ((sprites (game-sprites game))
           (tank (tank (sprites-tank sprites)
                       (sprites-beam sprites))))
      (setf (gethash (game-frame game) (game-tanks game)) tank))))

(defun maybe-tank-shoot (tank fc)
  "Make the tank fire if conditions are met."
  (when (and (zerop (mod fc (* +frame-rate+)))
             (not (beam-shooting? (tank-beam tank)))
             (< 4 (random 10)))
    (shoot-beam (tank-beam tank) fc)))

(defmethod health ((tank tank))
  (tank-health tank))

(defmethod apply-damage ((tank tank))
  (decf (tank-health tank)))

(defun update-tank-status (tank fc)
  "Turn off the beam, etc."
  (update-beam-status (tank-beam tank) fc))

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

(defmethod draw ((blob blob) fc)
  (raylib:draw-texture-v (sprite-texture (animated-sprite (blob-animated blob)))
                         (blob-pos blob)
                         raylib:+white+))

(defmethod health ((blob blob))
  (blob-health blob))

(defmethod apply-damage ((blob blob))
  (decf (blob-health blob)))

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
;;
;; 2024-11-04 I've made these a `defmethod' to reduce some overall duplication.
(defmethod draw ((building building) fc)
  (raylib:draw-texture-v (sprite-texture (animated-sprite (building-animated building)))
                         (building-pos building)
                         raylib:+white+))

;; --- Shadow Fighters --- ;;
