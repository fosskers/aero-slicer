(in-package :aero-fighter)

#+nil
(launch)

;; --- General --- ;;

(defun random-spawn-position ()
  "A useful spawn position for enemies."
  (let ((rand-x (- (random +world-pixels-x+)
                   +world-max-x+
                   8)))
    (raylib:make-vector2 :y (float (- +world-min-y+ 16))
                         :x (float (max +world-min-x+ rand-x)))))

(defun offscreen-vert? (guy)
  "Is the dude off the bottom end of the screen?"
  (let ((y (raylib:vector2-y (pos guy))))
    (> y +world-max-y+)))

(defun move-enemies! (enemies)
  "Move all enemies of a certain type. If they move off the end of the screen,
despawn them."
  (with-hash-table-iterator (iter enemies)
    (labels ((recurse ()
               (multiple-value-bind (entry? key enemy) (iter)
                 (when entry?
                   (move! enemy)
                   (when (offscreen-vert? enemy)
                     (remhash key enemies))
                   (recurse)))))
      (recurse))))

;; --- Explosions --- ;;

(defstruct explosion
  (animated nil :type animated)
  (pos      nil :type raylib:vector2)
  (start-fc 0   :type fixnum)
  (duration 0   :type fixnum))

(defun explosion (sprite parent-pos fc)
  "Construct an `explosion' that knows when it should despawn."
  (make-explosion :animated (make-animated :sprite sprite :default 'exploding :active 'exploding)
                  :pos parent-pos
                  :start-fc fc
                  :duration (sprite-duration sprite)))

(defmethod expired? ((explosion explosion) fc)
  (> (- fc (explosion-start-fc explosion))
     (explosion-duration explosion)))

(defmethod draw ((explosion explosion) fc)
  (draw-animated (explosion-animated explosion)
                 (explosion-pos explosion)
                 fc))

(defun explode! (game enemy enemy-key)
  "Spawn an explosion on top of a given enemy."
  (let* ((fc (game-frame game))
         (explosion (explosion (sprites-explosion (game-sprites game))
                               (pos enemy)
                               fc)))
    ;; NOTE: If we just set the key to the
    ;; current fc, then when multiple enemies
    ;; were hit, only one explosion would
    ;; actually spawn since they Hash Table keys
    ;; would collide. We need some
    ;; disambiguating factor, which is precisely
    ;; the addition of the key of the enemy we
    ;; hit.
    (setf (gethash (+ enemy-key
                      (floor (raylib:vector2-y (pos enemy)))
                      fc)
                   (game-explosions game))
          explosion)))

;; --- Evil Ships --- ;;

(defstruct evil-ship
  "An evil version of the fighter who flies around and shoots back!"
  (animated nil :type animated)
  (pos      nil :type raylib:vector2)
  (bbox     nil :type raylib:rectangle)
  (health   0   :type fixnum)
  (beam     nil :type beam)
  ;; The frame upon being last hit by the fighter.
  (hit-fc   0   :type fixnum)
  ;; Tracking the fighter pos so that we can follow him.
  (fighter-pos nil :type raylib:vector2))

(defun evil-ship (evil-ship-sprite beam-sprite fighter-pos level fc)
  "A smart-constructor for an `evil-ship'."
  (let* ((pos        (random-spawn-position))
         (s-animated (make-animated :sprite evil-ship-sprite))
         (s-rect     (bounding-box s-animated))
         (b-animated (make-animated :sprite beam-sprite :default 'shooting :active 'shooting))
         (b-rect     (bounding-box b-animated)))
    (make-evil-ship :animated s-animated
                    :pos pos
                    :fighter-pos fighter-pos
                    :bbox (raylib:make-rectangle :x (raylib:vector2-x pos)
                                                 :y (raylib:vector2-y pos)
                                                 :width (raylib:rectangle-width s-rect)
                                                 :height (raylib:rectangle-height s-rect))
                    :health (+ +evil-ship-base-hp+ level)
                    :beam (make-beam :animated b-animated
                                     ;; FIXME: 2024-11-15 Don't use tank offsets.
                                     :pos (raylib:make-vector2 :x (+ +tank-beam-x-offset+ (raylib:vector2-x pos))
                                                               :y (+ +tank-beam-y-offset+ (raylib:vector2-y pos)))
                                     :bbox (raylib:make-rectangle :x (+ +tank-beam-x-offset+ (raylib:vector2-x pos))
                                                                  :y (+ +tank-beam-y-offset+ (raylib:vector2-y pos))
                                                                  :width (raylib:rectangle-width b-rect)
                                                                  :height (raylib:rectangle-height b-rect))
                                     :shot-dur (sprite-duration (animated-sprite b-animated))
                                     ;; Ensures that a newly spawned, offscreen tank
                                     ;; can't start shooting. Increase this if necessary.
                                     :shot-fc fc))))

(defmethod pos ((evil-ship evil-ship))
  (evil-ship-pos evil-ship))

(defmethod bbox ((evil-ship evil-ship))
  (evil-ship-bbox evil-ship))

(defmethod draw ((evil-ship evil-ship) fc)
  (let ((beam (evil-ship-beam evil-ship)))
    (when (beam-shooting? beam)
      (draw beam fc)))
  (draw-animated (evil-ship-animated evil-ship)
                 (evil-ship-pos evil-ship)
                 fc))

;; TODO: 2024-11-15 Move the beam too.
(defmethod move! ((evil-ship evil-ship))
  "Move the evil fighter depending on the position of the good fighter."
  (let* ((f-pos  (evil-ship-fighter-pos evil-ship))
         (e-pos  (evil-ship-pos evil-ship))
         (x-diff (- (raylib:vector2-x f-pos)
                    (raylib:vector2-x e-pos)))
         (y-diff (- (raylib:vector2-y f-pos)
                    (raylib:vector2-y e-pos)))
         ;; Normalisation.
         (mag    (sqrt (+ (expt x-diff 2) (expt y-diff 2))))
         (nor-x  (/ x-diff (max mag 0.1)))
         (nor-y  (/ y-diff (max mag 0.1))))
    ;; FIXME: 2024-11-15 Without normalisation this is probably going to be way
    ;; too fast! I'm pretty sure I was doing this in Save the Farm for the
    ;; "seeker" bug, so reference that code.
    (incf (raylib:vector2-x e-pos) nor-x)
    (incf (raylib:vector2-y e-pos) nor-y)
    (incf (raylib:rectangle-x (evil-ship-bbox evil-ship)) nor-x)
    (incf (raylib:rectangle-y (evil-ship-bbox evil-ship)) nor-y)))

(defun maybe-spawn-ship! (game)
  "Spawn an evil ship depending on the current frame."
  (let ((fc (game-frame game)))
    (when (= 0 (mod fc (* 6 +frame-rate+)))
      (let* ((sprites (game-sprites game))
             (evil-ship (evil-ship (sprites-evil-ship sprites)
                                   (sprites-beam-4 sprites)
                                   (fighter-pos (game-fighter game))
                                   (game-level game)
                                   fc)))
        (setf (gethash fc (game-evil-ships game)) evil-ship)))))

(defmethod vulnerable? ((evil-ship evil-ship) fc)
  (> (- fc (evil-ship-hit-fc evil-ship))
     +frame-rate+))

(defmethod damage! ((evil-ship evil-ship) fc)
  (setf (evil-ship-hit-fc evil-ship) fc)
  (decf (evil-ship-health evil-ship)))

(defmethod health ((evil-ship evil-ship))
  (evil-ship-health evil-ship))

;; --- Tanks --- ;;

(defstruct tank
  "Tanks that shoot back at the fighter."
  (animated   nil :type animated)
  (pos        nil :type raylib:vector2)
  (bbox       nil :type raylib:rectangle)
  (health     0   :type fixnum)
  (beam       nil :type beam)
  (reversing? nil :type symbol)
  ;; Ok / Charging
  (status     'ok :type symbol)
  (status-fc  0   :type fixnum)
  ;; The frame upon being last hit by the fighter.
  (hit-fc     0   :type fixnum)
  (charge-dur 0   :type fixnum))

(defun tank (tank-sprite beam-sprite level fc)
  "Spawn a `tank' with an associated `beam'."
  (let* ((pos (random-spawn-position))
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
               :health (+ +tank-base-hp+ level)
               :charge-dur (charge-duration tank-sprite)
               :beam (make-beam :animated b-animated
                                :pos (raylib:make-vector2 :x (+ +tank-beam-x-offset+ (raylib:vector2-x pos))
                                                          :y (+ +tank-beam-y-offset+ (raylib:vector2-y pos)))
                                :bbox (raylib:make-rectangle :x (+ +tank-beam-x-offset+ (raylib:vector2-x pos))
                                                             :y (+ +tank-beam-y-offset+ (raylib:vector2-y pos))
                                                             :width (raylib:rectangle-width b-rect)
                                                             :height (raylib:rectangle-height b-rect))
                                ;; TODO: 2024-11-05 Optimization: Precalculate
                                ;; this duration and share it among all tanks.
                                :shot-dur (sprite-duration (animated-sprite b-animated))
                                ;; Ensures that a newly spawned, offscreen tank
                                ;; can't start shooting. Increase this if necessary.
                                :shot-fc fc))))

(defun charge-duration (sprite)
  "How long does the charging animation last in frames?"
  (t:transduce (t:map #'frame-duration-fs)
               #'+ (animation-frames (gethash 'charging (sprite-animations sprite)))))

(defmethod pos ((tank tank))
  (tank-pos tank))

(defmethod bbox ((tank tank))
  (tank-bbox tank))

(defmethod move! ((tank tank))
  "Steady movement down the screen with occasional reversals."
  (let ((movement (if (tank-reversing? tank) -0.15 0.5)))
    (incf (raylib:vector2-y   (tank-pos tank)) movement)
    (incf (raylib:rectangle-y (tank-bbox tank)) movement)
    (incf (raylib:vector2-y   (beam-pos (tank-beam tank))) movement)
    (incf (raylib:rectangle-y (beam-bbox (tank-beam tank))) movement)))

(defmethod draw ((tank tank) fc)
  (when (beam-shooting? (tank-beam tank))
    (draw (tank-beam tank) fc))
  (draw-animated (tank-animated tank) (tank-pos tank) fc))

(defun maybe-spawn-tank! (game)
  "Spawn a tank depending on the current frame."
  (let ((fc (game-frame game)))
    (when (= 0 (mod fc (* 4 +frame-rate+)))
      (let* ((sprites (game-sprites game))
             (tank (tank (sprites-tank sprites)
                         (sprites-beam-4 sprites)
                         (game-level game)
                         fc)))
        (setf (gethash fc (game-tanks game)) tank)))))

;; TODO: 2024-11-09 Merge with `tick!'?
(defun maybe-tank-shoot! (tank fc)
  "Make the tank fire if conditions are met."
  (cond ((and (eq 'ok (tank-status tank))
              (not (beam-shooting? (tank-beam tank)))
              ;; NOTE: So that the tank will never try to shoot while its off
              ;; the top of the screen.
              (> (raylib:vector2-y (tank-pos tank))
                 +world-min-y+)
              (zerop (mod fc (* +frame-rate+)))
              (< (random 10) 4))
         (setf (tank-status tank) 'charging)
         (setf (tank-status-fc tank) fc)
         (set-animation! (tank-animated tank) 'charging))
        ((and (eq 'charging (tank-status tank))
              (> (- fc (tank-status-fc tank))
                 (tank-charge-dur tank)))
         (setf (tank-status tank) 'ok)
         (setf (tank-status-fc tank) fc)
         (set-animation! (tank-animated tank) 'idle)
         (shoot-beam! (tank-beam tank) fc))))

(defmethod health ((tank tank))
  (tank-health tank))

(defmethod vulnerable? ((tank tank) fc)
  (> (- fc (tank-hit-fc tank))
     +frame-rate+))

(defmethod damage! ((tank tank) fc)
  (setf (tank-hit-fc tank) fc)
  (decf (tank-health tank)))

(defmethod tick! ((tank tank) fc)
  "Turn off the beam, etc."
  (tick! (tank-beam tank) fc)
  (cond ((and (not (tank-reversing? tank))
              (zerop (mod fc (* 3 +frame-rate+)))
              (< (random 10) 3))
         (setf (tank-reversing? tank) t))
        ((and (tank-reversing? tank)
              (zerop (mod fc (* 3 +frame-rate+))))
         (setf (tank-reversing? tank) nil))))

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
  (health   0   :type fixnum)
  (hit-fc   0   :type fixnum))

(defun blob (sprite level)
  "Spawn a `blob' somewhere off the top of the screen."
  (let* ((pos (random-spawn-position))
         (animated (make-animated :sprite sprite))
         (rect (bounding-box animated)))
    (make-blob :animated animated
               :health (+ +blob-base-hp+ level)
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

(defmethod move! ((blob blob))
  "Gradual sinusoidal movement down the screen."
  (let* ((x-diff (coerce (* 16 (sin (* pi 1/32 (raylib:vector2-y (blob-pos blob))))) 'single-float))
         (new-x  (+ x-diff (blob-orig-x blob))))
    (setf (raylib:vector2-x   (blob-pos blob)) new-x)
    (setf (raylib:rectangle-x (blob-bbox blob)) new-x)
    (incf (raylib:vector2-y   (blob-pos blob)) 1.0)
    (incf (raylib:rectangle-y (blob-bbox blob)) 1.0)))

(defun maybe-spawn-blob! (game)
  "Spawn a blob depending on the current frame."
  (when (= 0 (mod (game-frame game) (* 2 +frame-rate+)))
    (let ((blob (blob (sprites-blob (game-sprites game))
                      (game-level game))))
      (setf (gethash (game-frame game) (game-blobs game)) blob))))

(defmethod draw ((blob blob) fc)
  (raylib:draw-texture-v (sprite-texture (animated-sprite (blob-animated blob)))
                         (blob-pos blob)
                         raylib:+white+))

(defmethod health ((blob blob))
  (blob-health blob))

(defmethod vulnerable? ((blob blob) fc)
  (> (- fc (blob-hit-fc blob))
     +frame-rate+))

(defmethod damage! ((blob blob) fc)
  (setf (blob-hit-fc blob) fc)
  (decf (blob-health blob)))

;; --- Buildings --- ;;

(defstruct building
  "A building structure that the fighter shouldn't crash into."
  (animated nil :type animated)
  (pos      nil :type raylib:vector2)
  (bbox     nil :type raylib:rectangle))

(defun building (sprite)
  "Spawn a `building' somewhere off the top of the screen."
  (let* ((pos (random-spawn-position))
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

(defmethod move! ((building building))
  "Straight movement down the screen."
  (incf (raylib:vector2-y   (building-pos building)) 0.25)
  (incf (raylib:rectangle-y (building-bbox building)) 0.25))

(defun maybe-spawn-building! (game)
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
