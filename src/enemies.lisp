(in-package :aero-fighter)

#+nil
(launch)

;; --- Buildings --- ;;

(defstruct building
  "A building structure that the fighter shouldn't crash into."
  (animated nil :type animated)
  (pos      nil :type raylib:vector2)
  (bbox     nil :type raylib:rectangle)
  (shadow   nil :type shadow))

(defun @building (sprite buildings road shadow-texture)
  "Spawn a `building' somewhere off the top of the screen."
  (let* ((animated (make-animated :sprite sprite))
         (rect     (bounding-box animated))
         (pos      (random-off-road rect buildings road)))
    (make-building :animated animated
                   :pos pos
                   :bbox (raylib:make-rectangle :x (raylib:vector2-x pos)
                                                :y (raylib:vector2-y pos)
                                                :width (raylib:rectangle-width rect)
                                                :height (raylib:rectangle-height rect))
                   :shadow (@shadow shadow-texture pos :x-offset 4 :y-offset 20))))

(defmethod pos ((building building))
  (building-pos building))

(defmethod bbox ((building building))
  (building-bbox building))

(defmethod move! ((building building))
  "Straight movement down the screen."
  (incf (raylib:vector2-y   (building-pos building)) +slowest-scroll-rate+)
  (incf (raylib:rectangle-y (building-bbox building)) +slowest-scroll-rate+)
  (incf (->> building building-shadow shadow-pos raylib:vector2-y) +slowest-scroll-rate+))

(defmethod draw ((building building) fc)
  (draw-shadow (building-shadow building))
  (raylib:draw-texture-v (->> building building-animated animated-sprite sprite-texture)
                         (building-pos building)
                         +white+))

;; --- Cannons --- ;;

(defstruct cannon
  "A screen-stretching laser that must be warped across."
  (left  nil :type cannon-bulb)
  (right nil :type cannon-bulb)
  (beam  nil :type cannon-beam))

(defstruct cannon-bulb
  "The left/right piece of the `cannon'."
  (animated nil :type animated)
  (pos      nil :type raylib:vector2))

(defstruct cannon-beam
  "The beam portion of the `cannon'."
  (animated nil :type animated)
  (pos      nil :type raylib:vector2)
  (bbox     nil :type raylib:rectangle))

(defun @cannon (bulb-sprite beam-sprite)
  (let* ((animated-beam (make-animated :sprite beam-sprite))
         (rect (bounding-box animated-beam))
         (beam-pos (raylib:make-vector2 :x (float +world-min-x+)
                                        :y (+ 5.0 +world-min-y+))))
    (make-cannon :left (make-cannon-bulb :animated (make-animated :sprite bulb-sprite)
                                         :pos (raylib:make-vector2 :x (float +world-min-x+)
                                                                   :y (float +world-min-y+)))
                 :right (make-cannon-bulb :animated (make-animated :sprite bulb-sprite)
                                          :pos (raylib:make-vector2 :x (float (- +world-max-x+ 12))
                                                                    :y (float +world-min-y+)))
                 :beam (make-cannon-beam :animated animated-beam
                                         :pos beam-pos
                                         :bbox (raylib:make-rectangle :x (raylib:vector2-x beam-pos)
                                                                      :y (raylib:vector2-y beam-pos)
                                                                      :width (raylib:rectangle-width rect)
                                                                      :height (raylib:rectangle-height rect))))))

(defmethod pos ((cannon cannon))
  (->> cannon cannon-left cannon-bulb-pos))

(defmethod bbox ((cannon cannon))
  (->> cannon cannon-beam cannon-beam-bbox))

(defmethod draw ((cannon cannon) fc)
  (let ((left  (cannon-left cannon))
        (right (cannon-right cannon))
        (beam  (cannon-beam cannon)))
    (draw-animated (cannon-beam-animated beam) (cannon-beam-pos beam) fc)
    (draw-animated (cannon-bulb-animated left) (cannon-bulb-pos left) fc)
    (draw-animated (cannon-bulb-animated right) (cannon-bulb-pos right) fc :flip? t)))

(defmethod move! ((cannon cannon))
  (->> cannon cannon-left move!)
  (->> cannon cannon-right move!)
  (->> cannon cannon-beam move!))

(defmethod move! ((bulb cannon-bulb))
  (incf (raylib:vector2-y (cannon-bulb-pos bulb)) 2.0))

(defmethod move! ((beam cannon-beam))
  (incf (raylib:vector2-y (cannon-beam-pos beam)) 2.0)
  (incf (raylib:rectangle-y (cannon-beam-bbox beam)) 2.0))

;; --- Explosions --- ;;

(defstruct explosion
  (animated nil :type animated)
  (pos      nil :type raylib:vector2)
  (start-fc 0   :type fixnum)
  (duration 0   :type fixnum))

(defun @explosion (sprite parent-pos fc)
  "Construct an `explosion' that knows when it should despawn."
  (make-explosion :animated (make-animated :sprite sprite :default :exploding :active :exploding)
                  :pos parent-pos
                  :start-fc fc
                  :duration (sprite-duration sprite)))

(defmethod expired? ((explosion explosion) fc)
  (>= (- fc (explosion-start-fc explosion))
      (explosion-duration explosion)))

(defmethod draw ((explosion explosion) fc)
  (draw-animated (explosion-animated explosion)
                 (explosion-pos explosion)
                 fc))

(defun explode! (game pos)
  "Spawn an explosion at the given position."
  (let ((fc (game-frame game)))
    (setf (gethash (+ fc (raylib:vector2-x pos)) (game-explosions game))
          (@explosion (->> game game-sprites sprites-explosion)
                      (raylib:make-vector2 :x (raylib:vector2-x pos)
                                           :y (raylib:vector2-y pos))
                      fc))))

;; --- Missiles --- ;;

(defstruct missile
  (animated nil :type animated)
  (pos      nil :type raylib:vector2)
  (bbox     nil :type raylib:rectangle)
  (health   1   :type fixnum)
  (shadow   nil :type shadow))

(defun @missile (sprite shadow-texture)
  "Construct a little missile."
  (let* ((pos      (random-spawn-position))
         (animated (make-animated :sprite sprite))
         (rect     (bounding-box animated)))
    (make-missile :animated animated
                  :pos pos
                  :shadow (@shadow shadow-texture pos :x-offset 12)
                  :bbox (raylib:make-rectangle :x (raylib:vector2-x pos)
                                               :y (raylib:vector2-y pos)
                                               :width (raylib:rectangle-width rect)
                                               :height (raylib:rectangle-height rect)))))

(defmethod pos ((missile missile))
  (missile-pos missile))

(defmethod bbox ((missile missile))
  (missile-bbox missile))

(defmethod vulnerable? ((missile missile) fc)
  "Missiles can always be instantly shot down."
  t)

(defmethod damage! ((missile missile) dmg fc)
  (decf (missile-health missile) dmg))

(defmethod health ((missile missile))
  (missile-health missile))

(defmethod points ((missile missile))
  10)

(defmethod draw ((missile missile) fc)
  (draw-shadow (missile-shadow missile))
  (draw-animated (missile-animated missile)
                 (missile-pos missile)
                 fc))

(defmethod move! ((missile missile))
  "Missiles shoot straight ahead quickly."
  (incf (raylib:vector2-y (missile-pos missile)) 2.0)
  (incf (raylib:rectangle-y (missile-bbox missile)) 2.0)
  (incf (->> missile missile-shadow shadow-pos raylib:vector2-y) 2.0))

;; --- Evil Ships --- ;;

(defstruct evil-ship
  "An evil version of the fighter who flies around and shoots back!"
  (animated  nil :type animated)
  (pos       nil :type raylib:vector2)
  (bbox      nil :type raylib:rectangle)
  (health    3   :type fixnum)
  (beam      nil :type beam)
  ;; Ok / Charging
  (status    'ok :type symbol)
  (status-fc 0   :type fixnum)
  ;; The frame upon being last hit by the fighter.
  (hit-fc    0   :type fixnum)
  ;; Tracking the fighter pos so that we can follow him.
  (fighter-pos nil :type raylib:vector2)
  ;; How long the beam charge should last.
  (charge-dur 0  :type fixnum)
  (spawned-fc 0  :type fixnum)
  (shadow    nil :type shadow))

(defun @evil-ship (evil-ship-sprite beam-sprite shadow-texture fighter-pos fc)
  "A smart-constructor for an `evil-ship'."
  (let* ((pos      (random-spawn-position))
         (animated (make-animated :sprite evil-ship-sprite))
         (rect     (bounding-box animated))
         (width    (raylib:rectangle-width rect)))
    (make-evil-ship :animated animated
                    :pos pos
                    :fighter-pos fighter-pos
                    :bbox (raylib:make-rectangle :x (raylib:vector2-x pos)
                                                 :y (raylib:vector2-y pos)
                                                 :width width
                                                 :height (raylib:rectangle-height rect))
                    :beam (@beam beam-sprite pos width +evil-ship-beam-y-offset+)
                    :charge-dur (charge-duration evil-ship-sprite)
                    :spawned-fc fc
                    :shadow (@shadow shadow-texture pos :flip? t))))

(defmethod pos ((evil-ship evil-ship))
  (evil-ship-pos evil-ship))

(defmethod bbox ((evil-ship evil-ship))
  (evil-ship-bbox evil-ship))

(defmethod beam ((evil-ship evil-ship))
  (evil-ship-beam evil-ship))

(defmethod draw ((evil-ship evil-ship) fc)
  (let ((beam (evil-ship-beam evil-ship)))
    (when (beam-shooting? beam)
      (draw beam fc)))
  (draw-shadow (evil-ship-shadow evil-ship))
  (draw-animated (evil-ship-animated evil-ship)
                 (evil-ship-pos evil-ship)
                 fc))

;; - Move closer to the fighter.
;; - Stay within a certain Y distance, such that the Evil stays within the bounds of the screen.
;; - Thus, it will back up if the fighter approaches.
;; - Try to X-align such that a shot will hit.

(defun inc-evil-x! (evil-ship new)
  (let ((e-pos (evil-ship-pos evil-ship))
        (bbox  (evil-ship-bbox evil-ship))
        (beam  (evil-ship-beam evil-ship)))
    (incf (raylib:vector2-x e-pos) new)
    (incf (raylib:rectangle-x bbox) new)
    (incf (raylib:vector2-x (beam-pos beam)) new)
    (incf (raylib:rectangle-x (beam-bbox beam)) new)
    (incf (->> evil-ship evil-ship-shadow shadow-pos raylib:vector2-x) new)))

(defun inc-evil-y! (evil-ship new)
  (let ((e-pos (evil-ship-pos evil-ship))
        (bbox  (evil-ship-bbox evil-ship))
        (beam  (evil-ship-beam evil-ship)))
    (incf (raylib:vector2-y e-pos) new)
    (incf (raylib:rectangle-y bbox) new)
    (incf (raylib:vector2-y (beam-pos beam)) new)
    (incf (raylib:rectangle-y (beam-bbox beam)) new)
    (incf (->> evil-ship evil-ship-shadow shadow-pos raylib:vector2-y) new)))

(defun move-evil-ship! (evil-ship buildings)
  "Move the evil fighter depending on the position of the good fighter and the
surrounding buildings."
  (let* ((f-pos  (evil-ship-fighter-pos evil-ship))
         (e-pos  (evil-ship-pos evil-ship))
         (x-diff (- (raylib:vector2-x f-pos)
                    (raylib:vector2-x e-pos)))
         (y-diff (- (raylib:vector2-y f-pos)
                    (raylib:vector2-y e-pos))))
    (unless (t:transduce (t:comp (t:map #'cdr)
                                 (t:map (lambda (building)
                                          (let* ((b-pos (building-pos building))
                                                 (rect  (->> building building-animated bounding-box))
                                                 (x1    (raylib:vector2-x b-pos))
                                                 (y1    (raylib:vector2-y b-pos))
                                                 (x2    (+ x1 (raylib:rectangle-width rect)))
                                                 (y2    (+ y1 (raylib:rectangle-height rect))))
                                            (or (dodge-if-close! evil-ship x1 y1 :paranoia-radius 18 :dodge 'left)
                                                (dodge-if-close! evil-ship x2 y2 :paranoia-radius 18 :dodge 'right))))))
                         (t:fold (lambda (acc dodged?) (or acc dodged?)) nil) buildings)
      (move-or-dodge-if-close! evil-ship x-diff y-diff))))

(defun move-or-dodge-if-close! (evil-ship x-diff y-diff &key (paranoia-radius +evil-ship-paranoia-radius+))
  "Moves the evil ship according to a given desired trajectory, but takes a
perpendicular course instead if he detects he's too close to some object."
  (let* ((dist   (euclidean-distance x-diff y-diff))
         (too-close? (< dist paranoia-radius)))
    (multiple-value-bind (corrected-x corrected-y)
        ;; This coordinate flipping has the effect in Linear Algebra of
        ;; producing a perpendicular vector. For any given vector there are of
        ;; course two possible perpendicular ones, and we bias toward the one
        ;; the always moves the evil ship back up toward the top of the screen.
        ;; The visual effect is that he's "backing up" to get a better shot on
        ;; the fighter.
        (cond ((and too-close? (neg? x-diff)) (values (- y-diff) x-diff))
              (too-close? (values y-diff (- x-diff)))
              (t (values x-diff y-diff)))
      (let* ((nor-x (/ corrected-x (* dist 1.25)))
             (nor-y (/ corrected-y (* dist 1.25))))
        (unless (close-to-zero? nor-y)
          (inc-evil-x! evil-ship nor-x)
          (inc-evil-y! evil-ship nor-y))))))

(defun dodge-if-close! (evil-ship x-diff y-diff &key paranoia-radius dodge)
  "Moves the evil ship away from some entity."
  (let ((dist  (euclidean-distance x-diff y-diff)))
    (when (< dist paranoia-radius)
      (multiple-value-bind (corrected-x corrected-y)
          (case dodge
            (left (if (neg? y-diff)
                      (values y-diff (- x-diff))
                      (values (- y-diff) x-diff)))
            (right (if (neg? y-diff)
                       (values (- y-diff) x-diff)
                       (values y-diff (- x-diff)))))
        (let* ((nor-x (/ corrected-x (max dist 0.1)))
               (nor-y (/ corrected-y (max dist 0.1))))
          (inc-evil-x! evil-ship nor-x)
          (inc-evil-y! evil-ship nor-y))))))

(defmethod tick! ((evil-ship evil-ship) fc)
  "Shooting and turning off the beam."
  (let ((beam (evil-ship-beam evil-ship)))
    (maybe-ship-shoot! evil-ship fc)
    (tick! beam fc)))

(defun maybe-ship-shoot! (evil-ship fc)
  "Make the evil ship shoot if conditions are met."
  (cond ((and (eq 'ok (evil-ship-status evil-ship))
              (not (beam-shooting? (evil-ship-beam evil-ship)))
              ;; NOTE: So that the ship will never try to shoot while off the
              ;; top of the screen.
              (> (raylib:vector2-y (evil-ship-pos evil-ship))
                 +world-min-y+)
              (zerop (mod (- fc (evil-ship-spawned-fc evil-ship))
                          (* +frame-rate+)))
              (< (random 10) 6))
         (setf (evil-ship-status evil-ship) 'charging)
         (setf (evil-ship-status-fc evil-ship) fc)
         (set-animation! (evil-ship-animated evil-ship) :charging fc))
        ((and (eq 'charging (evil-ship-status evil-ship))
              (>= (- fc (evil-ship-status-fc evil-ship))
                  (evil-ship-charge-dur evil-ship)))
         (setf (evil-ship-status evil-ship) 'ok)
         (setf (evil-ship-status-fc evil-ship) fc)
         (set-animation! (evil-ship-animated evil-ship) :idle fc)
         (shoot-beam! (evil-ship-beam evil-ship) fc))))

(defmethod vulnerable? ((evil-ship evil-ship) fc)
  (>= (- fc (evil-ship-hit-fc evil-ship))
      +enemy-invincibility-frames+))

(defmethod damage! ((evil-ship evil-ship) dmg fc)
  (setf (evil-ship-hit-fc evil-ship) fc)
  (decf (evil-ship-health evil-ship) dmg))

(defmethod health ((evil-ship evil-ship))
  (evil-ship-health evil-ship))

(defmethod points ((evil-ship evil-ship))
  100)

;; --- Tanks --- ;;

(defstruct tank
  "Tanks that shoot back at the fighter."
  (animated   nil :type animated)
  (pos        nil :type raylib:vector2)
  (bbox       nil :type raylib:rectangle)
  (health     2   :type fixnum)
  (beam       nil :type beam)
  (reversing? nil :type symbol)
  ;; Ok / Charging
  (status     'ok :type symbol)
  (status-fc  0   :type fixnum)
  ;; The frame upon being last hit by the fighter.
  (hit-fc     0   :type fixnum)
  (charge-dur 0   :type fixnum)
  (spawned-fc 0   :type fixnum))

(defun @tank (tank-sprite beam-sprite buildings fc)
  "Spawn a `tank' with an associated `beam'."
  (let* ((animated (make-animated :sprite tank-sprite))
         (rect     (bounding-box animated))
         (width    (raylib:rectangle-width rect))
         (pos      (random-pos-clear-of-building width buildings))
         (beam     (@beam beam-sprite pos width +tank-beam-y-offset+)))
    (setf (beam-shot-fc beam) fc)
    (make-tank :animated animated
               :pos pos
               :bbox (raylib:make-rectangle :x (raylib:vector2-x pos)
                                            :y (raylib:vector2-y pos)
                                            :width width
                                            :height (raylib:rectangle-height rect))
               :charge-dur (charge-duration tank-sprite)
               :spawned-fc fc
               :beam beam)))

(defun charge-duration (sprite)
  "How long does the charging animation last in frames?"
  (t:transduce (t:map #'frame-duration-fs)
               #'+ (->> sprite sprite-animations (gethash :charging) animation-frames)))

(defmethod pos ((tank tank))
  (tank-pos tank))

(defmethod bbox ((tank tank))
  (tank-bbox tank))

(defmethod beam ((tank tank))
  (tank-beam tank))

(defmethod move! ((tank tank))
  "Steady movement down the screen with occasional reversals."
  (let ((movement (if (tank-reversing? tank) +slowest-scroll-rate+ (+ +slowest-scroll-rate+ 0.1))))
    (incf (raylib:vector2-y   (tank-pos tank)) movement)
    (incf (raylib:rectangle-y (tank-bbox tank)) movement)
    (incf (raylib:vector2-y   (beam-pos (tank-beam tank))) movement)
    (incf (raylib:rectangle-y (beam-bbox (tank-beam tank))) movement)))

(defmethod draw ((tank tank) fc)
  (when (beam-shooting? (tank-beam tank))
    (draw (tank-beam tank) fc))
  (draw-animated (tank-animated tank) (tank-pos tank) fc))

;; TODO: 2024-11-09 Merge with `tick!'?
(defun maybe-tank-shoot! (tank fc)
  "Make the tank fire if conditions are met."
  (cond ((and (eq 'ok (tank-status tank))
              (not (beam-shooting? (tank-beam tank)))
              ;; NOTE: So that the tank will never try to shoot while its off
              ;; the top of the screen.
              (> (raylib:vector2-y (tank-pos tank))
                 +world-min-y+)
              (zerop (mod (- fc (tank-spawned-fc tank)) (* +frame-rate+)))
              (< (random 10) 6))
         (setf (tank-status tank) 'charging)
         (setf (tank-status-fc tank) fc)
         (set-animation! (tank-animated tank) :charging fc))
        ((and (eq 'charging (tank-status tank))
              (>= (- fc (tank-status-fc tank))
                  (tank-charge-dur tank)))
         (setf (tank-status tank) 'ok)
         (setf (tank-status-fc tank) fc)
         (set-animation! (tank-animated tank) :idle fc)
         (shoot-beam! (tank-beam tank) fc))))

(defmethod health ((tank tank))
  (tank-health tank))

(defmethod vulnerable? ((tank tank) fc)
  (>= (- fc (tank-hit-fc tank))
      +enemy-invincibility-frames+))

(defmethod damage! ((tank tank) dmg fc)
  (setf (tank-hit-fc tank) fc)
  (decf (tank-health tank) dmg))

(defmethod tick! ((tank tank) fc)
  "Turn off the beam, etc."
  (maybe-tank-shoot! tank fc)
  (tick! (tank-beam tank) fc)
  (cond ((and (not (tank-reversing? tank))
              (zerop (mod (- fc (tank-spawned-fc tank))
                          (* 3 +frame-rate+)))
              (< (random 10) 3))
         (setf (tank-reversing? tank) t))
        ((and (tank-reversing? tank)
              (zerop (mod (- fc (tank-spawned-fc tank))
                          (* 2 +frame-rate+))))
         (setf (tank-reversing? tank) nil))))

(defmethod points ((tank tank))
  100)

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
  (health   1   :type fixnum)
  (hit-fc   0   :type fixnum)
  (shadow   nil :type shadow))

(defun @blob (sprite shadow-texture)
  "Spawn a `blob' somewhere off the top of the screen."
  (let* ((pos (random-spawn-position))
         (animated (make-animated :sprite sprite))
         (rect (bounding-box animated)))
    (make-blob :animated animated
               :orig-x (raylib:vector2-x pos)
               :pos pos
               :shadow (@shadow shadow-texture pos)
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
  (let* ((x-diff (real->float (* 16 (sin (* pi 1/32 (raylib:vector2-y (blob-pos blob)))))))
         (new-x  (+ x-diff (blob-orig-x blob)))
         (s-pos  (->> blob blob-shadow shadow-pos)))
    (setf (raylib:vector2-x   (blob-pos blob)) new-x)
    (incf (raylib:vector2-y   (blob-pos blob)) +blob-y-speed+)
    (setf (raylib:rectangle-x (blob-bbox blob)) new-x)
    (incf (raylib:rectangle-y (blob-bbox blob)) +blob-y-speed+)
    (setf (raylib:vector2-x s-pos) (+ +shadow-offset+ new-x))
    (incf (raylib:vector2-y s-pos) +blob-y-speed+)))

(defmethod draw ((blob blob) fc)
  (draw-shadow (blob-shadow blob))
  (raylib:draw-texture-v (->> blob blob-animated animated-sprite sprite-texture)
                         (blob-pos blob)
                         +white+))

(defmethod health ((blob blob))
  (blob-health blob))

(defmethod vulnerable? ((blob blob) fc)
  (>= (- fc (blob-hit-fc blob))
      +enemy-invincibility-frames+))

(defmethod damage! ((blob blob) dmg fc)
  (setf (blob-hit-fc blob) fc)
  (decf (blob-health blob) dmg))

(defmethod points ((blob blob))
  100)

;; --- General --- ;;

(defun random-spawn-position ()
  "A useful spawn position for enemies."
  (let ((rand-x (- (random +world-pixels-x+)
                   +world-max-x+
                   8)))
    (raylib:make-vector2 :y (float (- +world-min-y+ 36))
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

(defun random-off-road (rect buildings road)
  "Find a random spawn position for a building, such that it isn't on the road, nor
is it overlapping with any existing buildings."
  (let* ((pos  (random-spawn-position))
         (pair (make-rect-pos :pos pos :rect rect)))
    (setf (raylib:rectangle-x rect) (raylib:vector2-x pos))
    (setf (raylib:rectangle-y rect) (raylib:vector2-y pos))
    (if (or (any-entity-collision? pair buildings)
            (any-entity-collision? pair road))
        (random-off-road rect buildings road)
        pos)))

(defun random-pos-clear-of-building (width buildings)
  "Find a spawn position that isn't behind some building. A bit crude, but gets the
job done."
  (let ((pos (random-spawn-position)))
    (if (behind-buildings? pos width buildings)
        (random-pos-clear-of-building width buildings)
        pos)))

(defun behind-buildings? (pos width buildings)
  "Is some entity (given by its `pos') behind one of the buildings? If so, yields
the X of that building."
  (t:transduce (t:comp (t:filter (lambda (building) (x-overlapping? pos width (cdr building))))
                       (t:map (lambda (building) (->> building cdr building-pos raylib:vector2-x))))
               (t:find #'identity) buildings))

(defun x-overlapping? (pos width thing)
  "Is some entity (given by its `pos' and `width') overlapping some other entity,
at least in the X-direction? If so, yields the minimum X of that overlapped
entity."
  (let* ((b-l (->> thing pos raylib:vector2-x))
         (b-r (+ b-l (->> thing bbox raylib:rectangle-width)))
         (p-l (raylib:vector2-x pos))
         (p-r (+ p-l width)))
    (when (or (<= b-l p-r b-r)
              (<= b-l p-l b-r))
      b-l)))
