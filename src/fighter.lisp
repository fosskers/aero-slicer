;;; Everything involving the fighter and his beam weapon.
;;;
;;; The beam weapon comes in three strengths, represented by beam width. The
;;; initial beam is 4 pixels wide. The next is 8, and the final is 16, the full
;;; width of the ship.

(in-package :aero-fighter)

#+nil
(launch)

;; --- Types --- ;;

(defstruct fighter
  "The player's fighter ship."
  (animated  nil :type animated)
  (pos       nil :type raylib:vector2)
  (bbox      nil :type raylib:rectangle)
  ;; Ok / Hit
  (status    'ok :type symbol)
  (status-fc 0   :type fixnum)
  (warp-dir  nil :type symbol)
  (beam      nil :type beam)
  (bombs     3   :type fixnum)
  ;; The last time a bomb was used.
  (bomb-fc   (- +bomb-cooldown+) :type fixnum))

(defun @fighter (fighter-sprite beam-sprite)
  "A smart-constructor for `fighter'."
  (let* ((animated (make-animated :sprite fighter-sprite))
         (rect     (bounding-box animated))
         (width    (raylib:rectangle-width rect))
         (pos      (raylib:make-vector2 :x +fighter-spawn-x+
                                        :y +fighter-spawn-y+)))
    (make-fighter :animated animated
                  :pos pos
                  :bbox (raylib:make-rectangle :x +fighter-spawn-x+
                                               :y +fighter-spawn-y+
                                               :width width
                                               :height (raylib:rectangle-height rect))
                  :beam (@beam beam-sprite pos width +beam-y-offset+))))

(defstruct ghost
  "A warp ghost."
  (animated nil :type animated)
  (pos      nil :type raylib:vector2))

(defun @ghost (sprite)
  "Construct a warp `ghost'."
  (make-ghost :animated (make-animated :sprite sprite)
              :pos (raylib:make-vector2 :x 0.0 :y 0.0)))

(defun draw-ghost (ghost dir f-pos fc)
  "Draw a spoooky warp ghost."
  (let ((x-diff (case dir
                  (left  (- +warp-distance+))
                  (right +warp-distance+)
                  (t 0)))
        (y-diff (case dir
                  (up (- +warp-distance+))
                  (down +warp-distance+)
                  (t 0))))
    (setf (raylib:vector2-x (ghost-pos ghost))
          (+ x-diff (raylib:vector2-x f-pos)))
    (setf (raylib:vector2-y (ghost-pos ghost))
          (+ y-diff (raylib:vector2-y f-pos)))
    (draw-animated (ghost-animated ghost)
                   (ghost-pos ghost)
                   fc
                   :colour +very-faded-blue+)))

;; --- Status --- ;;

(defun maybe-set-warp-direction! (fighter)
  "Attempt to set the warp direction."
  (when (warp-button-down?)
    (when-let* ((dir (warp-direction)))
      (setf (fighter-warp-dir fighter) dir))))

(defun can-bomb? (fighter fc)
  "Could the fighter launch a bomb on this frame?"
  (and (> (fighter-bombs fighter) 0)
       (not (bomb-cooling-down? fighter fc))))

(defun bomb-cooling-down? (fighter fc)
  "Was a bomb very recently used?"
  (< (- fc (fighter-bomb-fc fighter))
     +bomb-cooldown+))

(defun has-bomb-capacity? (fighter)
  "Has at least one bomb been used?"
  (< (fighter-bombs fighter) +bomb-max-capacity+))

(defun kill-fighter! (fighter beam-sprite fc)
  "Reset the fighter's position and animation."
  (setf (fighter-status fighter) 'hit)
  (setf (fighter-status-fc fighter) fc)
  (set-animation! (fighter-animated fighter) 'damaged fc)
  ;; Move him back to the initial spawn position.
  (setf (raylib:vector2-x (fighter-pos fighter)) +fighter-spawn-x+)
  (setf (raylib:vector2-y (fighter-pos fighter)) +fighter-spawn-y+)
  (setf (raylib:rectangle-x (fighter-bbox fighter)) +fighter-spawn-x+)
  (setf (raylib:rectangle-y (fighter-bbox fighter)) +fighter-spawn-y+)
  ;; One of the punishments for dying is the downgrading of your awesome beam width.
  (reset-beam! fighter beam-sprite))

(defun reset-beam! (fighter beam-sprite)
  "Shrink the beam back to a narrower size because the fighter was destroyed, etc."
  (setf (fighter-beam fighter)
        (@beam beam-sprite
               (fighter-pos fighter)
               (raylib:rectangle-width (fighter-bbox fighter))
               +beam-y-offset+)))

;; --- Generics --- ;;

(defmethod tick! ((fighter fighter) fc)
  "Resetting of the fighter's flashing respawn status, etc."
  (cond ((and (eq 'hit (fighter-status fighter))
              (>= (- fc (fighter-status-fc fighter)) (* 1.5 +frame-rate+)))
         (setf (fighter-status fighter) 'ok)
         (setf (fighter-status-fc fighter) fc)
         (set-animation! (fighter-animated fighter) 'idle fc)))
  (tick! (fighter-beam fighter) fc))

(defmethod draw ((fighter fighter) fc)
  (let ((beam (fighter-beam fighter)))
    (when (beam-shooting? beam)
      (draw beam fc))
    (draw-animated (fighter-animated fighter) (fighter-pos fighter) fc)))

(defmethod pos ((fighter fighter))
  (fighter-pos fighter))

(defmethod bbox ((fighter fighter))
  (fighter-bbox fighter))

(defun set-x! (fighter new)
  "Set a new x-axis value for the various subcomponents."
  (let* ((f-pos  (fighter-pos fighter))
         (f-bbox (fighter-bbox fighter))
         (beam   (fighter-beam fighter))
         (b-pos  (beam-pos  beam))
         (b-bbox (beam-bbox beam)))
    (setf (raylib:vector2-x f-pos) new)
    (setf (raylib:rectangle-x f-bbox) new)
    (setf (raylib:vector2-x b-pos) (+ new (beam-x-offset beam)))
    (setf (raylib:rectangle-x b-bbox) (+ new (beam-x-offset beam)))))

(defun set-y! (fighter new)
  "Set a new y-axis value for the various subcomponents."
  (let* ((f-pos  (fighter-pos fighter))
         (f-bbox (fighter-bbox fighter))
         (b-pos  (beam-pos  (fighter-beam fighter)))
         (b-bbox (beam-bbox (fighter-beam fighter))))
    (setf (raylib:vector2-y f-pos) new)
    (setf (raylib:rectangle-y f-bbox) new)
    (setf (raylib:vector2-y b-pos) (+ new +beam-y-offset+))
    (setf (raylib:rectangle-y b-bbox) (+ new +beam-y-offset+))))

(defmethod move! ((fighter fighter))
  "Move the fighter depending on the current button presses."
  (let ((warp-pressed? (warp-button-down?)))
    (cond ((and (fighter-warp-dir fighter)
                (not warp-pressed?))
           (move-by-warp! fighter))
          (warp-pressed? nil)
          (t (move-by-press! fighter)))))

(defun move-by-warp! (fighter)
  "The fighter is warping in a set direction."
  (let ((pos (fighter-pos fighter)))
    (case (fighter-warp-dir fighter)
      (up (let ((new (max -106.0 (- (raylib:vector2-y pos) +warp-distance+))))
            (set-y! fighter new)))
      (down (let ((new (min +104.0 (+ +warp-distance+ (raylib:vector2-y pos)))))
              (set-y! fighter new)))
      (left (let ((new (max -128.0 (- (raylib:vector2-x pos) +warp-distance+))))
              (set-x! fighter new)))
      (right (let ((new (min +112.0 (+ +warp-distance+ (raylib:vector2-x pos)))))
               (set-x! fighter new))))
    (setf (fighter-warp-dir fighter) nil)))

(defun move-by-press! (fighter)
  "Move the fighter in a normal, non-warping fashion."
  (let ((pos  (fighter-pos fighter))
        (dist 2.0))
    (when (or (raylib:is-key-down +key-right+)
              (raylib:is-gamepad-button-down +gamepad+ +gamepad-right+))
      (let ((new (min +112.0 (+ dist (raylib:vector2-x pos)))))
        (set-x! fighter new)))
    (when (or (raylib:is-key-down +key-left+)
              (raylib:is-gamepad-button-down +gamepad+ +gamepad-left+))
      (let ((new (max -128.0 (- (raylib:vector2-x pos) dist))))
        (set-x! fighter new)))
    (when (or (raylib:is-key-down +key-down+)
              (raylib:is-gamepad-button-down +gamepad+ +gamepad-down+))
      (let ((new  (min +104.0 (+ dist (raylib:vector2-y pos)))))
        (set-y! fighter new)))
    (when (or (raylib:is-key-down +key-up+)
              (raylib:is-gamepad-button-down +gamepad+ +gamepad-up+))
      (let ((new (max -106.0 (- (raylib:vector2-y pos) dist))))
        (set-y! fighter new)))))
