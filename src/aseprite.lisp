;;; Animated sprites, as pulled from an Aseprite sprite sheet.

(in-package :aero-fighter)

;; Requirements:
;;
;; - A command to play the same animation as the current one should _not_ reset the animation.
;; - A completed animation should return to the default/first one.

#+nil
(launch)

;; --- Types --- ;;

(defstruct frame
  "A single animation frame."
  (rect        nil :type raylib:rectangle :read-only t)
  (duration-ms 0   :type fixnum :read-only t)
  (duration-fs 0   :type fixnum :read-only t))

(defstruct animation
  "One or more frames of an animation."
  (frames nil :type vector :read-only t))

(defstruct sprite
  "The base definition of an animated sprite. Should be created once and shared
around multiple instances of an `animated'."
  (texture    nil :type raylib:texture :read-only t)
  (animations nil :type hash-table :read-only t))

(defun @sprite (path)
  "Read animated sprite data from the given JSON file. Must be called after a GL
context has been initialised via `raylib:init-window'."
  (let* ((json   (jzon:parse path))
         (meta   (gethash "meta" json))
         (texture (->> meta (gethash "image") (p:with-name path) p:to-string raylib:load-texture))
         (frames (gethash "frames" json))
         (tags   (gethash "frameTags" meta))
         (anims  (t:transduce (t:map (lambda (tag) (json->animation frames tag))) #'t:hash-table tags)))
    (make-sprite :texture texture
                 :animations anims)))

(defstruct animated
  "An actual animated sprite that tracks its own state. Should be one per living
entity in the game, but all instances of the same entity type should share an
underlying `sprite' definition."
  (sprite  nil   :type sprite :read-only t)
  (default :idle :type keyword)
  (active  :idle :type keyword)
  (frame   0     :type fixnum)
  ;; The fc when _this frame_ began.
  (started 0     :type fixnum))

;; --- Reading Asesprite Data --- ;;

(defun json->frame (json)
  "Read a `frame' out of some JSON."
  (let ((dim    (gethash "frame" json))
        (millis (gethash "duration" json)))
    (make-frame :duration-ms millis
                ;; So long as the millis count of the single animation frame is
                ;; no less than 16ms, the `round` here will ensure that the
                ;; result is 1 at minimum. A 0 here would likely mean that the
                ;; frame never renders within `draw-animated'.
                :duration-fs (round (/ millis +millis-per-frame+))
                :rect (raylib:make-rectangle
                       :x (float (gethash "x" dim))
                       :y (float (gethash "y" dim))
                       :width (float (gethash "w" dim))
                       :height (float (gethash "h" dim))))))

(defun json->animation (frames json)
  "Read an `animation' out of some JSON."
  (let* ((from (gethash "from" json))
         (to   (gethash "to" json))
         (frs  (t:transduce (t:comp (t:drop from)
                                    (t:take (1+ (- to from)))
                                    (t:map #'json->frame))
                            #'t:vector frames)))
    (cons (intern (string-upcase (gethash "name" json)) "KEYWORD")
          (make-animation :frames frs))))

;; --- Helpers --- ;;

(defun set-animation! (animated active fc)
  "Alter the current animation in a safe way. In general, if you forget to reset
the current `frame' when altering `active', you'll notice animations starting
and stopping at unexpected frames."
  (setf (animated-active animated) active)
  (setf (animated-frame animated) 0)
  (setf (animated-started animated) fc))

(defun animation-duration (sprite tag)
  "The duration in frame-count of a particular animation within a sprite."
  (->> sprite
    (sprite-animations)
    (gethash tag)
    (animation-frames)
    (t:transduce (t:map #'frame-duration-fs) #'+)))

(defun sprite-duration (sprite)
  "The duration of the sprite animation in terms of frame-count. Works best for
single-tag sprites."
  (t:transduce (t:comp (t:map #'cdr)
                       (t:map #'animation-frames)
                       #'t:concatenate
                       (t:map #'frame-duration-fs))
               #'+ (sprite-animations sprite)))

(defun bounding-box (animated)
  "Yield the `raylib:rectangle' of the first frame of the default animation. This
is assumed to be sufficient as a bounding box for collisions.

Note that Aseprite does not over-trim individual frames - each frame is given
the width of the widest one."
  (-<>> (animated-sprite animated)
    (sprite-animations)
    (gethash (animated-default animated))
    (animation-frames)
    (aref <> 0)
    (frame-rect)))

(defun draw-at-frame (texture animation pos frame &key (colour raylib:+white+) (flip? nil))
  "Draw a specific frame from a specific texture."
  (let* ((frames (animation-frames animation))
         (rect   (frame-rect (aref frames frame))))
    (if flip?
        (with-flipped-sprite rect
          (raylib:draw-texture-rec texture rect pos colour))
        (raylib:draw-texture-rec texture rect pos colour))))

(defun draw-animated (animated pos fc &key (colour raylib:+white+) (flip? nil))
  "Draw the given `animated' sprite at a certain position, using the current frame
count to determine how much time has passed. Each frame has a known max duration
which is used to calculate the time difference."
  (let* ((sprite    (animated-sprite animated))
         (active    (animated-active animated))
         (animation (gethash active (sprite-animations sprite)))
         (frames    (animation-frames animation))
         (texture   (sprite-texture sprite)))
    ;; Optimisation: When we know there is only one frame to the current
    ;; animation, we don't need to bother with time differences and frame
    ;; transitions. We just draw the one frame as-is.
    (cond ((= 1 (length frames))
           (draw-at-frame texture animation pos 0 :colour colour :flip? flip?))
          ;; Enough frames have passed in this particular animation frame, so we
          ;; must advance.
          ((>= (- fc (animated-started animated))
               (frame-duration-fs (aref frames (animated-frame animated))))
           (setf (animated-started animated) fc)
           ;; Animation looping.
           (if (= (animated-frame animated) (1- (length frames)))
               (setf (animated-frame animated) 0)
               (incf (animated-frame animated)))
           (draw-at-frame texture animation pos (animated-frame animated) :colour colour :flip? flip?))
          ;; This frame is still not over, so we draw as normal.
          (t (draw-at-frame texture animation pos (animated-frame animated) :colour colour :flip? flip?)))))
