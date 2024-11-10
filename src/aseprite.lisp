;;; Animated sprites, as pulled from an Aseprite sprite sheet.

(in-package :aero-fighter)

;; Requirements:
;;
;; - A command to play the same animation as the current one should _not_ reset the animation.
;; - A completed animation should return to the default/first one.

#+nil
(launch)

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

(defstruct animated
  "An actual animated sprite that tracks its own state. Should be one per living
entity in the game, but all instances of the same entity type should share an
underlying `sprite' definition."
  (sprite  nil   :type sprite :read-only t)
  (default 'idle :type symbol)
  (active  'idle :type symbol)
  (frame   0     :type fixnum)
  (started 0     :type fixnum))

(defun json->frame (json)
  "Read a `frame' out of some JSON."
  (let ((dim    (gethash "frame" json))
        (millis (gethash "duration" json)))
    (make-frame :duration-ms millis
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
    (cons (intern (string-upcase (gethash "name" json)))
          (make-animation :frames frs))))

(defun sprite (path)
  "Read animated sprite data from the given JSON file. Must be called after a GL
context has been initialised via `raylib:init-window'."
  (let* ((json   (jzon:parse path))
         (meta   (gethash "meta" json))
         (texture (raylib:load-texture (p:to-string (p:with-name path (gethash "image" meta)))))
         (frames (gethash "frames" json))
         (tags   (gethash "frameTags" meta))
         (anims  (t:transduce (t:map (lambda (tag) (json->animation frames tag))) #'t:hash-table tags)))
    (make-sprite :texture texture
                 :animations anims)))

(defun bounding-box (animated)
  "Yield the `raylib:rectangle' of the first frame of the default animation. This
is assumed to be sufficient as a bounding box for collisions.

Note that Aseprite does not over-trim individual frames - each frame is given
the width of the widest one."
  (frame-rect (aref (animation-frames (gethash (animated-default animated)
                                               (sprite-animations (animated-sprite animated))))
                    0)))

(defun draw-animated (animated pos fc)
  "Draw the given `animated' sprite at a certain position, using the current frame
count to determine how much time has passed. Each frame has a known max duration
which is used to calculate the time difference."
  (let* ((sprite    (animated-sprite animated))
         (active    (animated-active animated))
         (animation (gethash active (sprite-animations sprite)))
         (frames    (animation-frames animation)))
    ;; Optimisation: When we know there is only one frame to the current
    ;; animation, we don't need to bother with time differences and frame
    ;; transitions. We just draw the one frame as-is.
    (cond ((= 1 (length frames))
           (raylib:draw-texture-rec (sprite-texture sprite)
                                    (frame-rect (aref frames 0))
                                    pos
                                    raylib:+white+))
          ;; Enough frames have passed in this particular animation frame, so we must advance.
          ((> (- fc (animated-started animated))
              (frame-duration-fs (aref frames (animated-frame animated))))
           (setf (animated-started animated) fc)
           (if (= (animated-frame animated) (1- (length frames)))
               (setf (animated-frame animated) 0)
               (incf (animated-frame animated)))
           (raylib:draw-texture-rec (sprite-texture sprite)
                                    (frame-rect (aref frames (animated-frame animated)))
                                    pos
                                    raylib:+white+))
          ;; This frame is still not over, so we draw as normal.
          (t (raylib:draw-texture-rec (sprite-texture sprite)
                                      (frame-rect (aref frames (animated-frame animated)))
                                      pos
                                      raylib:+white+)))))
