;;; Animated sprites, as pulled from an Aseprite sprite sheet.

(in-package :aero-fighter)

;; Requirements:
;;
;; - A command to play the same animation as the current one should _not_ reset the animation.
;; - A completed animation should return to the default/first one.

(defstruct frame
  "A single animation frame."
  (rect     nil :type raylib:rectangle)
  (duration 0   :type fixnum))

(defstruct animation
  "One or more frames of an animation."
  (frames nil :type vector))

(defstruct sprite
  "The base definition of an animated sprite. Should be created once and shared
around multiple instances of an `animated'."
  (texture    nil :type raylib:texture)
  (animations nil :type hash-table))

(defstruct (animated (:constructor animated))
  "An actual animated sprite that tracks its own state. Should be one per living
entity in the game, but all instances of the same entity type should share an
underlying `sprite' definition."
  (sprite  nil   :type sprite)
  (default 'idle :type symbol)
  (active  'idle :type symbol))

(defun json->frame (json)
  "Read a `frame' out of some JSON."
  (let ((dim (gethash "frame" json)))
    (make-frame :duration (gethash "duration" json)
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
    (cons (gethash "name" json)
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
