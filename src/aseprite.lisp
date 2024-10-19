(in-package :aero-fighter)

;; Requirements:
;;
;; - Know the location of the sprite sheet.
;; - Know the distinct animations from the available `frameTags'.
;; - Each frame knows its own rectangle dimensions within the sheet.
;; - Each frame knows its own duration.
;; - A command to play the same animation as the current one should _not_ reset the animation.
;; - A completed animation should return to the default/first one.

(defstruct frame
  "A single animation frame."
  (rect nil)
  (duration 0 :type fixnum))

(defstruct animation
  "One or more frames of an animation."
  (frames nil :type vector))

(defstruct sprite
  "An animated sprite."
  (texture    nil)
  (animations nil :type hash-table)
  (active     nil :type symbol))

;; TODO: use `filepaths' to produce the correct path to the PNG.

#+nil
(let* ((json (jzon:parse #p"assets/fighter.json"))
       (meta (gethash "meta" json))
       (path (gethash "image" meta))
       (frames (gethash "frames" json))))
