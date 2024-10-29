;;; Detection of collision between the fighter and various enemies and obstacles.
;;;
;;; We do this in two stages: a general search for things near to each other,
;;; and then a specific phase via Raylib to determine if their
;;; `raylib:rectangle' actually overlap.

(in-package :aero-fighter)

#+nil
(launch)

;; --- Abstract --- ;;

(defun euclidean-distance (v1 v2)
  "The distance between two `raylib:vector2'."
  (sqrt (+ (expt (- (raylib:vector2-x v2)
                    (raylib:vector2-x v1))
                 2)
           (expt (- (raylib:vector2-y v2)
                    (raylib:vector2-y v1))
                 2))))

#+nil
(let ((a (raylib:make-vector2 :x 2.0 :y -6.0))
      (b (raylib:make-vector2 :x 7.0 :y 3.0)))
  (euclidean-distance a b))

(defun near? (a b)
  "Are two sprites in the same general vicinity?"
  (let ((distance (euclidean-distance (pos a) (pos b))))
    (< distance +nearness-radius+)))

(defun colliding? (a b)
  "Are two near sprites actually colliding?"
  (raylib:check-collision-recs (bbox a) (bbox b)))

;; --- Specific --- ;;

(defun blob-collision (fighter blobs fc)
  "If the fighter is colliding with any blob, make him flash."
  (let ((blob (t:transduce (t:comp (t:map #'cdr)
                                   (t:filter (lambda (blob) (near? fighter blob)))
                                   (t:filter (lambda (blob) (colliding? fighter blob))))
                           (first-or nil) blobs)))
    (when (and blob (eq 'ok (fighter-status fighter)))
      (damage-fighter fighter fc))))
