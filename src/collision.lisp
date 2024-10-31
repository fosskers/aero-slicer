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

(defun enemy-collision? (fighter enemies-ht)
  "Is the fighter colliding with any enemy?"
  (when (eq 'ok (fighter-status fighter))
    (t:transduce (t:comp (t:map #'cdr)
                         (t:filter (lambda (enemy) (near? fighter enemy))))
                 (t:anyp (lambda (enemy) (colliding? fighter enemy))) enemies-ht)))
