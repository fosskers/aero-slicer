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
  (t:transduce (t:comp (t:map #'cdr)
                       (t:filter (lambda (enemy) (near? fighter enemy))))
               (t:anyp (lambda (enemy) (colliding? fighter enemy))) enemies-ht))

(defun enemies-hit-by-beam (beam enemies-ht)
  "Find all enemies that the beam is hitting."
  (t:transduce (t:filter (lambda (enemy) (colliding? beam (cdr enemy))))
               #'t:cons enemies-ht))

(defun colliding-entity (fighter entity-ht)
  "Which entity, if any, is colliding with the fighter?"
  (t:transduce (t:filter (lambda (entity) (near? fighter (cdr entity))))
               (t:find (lambda (entity) (colliding? fighter (cdr entity))))
               entity-ht))

(defun direct-collision! (fighter enemies-ht)
  "If there is a direct, physical collision, also kill the enemy. Yields the
position of the killed enemy."
  (when-let* ((enemy (colliding-entity fighter enemies-ht)))
    (remhash (car enemy) enemies-ht)
    (pos (cdr enemy))))
