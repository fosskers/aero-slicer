;;; Detection of collision between the fighter and various enemies and obstacles.
;;;
;;; We do this in two stages: a general search for things near to each other,
;;; and then a specific phase via Raylib to determine if their
;;; `raylib:rectangle' actually overlap.

(in-package :aero-fighter)

#+nil
(launch)

;; --- Types --- ;;

(defstruct rect-pos
  "A simple pair for collision detection when a more complex entity has yet to be created."
  (pos  nil :type raylib:vector2)
  (rect nil :type raylib:rectangle))

(defmethod pos ((rect-pos rect-pos))
  (rect-pos-pos rect-pos))

(defmethod bbox ((rect-pos rect-pos))
  (rect-pos-rect rect-pos))

;; --- Abstract --- ;;

(defgeneric near? (a b)
  (:documentation "Are two sprites in the same general vicinity?"))

(defmethod near? ((fighter fighter) (cannon cannon))
  "The cannon spans the whole screen width, so the generic nearness consideration
doesn't work."
  (let ((distance (abs (- (->> fighter fighter-pos raylib:vector2-y)
                          (->> cannon cannon-beam cannon-beam-pos raylib:vector2-y)))))
    (< distance 16)))

(defmethod near? (a b)
  "Generic case: Test by Euclidean Distance."
  (let ((distance (euclidean-distance (pos a) (pos b))))
    (< distance +nearness-radius+)))

(defun colliding? (a b)
  "Are two near sprites actually colliding?"
  (raylib:check-collision-recs (bbox a) (bbox b)))

(defun near-entity-collision? (thing entities)
  "Is some `thing' colliding with any of a collection of `entities'? For
performance, only checks things that are detected to be already close."
  (t:transduce (t:filter (lambda (entity) (near? thing (cdr entity))))
               (t:anyp (lambda (entity) (colliding? thing (cdr entity))))
               entities))

(defun any-entity-collision? (thing entities)
  "Is some `thing' colliding with any of a collection of `entities'? Checks the
entire Hash Table of the given entities, so may not be performant if large."
  (t:transduce #'t:pass
               (t:anyp (lambda (entity) (colliding? thing (cdr entity))))
               entities))

;; --- Specific --- ;;

(defun enemies-hit-by-beam (beam enemies-ht)
  "Find all enemies that the beam is hitting."
  (t:transduce (t:filter (lambda (enemy) (colliding? beam (cdr enemy))))
               #'t:snoc enemies-ht))

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

(defun got-shot? (fighter enemies-ht)
  "Was the fighter shot by an enemy?"
  (t:transduce #'t:pass
               (t:anyp (lambda (pair)
                         (let ((beam (beam (cdr pair))))
                           (and (beam-shooting? beam)
                                (colliding? fighter beam)))))
               enemies-ht))
