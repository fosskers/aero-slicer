;;; The mutable state of the running game.
;;;
;;; Calling `game' initialises it, and likewise `ungame' unloads various
;;; textures that had been loaded into the GPU by Raylib.

(in-package :aero-fighter)

#+nil
(launch)

;; NOTE: When you add a texture here, make sure to unload it in `ungame' below.
(defstruct (sprites (:constructor sprites))
  "A bank of various sprites and their loaded textures."
  (fighter  (sprite #p"assets/fighter.json"))
  (beam     (sprite #p"assets/beam.json"))
  (blob     (sprite #p"assets/blob.json"))
  (tank     (sprite #p"assets/tank.json"))
  (building (sprite #p"assets/building.json"))
  (bomb     (sprite #p"assets/bomb.json")))

;; FIXME: 2024-11-07 Can the hash tables for the blobs and tanks be merged?
(defstruct game
  "The state of the running game."
  (camera  (camera) :type raylib:camera-2d)
  (sprites nil :type sprites)
  (fighter nil :type fighter)
  ;; The key is the frame number upon which the blob was spawned.
  (blobs   (make-hash-table :size 16) :type hash-table)
  (tanks   (make-hash-table :size 16) :type hash-table)
  (buildings (make-hash-table :size 16) :type hash-table)
  (powerups (make-hash-table :size 16) :type hash-table)
  (frame   0 :type fixnum)
  (lives   3 :type fixnum)
  ;; Waiting / Playing / Dead
  (mode    'playing :type symbol))

(defun game ()
  "Initialise the various game resources."
  (let ((sprites (sprites)))
    (make-game :sprites sprites
               :fighter (fighter (sprites-fighter sprites)
                                 (sprites-beam sprites)))))

(defun reset-game! (game)
  "Reset the `game' to an initial, reusable state."
  (setf (game-lives game) 3)
  (clear-all-enemies! game)
  (setf (game-buildings game) (make-hash-table :size 16))
  (setf (game-powerups game) (make-hash-table :size 16))
  (setf (game-mode game) 'playing))

(defun clear-all-enemies! (game)
  "From a bomb or otherwise, clear all the damageable enemies."
  (setf (game-blobs game) (make-hash-table :size 16))
  (setf (game-tanks game) (make-hash-table :size 16)))

(defun camera ()
  "Initialise a 2D Camera."
  (let* ((center-x (/ +screen-width+ 2.0))
         (center-y (/ +screen-height+ 2.0))
         (offset   (raylib:make-vector2 :x center-x :y center-y))
         (target   (raylib:make-vector2 :x 0.0 :y 0.0)))
    ;; TODO: Restore to Zoom 3 once testing is done.
    (raylib:make-camera-2d :offset offset :target target :rotation 0.0 :zoom 2.0)))

(defun ungame (game)
  "Release various resources."
  (let ((sprites (game-sprites game)))
    (raylib:unload-texture (sprite-texture (sprites-fighter sprites)))
    (raylib:unload-texture (sprite-texture (sprites-beam sprites)))
    (raylib:unload-texture (sprite-texture (sprites-blob sprites)))
    (raylib:unload-texture (sprite-texture (sprites-tank sprites)))
    (raylib:unload-texture (sprite-texture (sprites-building sprites)))
    (raylib:unload-texture (sprite-texture (sprites-bomb sprites)))))
