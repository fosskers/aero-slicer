;;; The mutable state of the running game.
;;;
;;; Calling `game' initialises it, and likewise `ungame' unloads various
;;; textures that had been loaded into the GPU by Raylib.

(in-package :aero-fighter)

#+nil
(launch)

;; NOTE: When you add a texture here, make sure to unload it in `ungame' below.
;; NOTE: When you add a new beam width, account for it in the hash table in `game'.
(defstruct (sprites (:constructor sprites))
  "A bank of various sprites and their loaded textures."
  (fighter  (sprite #p"assets/fighter.json"))
  (beam-2   (sprite #p"assets/beam-2.json"))
  (beam-4   (sprite #p"assets/beam-4.json"))
  (beam-6   (sprite #p"assets/beam-6.json"))
  (beam-8   (sprite #p"assets/beam-8.json"))
  (beam-10  (sprite #p"assets/beam-10.json"))
  (beam-12  (sprite #p"assets/beam-12.json"))
  (beam-14  (sprite #p"assets/beam-14.json"))
  (beam-16  (sprite #p"assets/beam-16.json"))
  (beam-18  (sprite #p"assets/beam-18.json"))
  (blob     (sprite #p"assets/blob.json"))
  (tank     (sprite #p"assets/tank.json"))
  (building (sprite #p"assets/building.json"))
  (evil-ship (sprite #p"assets/evil-fighter.json"))
  (bomb     (sprite #p"assets/bomb.json"))
  (wide     (sprite #p"assets/wide-laser.json"))
  (explosion (sprite #p"assets/explosion.json")))

;; FIXME: 2024-11-07 Can the hash tables for the blobs and tanks be merged?
(defstruct game
  "The state of the running game."
  (camera  (camera) :type raylib:camera-2d)
  (sprites nil :type sprites)
  (fighter nil :type fighter)
  ;; For use in determining the next width of beam when a beam powerup has been
  ;; collected.
  (beams   nil :type hash-table)
  ;; The point after which the next Beam Widener powerup should spawn.
  (widener-threshold 1000 :type fixnum)
  ;; The key is the frame number upon which the blob was spawned.
  (blobs   (make-hash-table :size 16) :type hash-table)
  (tanks   (make-hash-table :size 16) :type hash-table)
  (evil-ships (make-hash-table :size 16) :type hash-table)
  (buildings (make-hash-table :size 16) :type hash-table)
  (powerups (make-hash-table :size 16) :type hash-table)
  ;; TODO: 2024-11-14 Consider generalising this to any other on-screen
  ;; animations.
  (explosions (make-hash-table :size 16) :type hash-table)
  (frame   0 :type fixnum)
  (lives   3 :type fixnum)
  (score   0 :type fixnum)
  ;; Waiting / Playing / Dead
  (mode    'playing :type symbol)
  (level   1 :type fixnum)
  ;; The point after which the level should increase.
  (level-thresh +level-progression-interval+ :type fixnum))

(defun game ()
  "Initialise the various game resources."
  (let ((sprites (sprites))
        (beams   (make-hash-table)))
    (setf (gethash (sprites-beam-2 sprites) beams) (sprites-beam-4 sprites))
    (setf (gethash (sprites-beam-4 sprites) beams) (sprites-beam-6 sprites))
    (setf (gethash (sprites-beam-6 sprites) beams) (sprites-beam-8 sprites))
    (setf (gethash (sprites-beam-8 sprites) beams) (sprites-beam-10 sprites))
    (setf (gethash (sprites-beam-10 sprites) beams) (sprites-beam-12 sprites))
    (setf (gethash (sprites-beam-12 sprites) beams) (sprites-beam-14 sprites))
    (setf (gethash (sprites-beam-14 sprites) beams) (sprites-beam-16 sprites))
    (setf (gethash (sprites-beam-16 sprites) beams) (sprites-beam-18 sprites))
    (make-game :sprites sprites
               :beams beams
               :fighter (fighter (sprites-fighter sprites)
                                 (sprites-beam-2 sprites)))))

;; TODO: 2024-11-12 Should I just reconstruct the `game' entirely instead of
;; doing all this manual resetting?
;;
;; Disdvantage: it would reread all the sprite data, reset the camera, and reset
;; the current frame number.
(defun reset-game! (game)
  "Reset the `game' to an initial, reusable state."
  (setf (game-lives game) 3)
  (clear-all-enemies! game)
  (setf (game-buildings game) (make-hash-table :size 16))
  (setf (game-powerups game) (make-hash-table :size 16))
  (setf (game-explosions game) (make-hash-table :size 16))
  (setf (game-score game) 0)
  (setf (game-level game) 1)
  (setf (game-mode game) 'playing)
  (setf (game-widener-threshold game) 1000)
  (let ((fighter (game-fighter game)))
    (setf (fighter-bombs fighter) 3)))

(defun clear-all-enemies! (game)
  "From a bomb or otherwise, clear all the damageable enemies."
  (setf (game-blobs game) (make-hash-table :size 16))
  (setf (game-tanks game) (make-hash-table :size 16))
  (setf (game-evil-ships game) (make-hash-table :size 16)))

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
    (raylib:unload-texture (sprite-texture (sprites-beam-2 sprites)))
    (raylib:unload-texture (sprite-texture (sprites-beam-4 sprites)))
    (raylib:unload-texture (sprite-texture (sprites-beam-6 sprites)))
    (raylib:unload-texture (sprite-texture (sprites-beam-8 sprites)))
    (raylib:unload-texture (sprite-texture (sprites-beam-10 sprites)))
    (raylib:unload-texture (sprite-texture (sprites-beam-12 sprites)))
    (raylib:unload-texture (sprite-texture (sprites-beam-14 sprites)))
    (raylib:unload-texture (sprite-texture (sprites-beam-16 sprites)))
    (raylib:unload-texture (sprite-texture (sprites-beam-18 sprites)))
    (raylib:unload-texture (sprite-texture (sprites-blob sprites)))
    (raylib:unload-texture (sprite-texture (sprites-tank sprites)))
    (raylib:unload-texture (sprite-texture (sprites-building sprites)))
    (raylib:unload-texture (sprite-texture (sprites-evil-ship sprites)))
    (raylib:unload-texture (sprite-texture (sprites-bomb sprites)))
    (raylib:unload-texture (sprite-texture (sprites-wide sprites)))
    (raylib:unload-texture (sprite-texture (sprites-explosion sprites)))))
