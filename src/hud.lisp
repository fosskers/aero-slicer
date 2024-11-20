;;; The player's current score, bomb count, etc.

(in-package :aero-fighter)

#++
(launch)

(defun draw-hud (game)
  "Draw the entire hud."
  (let* ((sprites (game-sprites game))
         (lives   (game-lives game))
         (bombs   (fighter-bombs (game-fighter game)))
         (f-icon  (sprites-little-f sprites))
         (b-icon  (sprites-little-b sprites)))
    (draw-top-bar (sprites-hud sprites))
    (when (> lives 2)
      (draw-icon f-icon +life-3-x+ +icon-y+))
    (when (> lives 1)
      (draw-icon f-icon +life-2-x+ +icon-y+))
    (when (> lives 0)
      (draw-icon f-icon +life-1-x+ +icon-y+))
    (when (> bombs 2)
      (draw-icon b-icon +ammo-3-x+ +icon-y+))
    (when (> bombs 1)
      (draw-icon b-icon +ammo-2-x+ +icon-y+))
    (when (> bombs 0)
      (draw-icon b-icon +ammo-1-x+ +icon-y+))))

(defun draw-top-bar (sprite)
  "Draw the top bar. This isn't a defmethod on `draw' because it doesn't need to be."
  (raylib:draw-texture (sprite-texture sprite) +world-min-x+ +world-min-y+ raylib:+white+))

(defun draw-icon (sprite x y)
  "Representing the number of player lives or bombs remaining."
  (raylib:draw-texture (sprite-texture sprite) x y raylib:+white+))
