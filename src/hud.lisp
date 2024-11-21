;;; The player's current score, bomb count, etc.

(in-package :aero-fighter)

#++
(launch)

(defun draw-hud (game)
  "Draw the entire hud."
  (let* ((sprites (game-sprites game))
         (lives   (game-lives game))
         (bombs   (fighter-bombs (game-fighter game)))
         (beams   (beam-upgrade-count sprites (animated-sprite (beam-animated (fighter-beam (game-fighter game))))))
         (f-icon  (sprites-little-f sprites))
         (b-icon  (sprites-little-b sprites))
         (p-icon  (sprites-little-p sprites)))
    (draw-top-bar (sprites-hud sprites))
    (draw-lives f-icon lives)
    (draw-bombs b-icon bombs)
    (draw-beams p-icon beams)))

(defun draw-lives (sprite lives)
  (when (> lives 2)
    (draw-icon sprite +life-3-x+ +icon-y+))
  (when (> lives 1)
    (draw-icon sprite +life-2-x+ +icon-y+))
  (when (> lives 0)
    (draw-icon sprite +life-1-x+ +icon-y+)))

(defun draw-bombs (sprite bombs)
  (when (> bombs 2)
    (draw-icon sprite +ammo-3-x+ +icon-y+))
  (when (> bombs 1)
    (draw-icon sprite +ammo-2-x+ +icon-y+))
  (when (> bombs 0)
    (draw-icon sprite +ammo-1-x+ +icon-y+)))

(defun draw-beams (sprite beams)
  (when (> beams 7)
    (draw-icon sprite +beam-8-x+ +mini-icon-y+))
  (when (> beams 6)
    (draw-icon sprite +beam-7-x+ +mini-icon-y+))
  (when (> beams 5)
    (draw-icon sprite +beam-6-x+ +mini-icon-y+))
  (when (> beams 4)
    (draw-icon sprite +beam-5-x+ +mini-icon-y+))
  (when (> beams 3)
    (draw-icon sprite +beam-4-x+ +mini-icon-y+))
  (when (> beams 2)
    (draw-icon sprite +beam-3-x+ +mini-icon-y+))
  (when (> beams 1)
    (draw-icon sprite +beam-2-x+ +mini-icon-y+))
  (when (> beams 0)
    (draw-icon sprite +beam-1-x+ +mini-icon-y+)))

(defun draw-top-bar (sprite)
  "Draw the top bar. This isn't a defmethod on `draw' because it doesn't need to be."
  (raylib:draw-texture (sprite-texture sprite) +world-min-x+ +world-min-y+ raylib:+white+))

(defun draw-icon (sprite x y)
  "Representing the number of player lives or bombs remaining."
  (raylib:draw-texture (sprite-texture sprite) x y raylib:+white+))
