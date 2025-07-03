;;; The player's current score, bomb count, etc.

(in-package :aero-slicer)

#++
(launch)

(defun draw-hud (game)
  "Draw the entire hud."
  (let* ((sprites (game-sprites game))
         (lives   (game-lives game))
         (bombs   (fighter-bombs (game-fighter game)))
         (beams   (->> game game-fighter fighter-beam beam-animated animated-sprite (beam-upgrade-count sprites)))
         (f-icon  (sprites-little-f sprites))
         (b-icon  (sprites-little-b sprites))
         (p-icon  (if (->> game game-fighter fighter-god-mode?)
                      (sprites-little-g sprites)
                      (sprites-little-p sprites)))
         (l-icon  (sprites-level sprites))
         (cool?   (not (can-bomb? (game-fighter game) (game-frame game)))))
    (draw-top-bar (sprites-hud sprites))
    (draw-lives f-icon lives)
    (draw-bombs b-icon +very-faded-white+ bombs cool?)
    (draw-beams p-icon beams)
    (draw-score game)
    (draw-levels l-icon (game-level game))))

(defun draw-score (game)
  "Draw the score."
  (let* ((sprite    (->> game game-sprites sprites-numbers))
         (texture   (sprite-texture sprite))
         (animation (gethash :numbers (sprite-animations sprite))))
    (t:transduce #'t:pass
                 (t:fold (lambda (acc place)
                           (if (zerop acc)
                               (t:reduced t)
                               (multiple-value-bind (next this) (floor acc 10)
                                 (setf (->> game game-score-pos raylib:vector2-x)
                                       (- +score-x+ (* place 4)))
                                 (draw-at-frame texture animation (game-score-pos game) this)
                                 next)))
                         (game-score game))
                 (t:ints 0))))

(defun draw-levels (sprite level)
  (t:transduce (t:comp (t:take level)
                       (t:map (lambda (n)
                                (draw-icon sprite
                                           (+ (* 4 n) +level-x+)
                                           +level-y+))))
               #'t:for-each (t:ints 0)))

(defun draw-lives (sprite lives)
  (when (> lives 2)
    (draw-icon sprite +life-3-x+ +icon-y+))
  (when (> lives 1)
    (draw-icon sprite +life-2-x+ +icon-y+))
  (when (> lives 0)
    (draw-icon sprite +life-1-x+ +icon-y+)))

(defun draw-bombs (sprite colour bombs cool?)
  (let ((colour (if cool? colour +white+)))
    (when (> bombs 2)
      (draw-icon sprite +ammo-3-x+ +icon-y+ :colour colour))
    (when (> bombs 1)
      (draw-icon sprite +ammo-2-x+ +icon-y+ :colour colour))
    (when (> bombs 0)
      (draw-icon sprite +ammo-1-x+ +icon-y+ :colour colour))))

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
  (raylib:draw-texture (sprite-texture sprite) +world-min-x+ +world-min-y+ +white+))

(defun draw-icon (sprite x y &key (colour +white+))
  "Representing the number of player lives or bombs remaining."
  (raylib:draw-texture (sprite-texture sprite) x y colour))
