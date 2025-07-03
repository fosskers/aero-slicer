;;; Interaction from the player.

(in-package :aero-slicer)

#+nil
(launch)

#+nil
*game*

(defun pressing-start? ()
  "Is the player pressed a start-like button?"
  (or (raylib:is-key-down +key-space+)
      (raylib:is-gamepad-button-down *gamepad* +gamepad-start+)))

(defun warp-button-down? ()
  "Is the warp trigger being held down?"
  (or (raylib:is-key-down +key-tab+)
      (raylib:is-gamepad-button-down *gamepad* +gamepad-y+)
      (raylib:is-gamepad-button-down *gamepad* +gamepad-x+)
      (raylib:is-gamepad-button-down *gamepad* +gamepad-left-shoulder+)
      (raylib:is-gamepad-button-down *gamepad* +gamepad-right-shoulder+)))

(defun warp-direction ()
  "The direction the player intends to warp."
  (cond ((or (raylib:is-key-down +key-up+)
             (raylib:is-gamepad-button-down *gamepad* +gamepad-up+))
         :up)
        ((or (raylib:is-key-down +key-down+)
             (raylib:is-gamepad-button-down *gamepad* +gamepad-down+))
         :down)
        ((or (raylib:is-key-down +key-left+)
             (raylib:is-gamepad-button-down *gamepad* +gamepad-left+))
         :left)
        ((or (raylib:is-key-down +key-right+)
             (raylib:is-gamepad-button-down *gamepad* +gamepad-right+))
         :right)
        (t :up)))

(defun trying-to-shoot? ()
  "Is the player trying to shoot?"
  (or (raylib:is-key-pressed +key-space+)
      ;; Doesn't crash if the gamepad isn't plugged in.
      (raylib:is-gamepad-button-pressed *gamepad* +gamepad-a+)))

;; --- Gamepads --- ;;

(defun gamepads ()
  "The current gamepads detected by the system."
  (t:transduce (t:take-while #'raylib:is-gamepad-available)
               #'t:cons (t:ints 0)))

#++
(gamepads)

(defun find-gamepads! (game)
  "Keep track of all available gamepads."
  (setf (game-gamepads game) (gamepads)))

(defun switch-gamepad? (game)
  "Is the user requesting a gamepad switch?"
  (t:transduce #'t:pass
               (t:anyp (lambda (pad) (raylib:is-gamepad-button-pressed pad +gamepad-select+)))
               (game-gamepads game)))

(defun set-gamepad! (n)
  "Set the current in-use gamepad."
  (setf *gamepad* n))

(defun auto-set-gamepad! (game)
  "Just pick one."
  (set-gamepad! (or (car (last (game-gamepads game))) 0)))

(defun detect-axes! (game)
  "Determine if the current gamepad has directional Sticks."
  (when (not (zerop (raylib:get-gamepad-axis-count *gamepad*)))
    (setf (game-gamepad-axes? game) t)))

(defun axes-yields ()
  "Gamepad axis values clamped to a minimum sensitivity."
  (values (raylib:get-gamepad-axis-movement *gamepad* 1)
          (raylib:get-gamepad-axis-movement *gamepad* 0)))

;; --- Debugging --- ;;

#++
(defun debugging-gamepad ()
  (let ((last-pressed (raylib:get-gamepad-button-pressed)))
    ;; (when (not (zerop last-pressed))
    (break (format nil "Button: ~a" last-pressed))))

#+nil
(debugging-gamepad)

#++
(defun debugging-keypress ()
  "Print the key currently being pressed."
  (let ((key (raylib:get-key-pressed)))
    (when (and key (not (zerop key)))
      (break (format nil "KEY: ~a" key))
      #+nil
      (raylib:draw-text (format nil "KEY: ~a" key)
                        (- +screen-width+ 75)
                        (- +screen-height+ 25)
                        20
                        raylib:+lightgray+))))

#+nil
(raylib:get-gamepad-axis-count 2)
