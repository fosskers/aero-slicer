;;; Interaction from the player.

(in-package :aero-fighter)

#+nil
(launch)

(defun pressing-start? ()
  "Is the player pressed a start-like button?"
  (or (raylib:is-key-down +key-space+)
      (raylib:is-gamepad-button-down +gamepad+ +gamepad-start+)))

(defun warp-button-down? ()
  "Is the warp trigger being held down?"
  (or (raylib:is-key-down +key-tab+)
      (raylib:is-gamepad-button-down +gamepad+ +gamepad-left-shoulder+)
      (raylib:is-gamepad-button-down +gamepad+ +gamepad-right-shoulder+)))

(defun warp-direction ()
  "The direction the player intends to warp."
  (cond ((or (raylib:is-key-down +key-up+)
             (raylib:is-gamepad-button-down +gamepad+ +gamepad-up+))
         'up)
        ((or (raylib:is-key-down +key-down+)
             (raylib:is-gamepad-button-down +gamepad+ +gamepad-down+))
         'down)
        ((or (raylib:is-key-down +key-left+)
             (raylib:is-gamepad-button-down +gamepad+ +gamepad-left+))
         'left)
        ((or (raylib:is-key-down +key-right+)
             (raylib:is-gamepad-button-down +gamepad+ +gamepad-right+))
         'right)
        (t nil)))

(defun trying-to-shoot? ()
  "Is the player trying to shoot?"
  (or (raylib:is-key-pressed +key-space+)
      ;; Doesn't crash if the gamepad isn't plugged in.
      (raylib:is-gamepad-button-pressed +gamepad+ +gamepad-a+)))

;; --- Gamepads --- ;;

(defun set-gamepad! ()
  "Set the global gamepad number to a known, good value."
  (let ((gamepad (->> (gamepads) car car)))
    (if gamepad
        (setf +gamepad+ gamepad)
        (setf +gamepad+ 0))))

(defun gamepads ()
  "The current gamepads detected by the system."
  (t:transduce (t:comp (t:take-while #'raylib:is-gamepad-available)
                       (t:map #'raylib:get-gamepad-name)
                       #'t:enumerate)
               #'t:snoc (t:ints 0)))

#++
(gamepads)

;; --- Debugging --- ;;

(defun debugging-gamepad ()
  (let ((last-pressed (raylib:get-gamepad-button-pressed)))
    (when (not (zerop last-pressed))
      (break (format nil "Button: ~a" last-pressed)))))

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
