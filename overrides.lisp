;;; overrides.lisp --- preferred functionality for StumpWM

;; Copyright © 2020-2021 James McCabe

;; Author: James McCabe <james.mccab3(at)gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file changes some default behavior of StumpWM.

;;; Code:

;;; Colors

;; overrides StumpWM default behavior of dimming normal colors
(defun update-color-map (screen)
  "Read *colors* and cache their pixel colors for use when rendering colored text."
  (labels ((map-colors (amt)
	     (loop for c in *colors*
		   as color = (lookup-color screen c)
		   do (adjust-color color amt)
		   collect (alloc-color screen color))))
    (setf (screen-color-map-normal screen) (apply #'vector (map-colors 0.00)))))

(update-color-map (current-screen))

;; fix colors in quit message
(defcommand quit-confirm () ()
  "Prompt the user to confirm quitting StumpWM."
  (if (y-or-n-p (format nil "~@{~a~^~%~}"
			"You are about to quit the window manager to TTY."
			"Really ^2quit^n ^4StumpWM^n?"
			"^5Confirm?^n "))
      (quit)
      (xlib:unmap-window (screen-message-window (current-screen)))))

;;; Splits

;; StumpWM by default treats horizontal and vertical splits as Emacs does.
;; Horizontal splits the current frame into 2 side-by-side frames and Vertical
;; splits the current frame into 2 frames, one on top of the other.
;; I reverse this behavior in my configuration.

(defcommand (vsplit tile-group) (&optional (ratio "1/2")) (:string)
  "Split the current frame into 2 side-by-side frames."
  (split-frame-in-dir (current-group) :column (read-from-string ratio)))

(defcommand (hsplit tile-group) (&optional (ratio "1/2")) (:string)
  "Split the current frame into 2 frames, one on top of the other."
  (split-frame-in-dir (current-group) :row (read-from-string ratio)))

(undefine-key *tile-group-root-map* (kbd "S"))
(undefine-key *tile-group-root-map* (kbd "s"))
(define-key *root-map* (kbd "S") "vsplit")
(define-key *root-map* (kbd "s") "hsplit")

(defcommand (vsplit-equally tile-group) (amt)
  ((:number "Enter the number of frames: "))
  "Split current frame in n columns of equal size."
  (split-frame-eql-parts (current-group) :column amt))

(defcommand (hsplit-equally tile-group) (amt)
  ((:number "Enter the number of frames: "))
  "Split current frame in n rows of equal size."
  (split-frame-eql-parts (current-group) :row amt))

;;; overrides.lisp ends here
