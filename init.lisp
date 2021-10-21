;;; init.lisp --- StumpWM demo configuration

;; Copyright © 2020-2021 James McCabe

;; Author: James McCabe <james.mccab3(at)gmail.com>
;; URL: https://github.com/jamesmccabe/stumpwm-demo-config

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

;; This is a basic StumpWM configuration for my demo video.

;;; Code:


(in-package :stumpwm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic settings                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load Stump contrib modules
(mapc #'load-module '("ttf-fonts"
		      "swm-gaps"))

;; set prefix key
(set-prefix-key (kbd "C-z"))

;; set desktop background color
(setf (xlib:window-background (screen-root (current-screen))) #x47456d)
;; set wallpaper
(run-shell-command "feh --bg-fill ~/Pictures/wallpaper.png")

;; font settings
(set-font (list (make-instance 'xft:font
			       :family "DejaVu Sans Mono"
			       :subfamily "Bold"
			       :size 13)
		(make-instance 'xft:font
			       :family "FontAwesome"
			       :subfamily "Regular"
			       :size 12)))

;; setup groups
(grename "Base")
(gnewbg "Extra")
(gnewbg-float "Float")
;(gnewbg ".scratchpad") ; hidden group / scratchpad

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Color settings                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; My own personal theme with random colors taken from
;; various Doom Emacs themes and also Dracula theme

(setf *colors*
      '("#ffffff"        ; ^0 ; White
	"#131220"        ; ^1 ; Dark Blue
	"#f72f33"        ; ^2 ; Red
	"#689d6a"        ; ^3 ; Light Green
	"#62bfef"        ; ^4 ; Light Blue
        "#fabd2f"        ; ^5 ; Yellow / Help map keys
	"#a644bf"        ; ^6 ; Magenta
	"#cc4a0e"        ; ^7 ; Brown
	"#56b6c2"))      ; ^8 ; Cyan  
	
(defparameter *mode-line-bg-color* (nth 1 *colors*))
(defparameter *mode-line-fg-color* (nth 0 *colors*))
(defparameter *msg-bg-color* (nth 1 *colors*))
(defparameter *msg-fg-color* (nth 0 *colors*))
(defparameter *msg-border-color* (nth 2 *colors*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General settings                                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mouse pointer
(run-shell-command "xsetroot -cursor_name left_ptr")

;; messages display time
(setf *timeout-wait* 7)

;; ignore window hints
(setf *ignore-wm-inc-hints* t)

;; window name format (truncate name after 20 letters)
(setf *window-format* "%m%n%s%20t")

;; input focus is transferred to the window you click on
(setf *mouse-focus-policy* :click)

;; message and input window location
(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)

;; message/input bar colors
(set-bg-color *msg-bg-color*)
(set-fg-color *msg-fg-color*)
(set-border-color *msg-border-color*)

;; message/input bar settings
(set-msg-border-width 3)
(setf *message-window-padding* 6)

;;; gaps settings
;; inner gaps run along all the 4 borders of a frame
(setf swm-gaps:*inner-gaps-size* 10)
;; outer gaps add more padding to the outermost borders
;; (touching the screen border)
(setf swm-gaps:*outer-gaps-size* 10)

;; startup message
(setf *startup-message* "^5    Stump Window Manager ^0has initialized!
Press ^2Ctrl+z ? ^0for Help. ^5Never Stop Hacking!^n
          Powered with ^02 Common Lisp ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Swank settings                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require :swank)
(swank-loader:init)

(defparameter *port-number* 4004
  "My default port number for Swank")

(defvar *swank-server-p* nil
  "Keep track of swank server, turned off by default on startup")

(defcommand start-swank () ()
  "Start Swank if it is not already running"
  (if *swank-server-p*
      (message "Swank server is already active on Port^5 ~a^n" *port-number*)
      (progn
	(swank:create-server :port *port-number*
			     :style swank:*communication-style*
			     :dont-close t)
	(setf *swank-server-p* t)
	(message "Swank server is now active on Port^5 ~a^n.
Use^4 M-x slime-connect^n in Emacs. 
Type^2 in-package :stumpwm^n in Slime REPL." *port-number*))))

(defcommand stop-swank () ()
  "Stop Swank"
  (swank:stop-server *port-number*)
  (setf *swank-server-p* nil)
  (message "Stopping Swank Server! Closing Port^5 ~a^n." *port-number*))

(defcommand toggle-swank () ()
  (if *swank-server-p*
      (run-commands "stop-swank")
      (run-commands "start-swank")))

(define-key *top-map* (kbd "s-s") "toggle-swank")

;; modeline status
(defun get-swank-status ()
  (if *swank-server-p*
      (setf *swank-ml-status* (format nil "Swank ^3^f1^f0^n Port:^5 ~a^n " *port-number*))
      (setf *swank-ml-status* "")))

(defun ml-fmt-swank-status (ml)
  (declare (ignore ml))
  (get-swank-status))

(add-screen-mode-line-formatter #\S #'ml-fmt-swank-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings                                                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; navigation
;; cycle forward and back through groups
(define-key *root-map* (kbd ".") "gnext")
(define-key *root-map* (kbd ",") "gprev")
(define-key *top-map* (kbd "s-Up") "gnext")
(define-key *top-map* (kbd "s-Down") "gprev")

;; cycle through windows using Super key + arrows
(define-key *top-map* (kbd "s-Right") "pull-hidden-next")
(define-key *top-map* (kbd "s-Left") "pull-hidden-previous")

;; send window to next/previous groups
(define-key *root-map* (kbd "s-Right") "gnext-with-window")
(define-key *root-map* (kbd "s-Left") "gprev-with-window")

(define-key *top-map* (kbd "s-SPC") "fnext")

;;others
;; run or raise firefox
(defcommand firefox () ()
  "Start Forefox or switch to it, if it is already running"
  (run-or-raise "firefox" '(:class "Firefox")))

(define-key *root-map* (kbd "b") "firefox")

;; open terminal
(define-key *root-map* (kbd "Return") "exec urxvt")
(define-key *root-map* (kbd "c") "exec urxvt")
(define-key *root-map* (kbd "C-c") "exec urxvt")

;; toggle useless gaps keybinding (Super + u)
(define-key *top-map* (kbd "s-u") "toggle-gaps")

;; hard restart keybinding (Super + r)
(define-key *top-map* (kbd "s-r") "restart-hard")

;; allows me to continously have control of Prefix key
;; by unmapping it from 'pull-hidden-other
(undefine-key *tile-group-root-map* (kbd "C-z"))
(define-key *root-map* (kbd "C-z") "abort")

;; take screenshot
(defcommand stump-screenshot () ()
  (run-shell-command "exec scrot")
  (sleep 0.5)
  (message "Screenshot taken!"))

(define-key *top-map* (kbd "Print") "stump-screenshot")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modeline settings                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *mode-line-timeout* 1)
(setf *mode-line-border-width* 0)

(setf *mode-line-background-color* *mode-line-bg-color*)
(setf *mode-line-border-color* *mode-line-bg-color*)
(setf *mode-line-foreground-color* *mode-line-fg-color*)

(setf *time-modeline-string* "^2^f1^f0^n %H:%M")

(defparameter *battery-percent* "")

(defun get-battery-status ()
  (let* ((batgetcap (run-shell-command "cat /sys/class/power_supply/BAT0/capacity | tr -d '\\r\\n'" t)))
    (setf *battery-percent* (format nil "^4^f1^f0^n ~a% " batgetcap))))

(defun battery-percentage (ml)
  (declare (ignore ml))
  *battery-percent*)
  
(run-with-timer 0 10 #'get-battery-status)
(add-screen-mode-line-formatter #\B #'battery-percentage)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *screen-mode-line-format*
      (list "^5[%g]^n "       ; groups
	    "%W"              ; windows
	    "^>"              ; right align
	    "%S"              ; swank status
	    "%B"              ; battery percentage
            "%d"))            ; time/date

;; turn on the mode line
(if (not (head-mode-line (current-head)))
    (toggle-mode-line (current-screen) (current-head)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overrides                                                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file when loaded overrides some Stump behavior
;; which are personal preferences I prefer to the defaults
(load "~/.stumpwm.d/overrides.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging                                                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(redirect-all-output (data-dir-file "debug-output" "txt"))
;(setf stumpwm:*debug-level* 10)

;(load "~/.stumpwm.d/test.lisp")

;;; init.lisp ends here
