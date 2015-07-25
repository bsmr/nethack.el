;;; nethack-nhlaunch.el --- Negotiate with nhlaunch to start a game of nethack

;; Copyright (C) 2005 Shawn Betts

;; Author: Shawn Betts <sabetts@vcn.bc.ca>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

(require 'nethack)

(defvar nethack-network-server "sputnik.emmett.ca"
  "The user to login as")

(defvar nethack-network-port 23
  "The user to login as")

(defvar nethack-network-user (user-login-name)
  "The user to login as")

(defvar nethack-network-password nil
  "The password to use")

(defvar nethack-network-game "nethack"
  "The game to play")

(defvar nh-network-password nil)
(defvar nh-network-game nil)

(defun nh-network-filter (proc str)
  (cond ((or (string-equal str "Welcome to the nethack-el server.\n")
	     (string-equal str (format "User %s added successfully.\n" nethack-network-user)))
	 (process-send-string proc (format "login %s %s\n" nethack-network-user nh-network-password)))
	((string-equal str (format "Welcome back %s.\n" nethack-network-user))
	 (message "Starting nethack...")
	 (process-send-string proc (format "play %s\n" nh-network-game))
	 (nethack-start proc))
	((or (string-equal str (format "Failed to login %s.\n" nethack-network-user))
	     (string-equal str "Error parsing name and password.\n"))
	 (delete-process proc)
	 (message str))
	((string-equal str (format "Unknown user %s.\n" nethack-network-user))
	 (process-send-string proc (format "new %s %s\n" nethack-network-user nh-network-password)))))

;;;###autoload
(defun nethack-connect-to-server (&optional prefix)
  "Connect to the nethack server specified by `nethack-network-server'
`nethack-network-port' `nethack-network-user' `nethack-network-passwd'
and `nethack-network-game'. When called with a universal arg, it
prompts for the game."
  (interactive "P")
  (if (nethack-is-running)
      (message "Nethack process already running...")
    (if (get-buffer nh-proc-buffer-name)
	(kill-buffer nh-proc-buffer-name))
    (setq nh-network-password (or nethack-network-password
				  (read-from-minibuffer "Password: ")))
    (setq nh-network-game (if prefix
			      (read-from-minibuffer "Game: ")
			    nethack-network-game))
    (message nh-network-game)
    (let ((proc (open-network-stream "nh" nh-proc-buffer-name
				     nethack-network-server
				     nethack-network-port)))
      (set-process-filter proc 'nh-network-filter))))
  
(provide 'nethack-nhlaunch)
;;; nethack-nhlaunch.el ends here
