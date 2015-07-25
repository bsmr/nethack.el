;;; nh-bot.el --- a bot that solves nethack

;; Copyright (C) 2005  Shawn Betts

;; Author: Shawn Betts <sabetts@vcn.bc.ca>
;; Keywords: games

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

;;; We need to redefine certain user input functions so the user isn't
;;; prompted for input. They're stubs that call the function in their
;;; corresponding bot-* variable. This way the robot can switch
;;; functions to respond differently based on its state.

(defvar bot-nhapi-ask-direction nil)
(defun nhapi-ask-direction (prompt)
  (nh-send (funcall bot-nhapi-ask-direction prompt)))

(defvar bot-nhapi-yn-function nil)
(defun nhapi-yn-function (ques choices default)
  (nh-send (funcall bot-nhapi-yn-function ques choices default)))

(defvar bot-nhapi-getlin nil)
(defun nhapi-getlin (ques)
  (nh-send (funcall nhapi-getlin)))

(defvar bot-nhapi-choose-attribute nil)
(defun nhapi-choose-attribute (prompt alist abort)
  (nh-send (funcall bot-nhapi-choose-attribute prompt alist abort)))

(defvar bot-nhapi-block nil)
(defun nhapi-block ()
  (funcall bot-nhapi-block)
  (nh-send 'block-dummy))

(defvar bot-nhapi-select-menu nil)
(defadvice nhapi-select-menu (after bot-select-advice (menuid how))
  (funcall bot-nhapi-select-menu menuid how))

;;; Random Walk

(defconst bot-nummons 382
  "The number of nethack monsters. Found in pm.h.")

(defvar bot-props (make-hash-table)
  "This puppy contains properties about the world.")

(defun bot-get-glyph-rel (x y)
  "Get the glyph at x,y relative to the hero."
  (save-excursion
    (let* ((p (text-property-any (point-min) (point-max) 'glyph 341))
	   hx hy)
      (goto-char p)
      (setq hx (- p (line-beginning-position))
	    hy (1- (count-lines (point-min) (point))))
      (get-text-property (gamegrid-cell-offset (+ hx x) (+ hy y)) 'glyph))))

(defun bot-prop-in-room ()
  "Determine if we're in a room. doorway: 2356, floor 2363, wall 2346 2345"
  (if (string= (first nh-status-attribute-Dungeon) "The Gnomish Mines")
      nil
    (and (find (bot-get-glyph-rel -1  0) '(2363 2356 2346 2345))
	 (find (bot-get-glyph-rel  1  0) '(2363 2356 2346 2345))
	 (find (bot-get-glyph-rel  0 -1) '(2363 2356 2346 2345))
	 (find (bot-get-glyph-rel  0  1) '(2363 2356 2346 2345)))))
    
(defun bot-gather-props ()
  "Called when we get a command prompt."
  )

(defun bot-explore (prompt)
  "Find a wall and start following it."
  (when (string= prompt "command")
    ()))

(defun random-walk (prompt)
  (when (string= prompt "command")
    (case (random 8)
      (0 (run-at-time 0.25 nil 'nethack-command-south))
      (1 (run-at-time 0.25 nil 'nethack-command-north))
      (2 (run-at-time 0.25 nil 'nethack-command-east))
      (3 (run-at-time 0.25 nil 'nethack-command-west))
      (4 (run-at-time 0.25 nil 'nethack-command-southwest))
      (5 (run-at-time 0.25 nil 'nethack-command-southeast))
      (6 (run-at-time 0.25 nil 'nethack-command-northeast))
      (7 (run-at-time 0.25 nil 'nethack-command-northwest)))))

;;; Character selection function chain.

(defun bot-pick-character (ques choices default)
  (prog1
      (if (string= ques "Shall I pick a character for you? [ynq] ")
	  ?n)
    (setq bot-nhapi-select-menu 'bot-select-role)))

(defun bot-select-role (menuid how)
  (let ((last-command-char ?v))
    (nh-menu-toggle-item))
  (setq bot-nhapi-select-menu 'bot-select-race))

(defun bot-select-race (menuid how)
  (let ((last-command-char ?h))
    (nh-menu-toggle-item))
  (setq bot-nhapi-select-menu 'bot-select-alignment))

(defun bot-select-alignment (menuid how)
  (let ((last-command-char ?l))
    (nh-menu-toggle-item))
  (setq bot-nhapi-select-menu '(lambda (m h) 
				 (nh-menu-submit)
				 (setq bot-nhapi-select-menu nil))))

(defun bot-start ()
  "Start a game of nethack as a valkyrie and walk around randomly."
  (interactive)
  (ad-activate 'nhapi-select-menu)
  ;; Start the function chain that selects a character.
  (setq bot-nhapi-yn-function 'bot-pick-character)
  (add-hook 'nh-at-prompt-hook 'random-walk)
  (nethack))

(defun bot-stop ()
  (interactive)
  (remove-hook 'nh-at-prompt-hook 'random-walk))

(provide 'nh-bot)
;;; nh-bot.el ends here
