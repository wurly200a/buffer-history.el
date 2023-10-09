;;; buffer-history.el --- Emacs Lisp Compatibility Library -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Wurly <48784425+wurly200a@users.noreply.github.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29"))
;; Keywords: lisp
;; Homepage: https://github.com/wurly200a/buffer-history.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

; This program is based on gtags.el from GLOBAL.
; So I left their licences here.
;; ----------------------------------------------------------------------
;; Copyright (c) 1997, 1998, 1999, 2000, 2006, 2007, 2008, 2009, 2010
;;		2011, 2012, 2013, 2021
;;	Tama Communications Corporation
;;
;; GLOBAL home page is at: http://www.gnu.org/software/global/
;; Author: Tama Communications Corporation
;; ----------------------------------------------------------------------

;;; Commentary:

;;; Code:

(setq debug-on-error t)

(provide 'buffer-history)

;;;
;;; Customizing
;;;
(defcustom buffer-history-pop-delete nil
  "*If non-nil, buffer-history-pop will delete the buffer."
  :group 'buffer-history
  :type 'boolean)

;;
;; Variables
;;
(defvar buffer-history-current-buffer nil
  "Current buffer.")
(defvar buffer-history-select-mode-map (make-sparse-keymap)
  "Keymap used in buffer-history select mode.")

;; Key mapping of buffer-history-select-mode.
(define-key buffer-history-select-mode-map "\C-m" 'buffer-history-select-item)
(define-key buffer-history-select-mode-map "g" 'buffer-history-list-display)
(define-key buffer-history-select-mode-map "p" 'previous-line)
(define-key buffer-history-select-mode-map "n" 'next-line)

(defvar buffer-history-combined-stack nil
  "Stack for buffer browsing.")

(defconst buffer-history-list-buffer-name "*buffer-history*")
(defconst buffer-history-process-buffer-name "*buffer-history-process*")

;;
;; utility
;;

(defun buffer-history-push-context ()
  "Push current context to stack."
  (setq buffer-history-combined-stack
        (cons (cons (current-buffer) (point))
              buffer-history-combined-stack)))

(defun buffer-history-pop-context ()
  "Pop context from stack."
  (if (not buffer-history-combined-stack) nil
    (let ((context (car buffer-history-combined-stack)))
      (setq buffer-history-combined-stack (cdr buffer-history-combined-stack))
      context)))

(defun buffer-history-exist-in-stack (buffer)
  "If the BUFFER exists in the stack."
  (memq buffer (mapcar 'car buffer-history-combined-stack)))

(defun buffer-history-pop-stack ()
  "Move to the previous point on the stack."
  (interactive)
  (let (delete context)
    (if (and (not (equal buffer-history-current-buffer nil))
             (not (equal buffer-history-current-buffer (current-buffer))))
        (switch-to-buffer buffer-history-current-buffer)
      ; By default, the buffer of the referred file is left.
      ; If buffer-history-pop-delete is set to t, the file is deleted.
      ; Buffer-History select mode buffer is always deleted.
      (if (and (or buffer-history-pop-delete (equal mode-name "Buffer-History-Select"))
               (not (buffer-history-exist-in-stack (current-buffer))))
          (setq delete t))
      (setq context (buffer-history-pop-context))
      (if (not context)
          (message "The buffer stack is empty.")
        (if delete
            (kill-buffer (current-buffer)))
        (switch-to-buffer (car context))
        (setq buffer-history-current-buffer (car context))
        (goto-char (cdr context))))))

(defun buffer-history-save-current-position ()
  "Save current position."
  (interactive)
  (buffer-history-push-context)
)

(defun my-switch-to-buffer (orig-func buffer-or-name &rest args)
  "Define a custom function to switch to a buffer."
  (apply orig-func buffer-or-name args)
  (when (buffer-file-name)
    (buffer-history-push-context)))

(defun buffer-history-list-display ()
  "Display buffer history."
  (interactive)

  (let (buffer-for-display buffer-for-process)

    (if (setq buffer-for-display (get-buffer buffer-history-list-buffer-name))
        (kill-buffer buffer-for-display)
      )
    (setq buffer-for-display (get-buffer-create buffer-history-list-buffer-name))
    (message "Executing ...")

    (if (setq buffer-for-process (get-buffer buffer-history-process-buffer-name))
        (kill-buffer buffer-for-process)
      )
    (setq buffer-for-process (get-buffer-create buffer-history-process-buffer-name))
    (set-buffer buffer-for-process)

    (let (status temp-temp-list)
      (prefer-coding-system 'utf-8-unix)
      (setq status 0)

      (set-buffer buffer-for-display)

      (setq temp-temp-list (reverse buffer-history-combined-stack))
      (while temp-temp-list
        (let ((temp-line (car temp-temp-list)))
          (insert (format "%s %s\n" (car temp-line) (cdr temp-line)))
          (setq temp-temp-list (cdr temp-temp-list))))

      (if (not (= 0 status))
          (goto-char (point-min))
;        (setq lines (count-lines (point-min) (point-max)))

        (switch-to-buffer buffer-for-display)
        (buffer-history-select-mode))
      )))

(defun buffer-history-select-item ()
  "Select the item."
  (interactive)

  (let (start-point end-point temp-line temp-list)
    (beginning-of-line)
    (setq start-point (point))
    (end-of-line)
    (setq end-point (point))
    (setq temp-line (buffer-substring start-point end-point))
    (setq temp-list (split-string temp-line " "))
    (switch-to-buffer (car temp-list))
    (goto-char (string-to-number (car (cdr temp-list))))
    )
)

;; make buffer-history select-mode
(defun buffer-history-select-mode ()
  "Major mode for choosing the item from list.

Select the item.
	\\[buffer-history-select-item]

Key definitions:
\\{buffer-history-select-mode-map}
Turning on Buffer-History-Select mode calls the value of the variable
`buffer-history-select-mode-hook' with no args, if that value is non-nil."
  (interactive)
;  (message "buffer-history-select-mode")
  (kill-all-local-variables)
  (use-local-map buffer-history-select-mode-map)
  (setq buffer-read-only t
        truncate-lines t
        major-mode 'buffer-history-select-mode
        mode-name "Buffer-History-Select")
  (setq buffer-history-current-buffer (current-buffer))
  (goto-char (point-min))
  (message "[buffer-history list] %d lines" (count-lines (point-min) (point-max)))
;  (setq hl-line-face 'underline)
  (hl-line-mode 1)
  (run-hooks 'buffer-history-select-mode-hook)
)

;;; buffer-history.el ends here
