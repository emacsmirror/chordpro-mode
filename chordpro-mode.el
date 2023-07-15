;;; chordpro-mode.el --- Major mode for ChordPro lead sheet file format  -*- lexical-binding: t; -*-

;; Author: Howard Ding <hading2@gmail.com>
;; Author: Joseph Turner <public@breatheoutbreathe.in>
;; Version: 1.0.0
;; Keywords: convenience

;; Copyright (C) 2023 Joseph Turner

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;; This file incorporates work covered by the following copyright and
;; permission notice:

;;     The MIT License (MIT)

;;     Copyright (c) 2014 Howard A. Ding

;;     Permission is hereby granted, free of charge, to any person obtaining a copy
;;     of this software and associated documentation files (the "Software"), to deal
;;     in the Software without restriction, including without limitation the rights
;;     to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;     copies of the Software, and to permit persons to whom the Software is
;;     furnished to do so, subject to the following conditions:

;;     The above copyright notice and this permission notice shall be included in
;;     all copies or substantial portions of the Software.

;;     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;     IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;     FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;     AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;     LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;     OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;     THE SOFTWARE.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)

;;;; Internal variables

(defvar chordpro-chord-regexp
  "\\[\\([^][]*\\)\\]"
  "Regexp for matching a chord without regard for the point.")

(defvar chordpro-font-lock-defaults
  '((("\\(\\[[^]]*\\]\\)" . font-lock-string-face)
     ("^\\(#.*\\)" . font-lock-comment-face)
     ("\\({subtitle[^}]*}\\)" . font-lock-type-face)
     ("\\({title[^}]*}\\)" . font-lock-keyword-face)
     ("\\({[^}]*}\\)" . font-lock-variable-name-face))))

;;;; Commands

(defun chordpro--insert-chord (chord)
  "Normalize then insert CHORD at point."
  (insert "[" (chordpro-normalize-chord chord) "]"))

(defun chordpro-insert-chord ()
  "Insert a chord chosen from among all chords already in the file.
Uses `completing-read'."
  (interactive)
  (let ((selection (completing-read "Choose chord: " (chordpro-buffer-chord-list))))
    (unless (string-blank-p selection)
      (chordpro--insert-chord selection))))

(defun chordpro-buffer-chord-list ()
  "Return a list of the chords currently used in the document."
  ;;This could be done more efficiently, but for most usages
  ;;it shouldn't be a problem to just scan the whole document each time
  (interactive)
  (let (chords)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward chordpro-chord-regexp nil t)
          (cl-pushnew (match-string 1) chords))))
    (sort chords #'string<)))

(defun chordpro-choose-replace-current-chord ()
  "Replace the current chord.
Uses `completing-read' to select among chords in current buffer."
  (interactive)
  (let ((selection (completing-read "Choose chord: " (chordpro-buffer-chord-list))))
    (unless (string-blank-p selection)
      (chordpro-delete-current-chord)
      (chordpro--insert-chord selection))))

(defun chordpro-normalize-chord (chord)
  "Trim whitespace, capitalize first letter of chord."
  (let ((trimmed-chord (string-trim chord)))
    ;; Without splitting the string, `capitalize' would incorrectly
    ;; return qualified chords like "A#Dim" instead of "A#dim".
    (concat (capitalize (substring trimmed-chord 0 1)) (substring trimmed-chord 1))))

(defun chordpro-kill-next-chord ()
  "Kill the next full chord after the point and move point there."
  (interactive)
  (let ((start (re-search-forward chordpro-chord-regexp nil t)))
    (if start
        (progn 
          (kill-region (match-beginning 0) (match-end 0))
          (goto-char (match-beginning 0))))))

(defun chordpro-copy-next-chord ()
  "Copy the next full chord after the point to the kill ring."
  (interactive)
  (save-excursion
    (let ((start (re-search-forward chordpro-chord-regexp nil t)))
      (if start
          (copy-region-as-kill (match-beginning 0) (match-end 0))))))

(defun chordpro-kill-current-chord ()
  "Kill the chord surrounding the point, if there is one."
  (interactive)
  (chordpro-operate-on-current-chord 'kill-region))

(defun chordpro-delete-current-chord ()
  "Delete the chord surrounding the point, if there is one."
  (interactive)
  (chordpro-operate-on-current-chord 'delete-region))

(defun chordpro-copy-current-chord ()
  "Copy the chord surrounding the point, if there is one."
  (interactive)
  (chordpro-operate-on-current-chord 'copy-region-as-kill))

(defun chordpro-operate-on-current-chord (function)
  "Call a two argument function on the current chord, if it exists, with
the start and end of the chord."
  (let ((current-position (point-marker)))
    (save-excursion
      (let ((start-found (search-backward "[" nil t)))
        (if start-found
            (let* ((start (point-marker))
                   (end-found (search-forward "]" nil t)))
              (if end-found
                  (let ((end (point-marker)))
                    (if (and (<  start current-position)
                             (< current-position end))
                        (funcall function start end))))))))))

(defun chordpro-current-chord-forward (n)
  "Move the current chord forward n characters."
  (interactive "*p")
  (let ((current-position (point-marker))
        (chord-offset (chordpro-position-in-current-chord)))
    (set-marker-insertion-type current-position t)
    (chordpro-operate-on-current-chord
     (lambda (start end)
       (kill-region start end)
       (forward-char n)
       (yank)))
    ;;I have to assume there's a better way to do this, but this works
    ;;Get back in the chord and then move to the offset
    (if (> n 0)
        (goto-char (+ current-position n 1))
      (goto-char (- current-position (+ n 4))))
    (chordpro-move-to-position-in-current-chord chord-offset)))
     
(defun chordpro-current-chord-backward (n)
  "Move the current chord backward n characters."
  (interactive "*p")
  (chordpro-current-chord-forward (- n)))

(defun chordpro-move-to-position-in-current-chord (n)
  "Move to the nth character in the current chord."
  (search-backward "[")
  (forward-char n))

(defun chordpro-position-in-current-chord ()
  "Determine the offset inside the current chord."
  (interactive)
  (let ((current-position (point)))
    (save-excursion
      (search-backward "[")
      (- current-position (point)))))

(defun chordpro-insert-single-directive (text)
  (insert "{" text ": }\n")
  (search-backward "}"))

(defun chordpro-insert-comment ()
  "Insert a chordpro comment."
  (interactive)
  (chordpro-insert-single-directive "comment"))

(defun chordpro-insert-title ()
  "Insert a chordpro title."
  (interactive)
  (chordpro-insert-single-directive "title"))

(defun chordpro-insert-subtitle ()
  "Insert a chordpro subtitle."
  (interactive)
  (chordpro-insert-single-directive "subtitle"))

(defun chordpro-insert-chorus ()
  "Insert a chordpro chorus."
  (interactive)
  (insert "{start-of-chorus}\n\n{end-of-chorus}\n")
  (search-backward "\n" nil nil 2))

;;;; ChordPro integration

(defun chordpro-export (&optional arg)
  "Export current buffer as PDF with ChordPro.
With a prefix argument, prompt for chordpro switches before running
external command."
  (interactive "P")
  (let* ((input (shell-quote-argument (buffer-file-name)))
         (output (file-name-with-extension input "pdf"))
         (default-switches (concat "--output=" output " " input))
         (switches (split-string-shell-command
                    (if arg
                        (read-string "ChordPro switches: " default-switches)
                      default-switches)))
         (buffer (with-current-buffer (get-buffer-create " *ChordPro output/errors*")
                   (erase-buffer)
                   (current-buffer))))
    (if (zerop (apply #'call-process "chordpro" nil buffer nil switches))
        (message "Successfully exported to PDF: %s" output)
      (error "Unable to export ChordPro document. For details, see: %S" buffer))))

;;;; Major mode

(defvar-keymap chordpro-mode-map
  :parent  text-mode-map
  :doc "Keymap for `chordpro-mode' commands."
  "C-c i" #'chordpro-insert-chord
  "C-c w" #'chordpro-kill-current-chord
  "C-c z" #'chordpro-kill-next-chord
  "C-c c" #'chordpro-copy-current-chord
  "C-c x" #'chordpro-copy-next-chord
  "C-c m" #'chordpro-insert-comment
  "C-c h" #'chordpro-insert-chorus
  "C-c t" #'chordpro-insert-title
  "C-c s" #'chordpro-insert-subtitle
  "C-c r" #'chordpro-choose-replace-current-chord
  "C-M-n" #'chordpro-current-chord-forward
  "C-M-p" #'chordpro-current-chord-backward)

;;;###autoload
(define-derived-mode chordpro-mode text-mode "ChordPro"
  "Major mode for editing Chordpro files.
Special commands:
\\{chordpro-mode-map}"
  (setq font-lock-defaults chordpro-font-lock-defaults)
  (auto-fill-mode -1))

;;;###autoload
(cl-pushnew '("\\.cho\\'" . chordpro-mode) auto-mode-alist :test #'equal)

;;;; Footer

(provide 'chordpro)

;;; chordpro-mode.el ends here
