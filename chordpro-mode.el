;;; chordpro-mode.el --- Major mode for ChordPro lead sheet file format  -*- lexical-binding: t; -*-

;; Author: Howard Ding <hading2@gmail.com>
;; Author: Joseph Turner <public@breatheoutbreathe.in>
;; Version: 2.4.0-pre
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.1"))
;; Homepage: https://git.sr.ht/~breatheoutbreathein/chordpro-mode.el/
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

;; This major mode is for editing files in the ChordPro format:
;; <https://www.chordpro.org/chordpro/chordpro-introduction/>

;; Files with the .cho extension will automatically use `chordpro-mode'.

;;; Code:

(require 'cl-lib)
(require 'thingatpt)
(require 'subr-x)
(require 'compat)

;;;; Customization

(defgroup chordpro nil
  "Major mode for ChordPro lead sheet file format."
  :group 'files
  :prefix "chordpro-")

(defcustom chordpro-environment-directives
  '("bridge" "chorus" "grid" "tab" "verse")
  "Environment directives available for completion with
`chordpro-insert-environment-directive'."
  :type '(repeat string))

;;;; Internal variables

(defvar chordpro-chord-regexp
  "\\[\\([^][]*\\)\\]"
  "Regexp for matching a chord without regard for the point.")

(defvar chordpro-font-lock-defaults
  '((("\\(\\[[^]]*\\]\\)" . font-lock-string-face)
     ("^\\(#.*\\)" . font-lock-comment-face)
     ("\\({subtitle[^}]*}\\)" . font-lock-type-face)
     ("\\({title[^}]*}\\)" . font-lock-keyword-face)
     ("\\({\\(composer\\|artist\\|album\\|capo\\|key\\|time\\|tempo\\)[^}]*}\\)" . font-lock-keyword-face)
     ("\\({[^}]*}\\)" . font-lock-variable-name-face))))

;;;; Functions

(defun chordpro--insert-chord (chord)
  "Normalize then insert CHORD at point."
  (insert "[" (chordpro-normalize-chord chord) "]"))

(defun chordpro-normalize-chord (chord)
  "Trim whitespace, upcase first letter of CHORD, downcase remaining letters."
  (mapconcat (lambda (part)
               (cl-callf upcase (aref part 0))
               part)
             ;; Split on "/" so that A/D has the proper case.
             (string-split (downcase chord) "/" t "[ \t\n\r]+") "/"))

(defun chordpro-complete-chord ()
  "Complete a chord from a list of chord already in the buffer."
  (completing-read "Choose chord: " (chordpro-buffer-chord-list)))

(defun chordpro-chord-text (chord)
  "Return text of CHORD with no leading or trailing brackets."
  (when (string-match chordpro-chord-regexp chord)
    (match-string 1 chord)))

;;;; Commands

(defun chordpro-close-chord ()
  "Close and normalize the chord at point."
  (interactive)
  (if (looking-at "]")
      ;; Account for `electric-pair-mode' which auto-inserts a closing bracket.
      (forward-char)
    (insert "]"))
  (when-let ((chord (thing-at-point 'chordpro-chord)))
    (chordpro-delete-chord-at-point)
    (chordpro--insert-chord (chordpro-chord-text chord))))

(defun chordpro-insert-chord ()
  "Insert a chord chosen from among all chords already in the file.
Uses `completing-read'."
  (interactive)
  (let ((selection (chordpro-complete-chord)))
    (unless (string-blank-p selection)
      (chordpro--insert-chord selection))))

(defun chordpro-buffer-chord-list ()
  "Return a list of the chords currently used in the document."
  ;; If scanning the whole document each time becomes a performance
  ;; issue, consider `with-memoization'.
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
  (let ((selection (chordpro-complete-chord)))
    (unless (string-blank-p selection)
      (chordpro-delete-chord-at-point)
      (chordpro--insert-chord selection))))

(defun chordpro-kill-next-chord ()
  "Kill the next full chord after the point and move point there."
  (interactive)
  (when (re-search-forward chordpro-chord-regexp nil t)
    (kill-region (match-beginning 0) (match-end 0))))

(defun chordpro-copy-next-chord ()
  "Copy the next full chord after the point to the kill ring."
  (interactive)
  (save-excursion
    (when (re-search-forward chordpro-chord-regexp nil t)
      (copy-region-as-kill (match-beginning 0) (match-end 0)))))

(defun chordpro-kill-chord-at-point ()
  "Kill the chord surrounding the point, if there is one."
  (interactive)
  (when-let ((bounds (bounds-of-thing-at-point 'chordpro-chord)))
    (kill-region (car bounds) (cdr bounds))))

(defun chordpro-delete-chord-at-point ()
  "Delete the chord at point, if there is one."
  (interactive)
  (when-let ((bounds (bounds-of-thing-at-point 'chordpro-chord)))
    (delete-region (car bounds) (cdr bounds))))

(defun chordpro-copy-chord-at-point ()
  "Copy the chord at point, if there is one."
  (interactive)
  (when-let ((chord (thing-at-point 'chordpro-chord)))
    (kill-new chord)))

(defun chordpro-move-chord (n)
  "Move the current chord forward N characters."
  (interactive "*p")
  (when-let (bounds (bounds-of-thing-at-point 'chordpro-chord))
    (pcase-let* ((`(,start . ,end) bounds)
                 (orig (point))
                 (offset (- end orig)))
      (condition-case nil
          (atomic-change-group
            ;; Use `atomic-change-group' in case `forward-char' attempts
            ;; to place point before `point-min' or after `point-max'.
            (kill-region start end)
            (forward-char n)
            (yank)
            (backward-char offset))
        (error (goto-char orig)
               (error "Unable to move chord"))))))

(defun chordpro-move-chord-backward (n)
  "Move the current chord backward N characters."
  (interactive "*p")
  (chordpro-move-chord (- n)))

;;;;; Inserting directives

(defun chordpro-insert-single-directive (text)
  "Insert TEXT directive."
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

(defun chordpro-insert-environment-directive (&optional start end)
  "Insert a chordpro environment directive.
When region is active, inserted directive wraps region."
  (interactive (list (use-region-beginning) (use-region-end)))
  (let ((directive (completing-read "Enter directive: " chordpro-environment-directives)))
    (if (use-region-p)
        (with-undo-amalgamate
          (goto-char end)
          (insert (format "{end_of_%s}\n" directive))
          (goto-char start)
          (insert (format "{start_of_%s}" directive)))
      (insert (format "{start_of_%s}\n\n{end_of_%s}\n" directive directive))
      (search-backward "\n" nil nil 2))))

(defun chordpro-insert-environment-directive ()
  "Insert a chordpro environment directive."
  (interactive)
  (let ((directive (completing-read "Enter directive: " chordpro-environment-directives)))
    (if (use-region-p)
        (save-excursion)
      (insert (format "{start_of_%s}\n\n{end_of_%s}\n" directive directive))))
  (search-backward "\n" nil nil 2))
;;;; ChordPro integration

;;;###autoload
(defun chordpro-a2crd (start end &optional flush-empty-lines)
  "Replace buffer or active region with ChordPro format.
This is useful for converting text with chords on top of lyrics,
e.g. from Ultimate Guitar, to proper ChordPro format with chords
embedded in the lyrics.

Note that chords and their lyrics must not be separated by a
newline. With FLUSH-EMPTY-LINES or universal prefix argument
\\[universal-argument], delete empty lines before running a2crd.

In non-interactive use, START and END must be point markers (not
numbers) when FLUSH-EMPTY-LINES is non-nil, since `flush-lines'
will change the region boundaries."
  (interactive
   (let ((start (if (use-region-p) (region-beginning) (point-min-marker)))
         (end (if (use-region-p) (region-end) (point-max-marker))))
     (list start end current-prefix-arg)))
  (when flush-empty-lines
    (goto-char start)
    (flush-lines "^$" start end))
  (shell-command-on-region start end
			   "chordpro --a2crd --fragment -- -" 'insert t))

;;;###autoload
(defun chordpro-export (&optional arg)
  "Export current buffer as PDF with ChordPro.
With a prefix ARG, prompt for chordpro switches before running
external command."
  (interactive "P")
  (unless (buffer-file-name)
    (user-error "ChordPro: Save buffer to file before exporting"))
  (unless (and (buffer-modified-p)
             (not (y-or-n-p "ChordPro: Buffer modified. Export from file on disk?")))
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
        (error "Unable to export ChordPro document. For details, see: %S" buffer)))))

;;;; Major mode

(defvar-keymap chordpro-mode-map
  :parent  text-mode-map
  :doc "Keymap for `chordpro-mode' commands."
  "]"        #'chordpro-close-chord
  "C-c i"    #'chordpro-insert-chord
  "C-c w"    #'chordpro-kill-chord-at-point
  "C-c z"    #'chordpro-kill-next-chord
  "C-c c"    #'chordpro-copy-chord-at-point
  "C-c x"    #'chordpro-copy-next-chord
  "C-c m"    #'chordpro-insert-comment
  "C-c e"    #'chordpro-insert-environment-directive
  "C-c t"    #'chordpro-insert-title
  "C-c s"    #'chordpro-insert-subtitle
  "C-c r"    #'chordpro-choose-replace-current-chord
  "C-M-n"    #'chordpro-move-chord
  "C-M-p"    #'chordpro-move-chord-backward
  "C-c C-c"  #'chordpro-export)

;;;###autoload
(define-derived-mode chordpro-mode text-mode "ChordPro"
  "Major mode for editing Chordpro files.
Special commands:
\\{chordpro-mode-map}"
  (setq font-lock-defaults chordpro-font-lock-defaults)
  (auto-fill-mode -1))

;;;###autoload
(cl-pushnew '("\\.cho\\'" . chordpro-mode) auto-mode-alist :test #'equal)

(put 'chordpro-chord 'bounds-of-thing-at-point
     (lambda ()
       (when (thing-at-point-looking-at chordpro-chord-regexp 10)
         (cons (match-beginning 0) (match-end 0)))))

;;;; Footer

(provide 'chordpro-mode)

;;; chordpro-mode.el ends here
