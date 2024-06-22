;;; ispell.el --- interface to spell checkers  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2023

;; Author: Nathaniel Cunningham & David Reitter

;; This file is part of  Aquamacs Emacs.

;; Aquamacs Emacs is based on GNU Emacs.

;; Both Aquamacs Emacs and GNU Emacs are free software: you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.

;; Aquamacs Emacs and GNU Emacs are distributed in the hope that it
;; will be useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Aquamacs Emacs and GNU Emacs.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains extensions for ispell to use the native Mac OS
;; spell checking system.

;; This file is loaded by ispell to isolate the Aquamacs-specific
;; code. Unfortunately, because we can't currently require ispell.el,
;; the byte-compiler can't tell if variables are defined. Here are
;; some extra defvars to keep it happy.

;; Break out XEmacs menu and split into several calls to avoid having
;; long lines in loaddefs.el.  Detect need off following constant.

;;; Set up dictionary
;;;###autoload
(defvar ispell-menu-map-needed
  ;; only needed when not version 18 and not XEmacs.
  (and (not ispell-menu-map)
       (not (featurep 'xemacs))
       'reload))


(defvar ispell-dictionary-internal ispell-dictionary
  "Internal copy of `ispell-dictionary'; can be modified by
 NSSpellChecker panel without disturbing `ispell-dictionary'
 setting.")

(defconst aquamacs-use-ns-spellchecker t
  "Indicates that Aquamacs is using the NS spellchecker.")

;; **********************************************************************
;; settings to control the use of NSSpellChecker as the spellchecking
;; engine, instead of ispell/aspell/hunspell

(defvar ns-spellchecker-language-alist
'(("es" . "castellano")
  ("da" . "dansk")
  ("de" . "deutsch")
  ("en" . "english")
  ("fr" . "francais")
  ("it" . "italiano")
  ("nl" . "nederlands")
  ("pt" . "portugues")
  ("ru" . "russian")
  ("sv" . "svenska"))
  "alist pairing NSSpellChecker languages with corresponding entries
in ispell's default ispell-dictionary-alist.  This is only for the
purposes of extracting `otherchars' and `many-otherchars-p'")

(defun ns-spellchecker-ispell-equiv-language (language)
  "Returns the name of the language from
ispell-dictionary-base-alist that corresponds to LANGUAGE
of NSSpellChecker.  Returns nil if no corresponding language found."
  (let ((lang-short-abbrev (substring language 0 2))
	(lang-abbrev-p (or (equal (length language) 2)
			   (equal (substring language 2 3) "_"))))
    (if lang-abbrev-p
	(cdr (assoc lang-short-abbrev ns-spellchecker-language-alist)))))

(defun ns-spellchecker-dictionary-otherchars (language)
  "Returns the pair (otherchars . many-otherchars-p) for the specified
NSSpellChecker LANGUAGE, by finding the corresponding entries in
ispell-dictionary-base-alist.  If no corresponding entry is found, assumes
' for otherchars and t for many-otherchars-p."
  (let* ((ispell-language (ns-spellchecker-ispell-equiv-language language))
	 (ispell-lang-list (assoc ispell-language ispell-dictionary-base-alist))
	otherchars
	many-otherchars-p)
    (if ispell-language
	(setq otherchars
	      (car (cdr (cdr (cdr ispell-lang-list))))
	      many-otherchars-p
	      (car (cdr (cdr (cdr (cdr ispell-lang-list))))))
      (setq otherchars "[']"
	    many-otherchars-p t))
    (cons otherchars many-otherchars-p)))

(defun ns-spellchecker-dictionary-details (language)
  (let* ((otherchars-pair (ns-spellchecker-dictionary-otherchars language))
	 (otherchars (car otherchars-pair))
	 (many-otherchars-p (cdr otherchars-pair)))
    (list
     language
     "[[:alpha:]]"
     "[^[:alpha:]]"
     otherchars
     many-otherchars-p
     nil
     nil
     'iso-8859-1)))

(defun ns-spellchecker-list-dictionaries ()
  (let ((lang-list (ns-spellchecker-list-languages))
	dictionary-list)
    ;; if (ns-spellchecker-current-language) returns a language not
    ;; included by (ns-spellchecker-list-languages) --
    ;; e.g. "Multilingual" in OS 10.6 -- append it to the list.
    (push  (ns-spellchecker-current-language)
           lang-list)
    (dolist (lang lang-list)
      (setq dictionary-list
	    (cons (ns-spellchecker-dictionary-details lang)
		  dictionary-list)))
    dictionary-list))

(defcustom ns-spellchecker-chunk-size 100000
  "approximate size in characters of the chunks of text to be
passed to `ns-spellchecker-check-spelling' when checking large
regions."
  :type '(choice (const :tag "Default" 100000)
                 number)
  :group 'ns-spellchecker)

(defun ns-spellchecker-parse-output (word)
  "NSSpellChecker replacement for ispell-parse-output.  Spellcheck WORD
and Return:
1: t for an exact match.
2: A list of possible correct spellings of the format:
   (\"ORIGINAL-WORD\" OFFSET MISS-LIST)
   ORIGINAL-WORD is a string of the possibly misspelled word.
   OFFSET is an integer giving the character offset of the word from
     the beginning of the line.
   MISS-LIST is a possibly null list of guesses."
  (unless (string= ispell-current-dictionary
		     (ns-spellchecker-current-language))
      (ispell-change-dictionary (ns-spellchecker-current-language)))
  (let* ((output (ns-spellchecker-check-spelling word (current-buffer)))
	 (offset (car output)))
    (if offset
	;; word is incorrect -- return
	;; (\"ORIGINAL-WORD\" OFFSET MISS-LIST GUESS-LIST)
	;; GUESS-LIST built from known affixes is nil for NSSpellChecker
	(list word (1+ offset) (ns-spellchecker-get-suggestions word) nil)
      ;; offset is nil: word is correct -- return t
      t)))

(defun ispell-ns-spellcheck-string (string)
  "NSSpellChecker replacement for ispell-parse-output.  Spellcheck STRING
and return a list of lists (one for each misspelled word) of the format:
   (\"ORIGINAL-WORD\" OFFSET MISS-LIST nil)
   ORIGINAL-WORD is a string of the possibly misspelled word.
   OFFSET is an integer giving the line offset of the word.
   MISS-LIST is a possibly null list of guesses."
  (unless (string= ispell-current-dictionary
		   (ns-spellchecker-current-language))
    (ispell-change-dictionary (ns-spellchecker-current-language)))
  (let ((strlen (length string))
	(prev-offset 0)
	ns-spellcheck-output
	offset
	length
	word
	return-list)
    (while (progn
	     (setq ns-spellcheck-output
		   (ns-spellchecker-check-spelling string (current-buffer))
		   offset (car ns-spellcheck-output)
		   length (cdr ns-spellcheck-output))
	     ;; if no misspelled words, terminate while loop
	     (when offset
	       ;; misspelled word found; get word;
	       ;;  set string to not-yet-checked portion;
	       ;;  add details of misspelling to head of return-list
	       (setq word (substring string offset (+ offset length))
		     string (substring string (+ offset length)))
	       (push (list word (+ prev-offset offset)
		           (ns-spellchecker-get-suggestions word)
		           nil)
                     return-list)
	       (setq prev-offset (+ prev-offset offset length))
	       )))
    return-list))

;; Aquamacs only. Not really part of NSSpellChecker, but might as well
;; keep it in an Aquamacs-only file.

;; Menu support for turning flyspell on/off in text modes
(defun toggle-text-mode-flyspell ()
  "Toggle whether to use Flyspell in Text mode and related modes.
This command affects all buffers that use modes related to Text mode,
both existing buffers and buffers that you subsequently create."
  (interactive)
  (let ((enable-mode (not (memq 'turn-on-flyspell text-mode-hook))))
    (if enable-mode
      (add-hook 'text-mode-hook 'turn-on-flyspell)
      (remove-hook 'text-mode-hook 'turn-on-flyspell))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
      (if (or (derived-mode-p 'text-mode) text-mode-variant)
          (flyspell-mode (if enable-mode 1 0)))))
    (message "Flyspell %s in Text modes"
           (if enable-mode "enabled" "disabled"))))

;;;###autoload
(defun menu-bar-text-mode-flyspell ()
  (interactive)
  (toggle-text-mode-flyspell)
  (customize-mark-as-set 'text-mode-hook))

;;; Aquamacs spellcheck submenu for using ispell. See also notes with
;;; AQTODO in ispell.el The Aquamacs spellcheck menu is in a different
;;; place with different options now compared to the Emacs one.

;; Define commands in menu in opposite order you want them to appear
;;;###autoload
(defvar ispell-submenu-map (make-sparse-keymap "Ispell")
  "Keymap for Aquamacs Edit->Spelling->Ispell menu. ")

(if ispell-menu-map-needed
    (progn
      (define-key ispell-submenu-map [ispell-complete-word]
	          `(menu-item ,(purecopy "Complete Word") ispell-complete-word
		              :help
                              ,(purecopy "Complete word at cursor using dictionary")))
      (define-key ispell-submenu-map [ispell-complete-word-interior-frag]
	          `(menu-item ,(purecopy "Complete Word Fragment")
		              ispell-complete-word-interior-frag
		              :help
                              ,(purecopy "Complete word fragment at cursor")))
      (define-key ispell-submenu-map [ispell-continue]
	          `(menu-item ,(purecopy "Continue Spell-Checking") ispell-continue
		              :enable (and (boundp 'ispell-region-end)
				           (marker-position ispell-region-end)
				           (equal (marker-buffer ispell-region-end)
					          (current-buffer)))
		              :help
                              ,(purecopy "Continue spell checking last region")))
      (define-key ispell-submenu-map [ispell-word]
	          `(menu-item ,(purecopy "Spell-Check Word") ispell-word
		              :help
                              ,(purecopy "Spell-check word at cursor")))
      (define-key ispell-submenu-map [ispell-comments-and-strings]
	          `(menu-item ,(purecopy "Spell-Check Comments")
                              ispell-comments-and-strings
		              :help
                              ,(purecopy "Spell-check only comments and strings")))
      (define-key ispell-submenu-map [ispell-region]
	          `(menu-item ,(purecopy "Spell-Check Region") ispell-region
		              :enable mark-active
		              :help ,(purecopy "Spell-check text in marked region")))
      (define-key ispell-submenu-map [ispell-message]
	          `(menu-item ,(purecopy "Spell-Check Message") ispell-message
		              :visible (eq major-mode 'mail-mode)
		              :help
                              ,(purecopy "Skip headers and included message text")))
      (define-key ispell-submenu-map [ispell-buffer]
	          `(menu-item ,(purecopy "Spell-Check Buffer") ispell-buffer
		              :help
                              ,(purecopy "Check spelling of selected buffer")))))

;;;###autoload
(defconst ispell-menu-map
  ;; Use `defconst' so as to redo the menu when loading ispell, like
  ;; the previous code did.

  ;; Define commands in menu in opposite order you want them to appear
  (let ((map (make-sparse-keymap "Spelling"))
        (using-ns-spellcheck
         (string= ispell-program-name "NSSpellChecker")))
    (define-key ispell-menu-map [ispell-region]
	        `(menu-item ,(purecopy "Spell-Check Region")
                            ispell-region
		            :enable mark-active
		            :visible (not using-ns-spellcheck)
                            :help
                            ,(purecopy "Spell-check text in marked region")))
    (define-key ispell-menu-map [ispell-message]
	        `(menu-item ,(purecopy "Spell-Check Message")
                            ispell-message
		            :visible (and (eq major-mode 'mail-mode)
				          (not using-ns-spellcheck))
		            :help
                            ,(purecopy "Skip headers and included message text")))
    (define-key ispell-menu-map [ispell-buffer]
	        `(menu-item ,(purecopy "Spell-Check Buffer")
                            spellcheck-now
		            :visible
                            (not using-ns-spellcheck)
		            :help
                            ,(purecopy "Check spelling of selected buffer")))
    (define-key ispell-menu-map [nsspellchecker-panel-hide]
	        `(menu-item ,(purecopy "Hide Spelling Panel")
                            spellchecker-panel-or-ispell
		            :visible (and using-ns-spellcheck
				          (ns-spellchecker-panel-visible-p))
		            :help
                            ,(purecopy "Toggle Mac OS spellcheck panel visibility")))
    (define-key ispell-menu-map [nsspellchecker-panel-show]
	        `(menu-item ,(purecopy "Show Spelling Panel")
                            spellchecker-panel-or-ispell
		            :visible (and using-ns-spellcheck
				          (not (ns-spellchecker-panel-visible-p)))
		            :help ,(purecopy "Toggle OS X spellcheck panel visibility")))
    (define-key ispell-menu-map [nsspellcheck]
	        `(menu-item ,(purecopy "Spellcheck Now")
                            spellcheck-now
		            :visible using-ns-spellcheck
		            :help
                            ,(purecopy "Check spelling with OS X spellchecker")))
    map)
  "Keymap for Aquamacs Edit->Spelling menu.")

;;;###autoload
(fset 'ispell-menu-map (symbol-value 'ispell-menu-map))
;;;###autoload
(fset 'ispell-submenu-map (symbol-value 'ispell-submenu-map))
(provide 'ns-spellchecker)
;;; ns-spellchecker.el ends here
