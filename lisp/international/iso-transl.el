;;; iso-transl.el --- keyboard input for ISO 10646 chars -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 1987, 1993-1999, 2001-2024 Free Software Foundation,
;; Inc.

;; Author: Howard Gayle
;; Maintainer: emacs-devel@gnu.org
;; Keywords: i18n

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Loading this package defines three ways of entering the non-ASCII
;; printable characters with codes above 127: the prefix C-x 8, and,
;; with `iso-transl-mode' enabled, also the Alt key, and a dead accent
;; key.  For example, you can enter uppercase A-umlaut as `C-x 8 " A'
;; or, if `iso-transl-mode' is enabled, `Alt-" A' (if you have an Alt
;; key) or `umlaut A' (if you have an umlaut/diaeresis key).

;; C-x 8 is set up to autoload this package, but Alt keys and dead
;; accent keys are only defined once you have loaded the package and
;; enabled `iso-transl-mode'.  It is nontrivial to make all of the Alt
;; keys autoload, and it is not clear that the dead accent keys SHOULD
;; autoload this package.

;; Note that C-h b will autoload this package, which is one reason
;; why the more destructive key sequences (without C-x 8 prefix) are
;; only defined in `iso-transl-mode', and are thus reversible.

;; This package supports all characters defined by ISO 8859-1, along
;; with a few other ISO 10646 characters commonly used in English and
;; basic math.

;;; Code:

(defvar iso-transl-dead-key-alist
  '((?\' . mute-acute)
    (?\` . mute-grave)
    (?\" . mute-diaeresis)
    (?^ . mute-asciicircum)
    (?\~ . mute-asciitilde)
    (?\' . dead-acute)
    (?\` . dead-grave)
    (?\" . dead-diaeresis)
    (?^ . dead-asciicircum)
    (?\~ . dead-asciitilde)
    (?^ . dead-circum)
    (?^ . dead-circumflex)
    (?\~ . dead-tilde)
    ;; Someone reports that these keys don't work if shifted.
    ;; This might fix it--no word yet.
    (?\' . S-dead-acute)
    (?\` . S-dead-grave)
    (?\" . S-dead-diaeresis)
    (?^ . S-dead-asciicircum)
    (?\~ . S-dead-asciitilde)
    (?^ . S-dead-circum)
    (?^ . S-dead-circumflex)
    (?\~ . S-dead-tilde))
  "Mapping of ASCII characters to their corresponding dead-key symbols.")

;; The two-character mnemonics are intended to be available in all languages.
;; The ones beginning with `*' have one-character synonyms, but a
;; language-specific table might override the short form for its own use.

(defvar iso-transl-char-map
  '(("* "   . [? ])
    (" "    . [? ])
    ("*!"   . [?¡])
    ("!"    . [?¡])
    ("\"\"" . [?¨])
    ("\"A"  . [?Ä])
    ("\"E"  . [?Ë])
    ("\"I"  . [?Ï])
    ("\"O"  . [?Ö])
    ("\"S"  . [?ẞ])
    ("\"U"  . [?Ü])
    ("\"a"  . [?ä])
    ("\"e"  . [?ë])
    ("\"i"  . [?ï])
    ("\"o"  . [?ö])
    ("\"s"  . [?ß])
    ("\"u"  . [?ü])
    ("\"y"  . [?ÿ])
    ("''"   . [?´])
    ("'A"   . [?Á])
    ("'C"   . [?Ć])
    ("'E"   . [?É])
    ("'I"   . [?Í])
    ("'N"   . [?Ń])
    ("'O"   . [?Ó])
    ("'S"   . [?Ś])
    ("'U"   . [?Ú])
    ("'Y"   . [?Ý])
    ("'Z"   . [?Ź])
    ("'a"   . [?á])
    ("'c"   . [?ć])
    ("'e"   . [?é])
    ("'i"   . [?í])
    ("'n"   . [?ń])
    ("'o"   . [?ó])
    ("'s"   . [?ś])
    ("'u"   . [?ú])
    ("'y"   . [?ý])
    ("'z"   . [?ź])
    ("*$"   . [?¤])
    ("$"    . [?¤])
    ("*+"   . [?±])
    ("+"    . [?±])
    (",,"   . [?¸])
    (",A"   . [?Ą])
    (",C"   . [?Ç])
    (",N"   . [?Ņ])
    (",S"   . [?Ş])
    (",a"   . [?ą])
    (",c"   . [?ç])
    (",n"   . [?ņ])
    (",s"   . [?ş])
    ("*-"   . [?­])
    ("-"    . [?­])
    ("*."   . [?·])
    (".."   . [?·])
    (".z"   . [?ż])
    ("//"   . [?÷])
    ("/A"   . [?Å])
    ("/L"   . [?Ł])
    ("/E"   . [?Æ])
    ("/O"   . [?Ø])
    ("/a"   . [?å])
    ("/l"   . [?ł])
    ("/e"   . [?æ])
    ("/o"   . [?ø])
    ("1/2"  . [?½])
    ("1/4"  . [?¼])
    ("3/4"  . [?¾])
    ("*<"   . [?«])
    ("<"    . [?«])
    ("*="   . [?¯])
    ("=="   . [?¯])
    ("=A"   . [?Ā])
    ("=a"   . [?ā])
    ("=E"   . [?Ē])
    ("=e"   . [?ē])
    ("=/E"  . [?Ǣ])
    ("=/e"  . [?ǣ])
    ("=G"   . [?Ḡ])
    ("=g"   . [?ḡ])
    ("=I"   . [?Ī])
    ("=i"   . [?ī])
    ("=O"   . [?Ō])
    ("=o"   . [?ō])
    ("=U"   . [?Ū])
    ("=u"   . [?ū])
    ("=Y"   . [?Ȳ])
    ("=y"   . [?ȳ])
    ("*>"   . [?»])
    (">"    . [?»])
    ("*?"   . [?¿])
    ("?"    . [?¿])
    ("*C"   . [?©])
    ("C"    . [?©])
    ("*L"   . [?£])
    ("L"    . [?£])
    ("*P"   . [?¶])
    ("P"    . [?¶])
    ("*R"   . [?®])
    ("R"    . [?®])
    ("*S"   . [?§])
    ("S"    . [?§])
    ("*T"   . [?™])
    ("T"    . [?™])
    ("*Y"   . [?¥])
    ("Y"    . [?¥])
    ("^0"   . [?⁰])
    ("^1"   . [?¹])
    ("^2"   . [?²])
    ("^3"   . [?³])
    ("^4"   . [?⁴])
    ("^5"   . [?⁵])
    ("^6"   . [?⁶])
    ("^7"   . [?⁷])
    ("^8"   . [?⁸])
    ("^9"   . [?⁹])
    ("^+"   . [?⁺])
    ("^-"   . [?⁻])
    ("_0"   . [?₀])
    ("_1"   . [?₁])
    ("_2"   . [?₂])
    ("_3"   . [?₃])
    ("_4"   . [?₄])
    ("_5"   . [?₅])
    ("_6"   . [?₆])
    ("_7"   . [?₇])
    ("_8"   . [?₈])
    ("_9"   . [?₉])
    ("_+"   . [?₊])
    ("_-"   . [?₋])
    ("^A"   . [?Â])
    ("^E"   . [?Ê])
    ("^I"   . [?Î])
    ("^O"   . [?Ô])
    ("^U"   . [?Û])
    ("^W"   . [?Ŵ])
    ("^Y"   . [?Ŷ])
    ("^a"   . [?â])
    ("^e"   . [?ê])
    ("^i"   . [?î])
    ("^o"   . [?ô])
    ("^u"   . [?û])
    ("^w"   . [?ŵ])
    ("^y"   . [?ŷ])
    ("^^A"  . [?Ǎ])
    ("^^C"  . [?Č])
    ("^^E"  . [?Ě])
    ("^^G"  . [?Ǧ])
    ("^^I"  . [?Ǐ])
    ("^^K"  . [?Ǩ])
    ("^^N"  . [?Ň])
    ("^^O"  . [?Ǒ])
    ("^^R"  . [?Ř])
    ("^^S"  . [?Š])
    ("^^U"  . [?Ǔ])
    ("^^Z"  . [?Ž])
    ("^^a"  . [?ǎ])
    ("^^c"  . [?č])
    ("^^e"  . [?ě])
    ("^^g"  . [?ǧ])
    ("^^i"  . [?ǐ])
    ("^^k"  . [?ǩ])
    ("^^n"  . [?ň])
    ("^^o"  . [?ǒ])
    ("^^r"  . [?ř])
    ("^^s"  . [?š])
    ("^^u"  . [?ǔ])
    ("^^z"  . [?ž])
    ("_a"   . [?ª])
    ("_o"   . [?º])
    ("`A"   . [?À])
    ("`E"   . [?È])
    ("`I"   . [?Ì])
    ("`O"   . [?Ò])
    ("`U"   . [?Ù])
    ("`a"   . [?à])
    ("`e"   . [?è])
    ("`i"   . [?ì])
    ("`o"   . [?ò])
    ("`u"   . [?ù])
    ("*c"   . [?¢])
    ("c"    . [?¢])
    ("*o"   . [?°])
    ("o"    . [?°])
    ("Oe"   . [?œ])
    ("OE"   . [?Œ])
    ("*u"   . [?μ])
    ("u"    . [?μ])
    ("*m"   . [?μ])
    ("m"    . [?μ])
    ("*x"   . [?×])
    ("x"    . [?×])
    ("*|"   . [?¦])
    ("|"    . [?¦])
    ("~A"   . [?Ã])
    ("~D"   . [?Ð])
    ("~N"   . [?Ñ])
    ("~O"   . [?Õ])
    ("~T"   . [?Þ])
    ("~a"   . [?ã])
    ("~d"   . [?ð])
    ("~n"   . [?ñ])
    ("~o"   . [?õ])
    ("~t"   . [?þ])
    ("~~"   . [?¬])
    ("_h"   . [?‐])
    ("_H"   . [?‑])
    ("_f"   . [?‒])
    ("_n"   . [?–])
    ("_m"   . [?—])
    ("_q"   . [?―])
    ("["    . [?‘])
    ("]"    . [?’])
    ("{"    . [?“])
    ("}"    . [?”])
    ("1+"   . [?†])
    ("2+"   . [?‡])
    ("**"   . [?•])
    ("*'"   . [?′])
    ("*\""  . [?″])
    ("*E"   . [?€])
    ("No"   . [?№])
    ("a<"   . [?←])
    ("a>"   . [?→])
    ("a="   . [?↔])
    ("_-"   . [?−])
    ("~="   . [?≈])
    ("/="   . [?≠])
    ("_<"   . [?≤])
    ("_>"   . [?≥])
    ("' "   . "'")
    ("` "   . "`")
    ("\" "  . "\"")
    ("^ "   . "^")
    ("~ "   . "~"))
  "Alist of character translations for entering ISO characters.
Each element has the form (STRING . VECTOR).
The sequence STRING of ASCII chars translates into the
sequence VECTOR.  (VECTOR is normally one character long.)")

;; Language-specific translation lists.
(defvar iso-transl-language-alist
  '(("Esperanto"
     ("C"  . [?Ĉ])
     ("G"  . [?Ĝ])
     ("H"  . [?Ĥ])
     ("J"  . [?Ĵ])
     ("S"  . [?Ŝ])
     ("U"  . [?Ŭ])
     ("c"  . [?ĉ])
     ("g"  . [?ĝ])
     ("h"  . [?ĥ])
     ("j"  . [?ĵ])
     ("s"  . [?ŝ])
     ("u"  . [?ŭ]))
    ("French"
     ("C"  . [?Ç])
     ("c"  . [?ç]))
    ("German"
     ("A"  . [?Ä])
     ("O"  . [?Ö])
     ("S"  . [?ẞ])
     ("U"  . [?Ü])
     ("a"  . [?ä])
     ("o"  . [?ö])
     ("s"  . [?ß])
     ("u"  . [?ü]))
    ("Portuguese"
     ("C"  . [?Ç])
     ("c"  . [?ç]))
    ("Spanish"
     ("!"  . [?¡])
     ("?"  . [?¿])
     ("N"  . [?Ñ])
     ("n"  . [?ñ]))))

(defvar iso-transl-ctl-x-8-map nil
  "Keymap for C-x 8 prefix.")
(or iso-transl-ctl-x-8-map
    (fset 'iso-transl-ctl-x-8-map
	  (setq iso-transl-ctl-x-8-map (make-sparse-keymap))))
(put 'iso-transl-ctl-x-8-map 'iso-transl-backup nil)
(or key-translation-map
    (setq key-translation-map (make-sparse-keymap)))
(define-key key-translation-map "\C-x8" iso-transl-ctl-x-8-map)
(put 'key-translation-map 'iso-transl-backup nil)

(defmacro iso-transl-define-key (keymap key def)
  "Back up definition of KEY in KEYMAP, then `define-key'."
  `(progn
     (put ',keymap 'iso-transl-backup
	  (cons (cons ,key (or (lookup-key ,keymap ,key) 'none))
		(get ',keymap 'iso-transl-backup)))
     ;; can't use push, since `cl' is not available during compilation
     ;; (push (cons ,key (or (lookup-key ,keymap ,key) 'none))
     ;; 	   (get ',keymap 'iso-transl-backup))
     (define-key ,keymap ,key ,def)))

;; For each entry in the alist, we'll make up to three ways to generate
;; the character in question: the prefix `C-x 8'; the ALT modifier on
;; the first key of the sequence; and (if applicable) replacing the first
;; key of the sequence with the corresponding dead key.  For example, a
;; character associated with the string "~n" can be input with `C-x 8 ~ n'
;; or `Alt-~ n' or `mute-asciitilde n'.
(defun iso-transl-define-prefix-keys (alist)
  (while alist
    (let ((translated-vec (cdr (car alist))))
      (iso-transl-define-key iso-transl-ctl-x-8-map (car (car alist)) translated-vec))
    (setq alist (cdr alist))))

(defun iso-transl-define-keys (alist)
  (while alist
    (let ((translated-vec (cdr (car alist))))
      ;;(iso-transl-define-key iso-transl-ctl-x-8-map (car (car alist)) translated-vec)
      (let ((inchar (aref (car (car alist)) 0))
	    (vec (vconcat (car (car alist))))
	    (tail iso-transl-dead-key-alist))
	(aset vec 0 (logior (aref vec 0) ?\A-\^@))
	(iso-transl-define-key key-translation-map vec translated-vec)
	(iso-transl-define-key isearch-mode-map (vector (aref vec 0)) nil)
	(while tail
	  (if (eq (car (car tail)) inchar)
	      (let ((deadvec (copy-sequence vec))
		    (deadkey (cdr (car tail))))
		(aset deadvec 0 deadkey)
		(iso-transl-define-key isearch-mode-map (vector deadkey) nil)
		(iso-transl-define-key key-translation-map deadvec translated-vec)))
	  (setq tail (cdr tail)))))
    (setq alist (cdr alist))))

(defun iso-transl-set-language (lang)
  "Set shorter key bindings for some characters relevant for LANG.
This affects the \\`C-x 8' prefix.

Note that only a few languages are supported, and for more
rigorous support it is recommended to use an input method
instead.  Also note that many of these characters can be input
with the regular \\`C-x 8' map without having to specify a language
here."
  (interactive (list (let ((completion-ignore-case t))
		       (completing-read "Set which language? "
					iso-transl-language-alist nil t))))
  (iso-transl-define-keys (cdr (assoc lang iso-transl-language-alist))))


;; unconditional definitions
(iso-transl-define-prefix-keys iso-transl-char-map)
(define-key isearch-mode-map "\C-x" nil)
(define-key isearch-mode-map [?\C-x t] 'isearch-other-control-char)
(define-key isearch-mode-map "\C-x8" nil)

(define-minor-mode iso-transl-mode
  "ISO Key translation mode.
This mode defines two ways of entering the non-ASCII printable
characters with codes above 127: the Alt key and
a dead accent key.  For example, you can enter uppercase A-umlaut as
`Alt-\" A' (if you have an Alt key) or `umlaut A' (if
you have an umlaut/diaeresis key).

This character can always be entered as `C-x 8 \" A' regardless of
this mode."
  :group 'i18n
  :lighter " ISO"

  (if iso-transl-mode
      ;; The standard mapping comes automatically.  You can partially overlay it
      ;; with a language-specific mapping by using `M-x iso-transl-set-language'.
      (iso-transl-define-keys iso-transl-char-map)
    (mapc
     (lambda (key-def)
       (define-key key-translation-map (car key-def)
	           (and (not (eq (cdr key-def) 'none)) (cdr key-def))))
     (nreverse (get 'key-translation-map 'iso-transl-backup)))
    (put 'key-translation-map 'iso-transl-backup nil)
    (mapc
     (lambda (key-def)
       (condition-case nil
	   (define-key isearch-mode-map (car key-def)
	               (and (not (eq (cdr key-def) 'none)) (cdr key-def)))
	 ;; if some of the unconditional definitions above are instead made conditional
	 ;; restoring the default keys fails.
	 (error nil)))
     (nreverse (get 'isearch-mode-map 'iso-transl-backup)))
    (put 'isearch-mode-map 'iso-transl-backup nil)))

(provide 'iso-transl)

;;; iso-transl.el ends here
