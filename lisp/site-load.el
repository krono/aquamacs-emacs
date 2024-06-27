;; Aquamacs core files
;; to be loaded and included in dumped state at compile time



;; If you add/remove Lisp files to be loaded here, consider the
;; following issues:

;; i) Load dependencies first.  Automatic loading with `require' is illegal.
;; ii) Add files and dependencies to src/lisp.mk.

;; The above step ensures both that the Lisp files are compiled (if
;; necessary) before the emacs executable is dumped, and that they are
;; passed to make-docfile.  (Any that are not processed for DOC will
;; not have doc strings in the dumped Emacs.)  Because of this:

;; iii) If the file is loaded uncompiled, it should (where possible)
;; obey the doc-string conventions expected by make-docfile.



(defvar aq-compile-path "aquamacs/")

(defvar aq-preloaded nil
  "List of preloaded (precompiled) features.")

(defvar aq--preloading-features-before features)

(defmacro aq-preload (f)
  `(load (concat aq-compile-path ,f)))

;; (load "ns-spellchecker.el")
;; this will only work if files have been byte-compiled.
(load "emacs-lisp/pcase") ;; for easymenu
(load "emacs-lisp/easy-mmode")
(load "emacs-lisp/easymenu")
                                        ;(load "mwheel") ;; wants to be loaded at runtime
(load "disp-table")
(load "tool-bar")  ;; taken out while we're working on it!
(load "tooltip")
;; Not needed now? The load-path isn't set right for them, and I think
;; loadup takes care of them. --WT
;; (load "image") ;; path issue should be OK now
;; (load "image-file")
(load "button")
                                        ; (load "view")  ; can't do it in Emacs 24
                                        ;(load "help-mode")

(load "emacs-lisp/cconv")
(load "emacs-lisp/macroexp")
;; help-fns : no good (require cl-lib)
;; can't load debug; requires cl-lib
;; (load "emacs-lisp/debug")
(load "emacs-lisp/bytecomp")
(load "emacs-lisp/byte-opt")
(load "emacs-lisp/regexp-opt")
;; (load "emacs-lisp/syntax") ; maybe not: syntax-ppss-stats is mutable
;;(load "custom") ;; loading this seems to cause problems with doc strings (why?) e.g. custom-file
                                        ;(load "emacs-lisp/cl")
                                        ;(load "emacs-lisp/cl-seq")

;; (load "international/encoded-kb")  must not be reloaded (creates
;; mutable objects in purespace)
;; XXX can't load wid-edit, requires cl-lib
;; (load "wid-edit.el")
                                        ; (load "emacs-lisp/easymenu") ;; needs to be loaded at runtime... causes strange behavior otherwise
                                        ; (load "recentf")



(let ((load-path
       (append load-path
	       (list (expand-file-name "emulation" (car load-path))))))
  (load "emulation/cua-base")
  )
(load "delsel")
(load "paren")
;;(load "calendar/time-date")
(load "timezone")
;;(load "calendar/parse-time") [no good, require cl-lib]

;; (load "emacs-lisp/cl-lib") ;; produces error




                                        ;(load "emacs-lisp/cl-macs")
                                        ;(load "emacs-lisp/cl")
                                        ;(load "emacs-lisp/cl-seq")
                                        ;(load "emacs-lisp/easy-mmode.el")

(load "emacs-lisp/nadvice") ;; needed for defadvice

;; Can't load time-date; requres cl-lib
;; (load "calendar/time-date") ;; for macros (ats)
;; (defun aq-preload (_x))
;; aquamacs
;; the function aq-preload is supplied by the make-aquamacs script
(aq-preload "aquamacs-macros")
(aq-preload "aquamacs-tools")
(aq-preload "macosx/mac-extra-functions")
                                        ;(aq-preload "applescript-mode")
                                        ; the following can't be precompiled. reason unknown.
;; no text available if this is compiled in.
;;(aq-preload "aquamacs-mode-specific-themes")

(load "emacs-lisp/gv") ;; setf
(aq-preload "aquamacs-macros")
(aq-preload "aquamacs-tool-bar")
(aq-preload "macosx/osx_defaults")
(aq-preload "aquamacs")
(aq-preload "macosx/aquamacs-menu")
; these define minor modes
;(aq-preload "macosx/emulate-mac-keyboard-mode")
;(aq-preload "macosx/osxkeys")
(aq-preload "macosx/mac-extra-functions")
;; autoface must be compiled (due to require aq-cl)
;; compilation is in src/Makefile.in (AQUAMACS_SUPPORT)
;;(aq-preload "aquamacs-autoface-mode")
;(aq-preload "one-buffer-one-frame") [define-minor-mode]
;(aq-preload "smart-frame-positioning")
;(aq-preload "visual-line")
(aq-preload "check-for-updates")
(aq-preload "aquamacs-redo")
;(aq-preload "auctex-config")

;(aq-preload "oneonone/strings")  ; loads cl must be compiled

(aq-preload "tabbar/revive") ; load/save sessions

(let ((load-path (cons (concat default-directory "../lisp/aquamacs/edit-modes/")
		       load-path)))
  (aq-preload "mode-preloads"))

;; (load "mail/rfc822.el")
;; (load "mail/mail-utils.el")
;; (load "international/mule-util.el")

;; (load "assoc.el")
;; (load "speedbar.el")
;; (load "mail/rmail.el")
;; (load "mail/sendmail.el")


;; (load "mail/emacsbug.el")
;; (aq-preload "aquamacs-bug.el")
;(aq-preload "aquamacs-mac-fontsets.el")

;(aq-preload "aquamacs-menu.el")
;(aq-preload "aquamacs-mode-defaults.el")
; (aq-preload "aquamacs-tool-bar.el")
; (aq-preload "auctex-config.el")

;(aq-preload "better-buffer-menu].el")
;(aq-preload "carbon-font.el")
(aq-preload "check-for-updates.el")
                                        ;(aq-preload "color-theme.el")
;; too large - will fail
;;(aq-preload "color-theme-themes.el")

                                        ;(aq-preload "css-mode.el")
                                        ;(aq-preload "def-face-const.el")

                                        ;(aq-preload "strings.el")
                                        ;(aq-preload "files+.el")
                                        ;(aq-preload "fit-frame.el")
                                        ;(aq-preload "frame+.el")
                                        ;(aq-preload "frame-cmds.el")
                                        ;(aq-preload "frame-fns.el")

                                        ;(aq-preload "icomplete.el")
                                        ;(aq-preload "icomplete+.el")
                                        ;(aq-preload "mac-drag-N-drop.el")
                                        ;(aq-preload "autofit-frame.el")
                                        ;(aq-preload "aquamacs-frame-setup.el")
                                        ;(aq-preload "oneonone.el")
                                        ;(aq-preload "smart-frame-positioning.el")
                                        ;(aq-preload "osx_defaults.el")
                                        ;(aq-preload "redo.el")
                                        ;(aq-preload "osxkeys.el")
                                        ;(aq-preload "php-mode.el")

                                        ;(aq-preload "ruby-mode.el")
                                        ; (aq-preload "site-start.el")


(mapc (lambda (e)
	(unless (member e aq--preloading-features-before)
	  (setq aq-preloaded (cons e aq-preloaded))))
      features)
