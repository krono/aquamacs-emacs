;;; csharp-mode.el --- Support for editing C#  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Theodor Thornhill <theo@thornhill.no>
;; Created    : September 2022
;; Keywords   : c# languages oop

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'compile)
(require 'cc-mode)
(require 'cc-langs)
(require 'treesit)

(eval-when-compile
  (require 'cc-fonts))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")

(defgroup csharp nil
  "Major mode for editing C# code."
  :group 'prog-mode)

(eval-and-compile
  (defconst csharp--regex-identifier
    "[A-Za-z][A-Za-z0-9_]*"
    "Regex describing an dentifier in C#.")

  (defconst csharp--regex-identifier-matcher
    (concat "\\(" csharp--regex-identifier "\\)")
    "Regex matching an identifier in C#.")

  (defconst csharp--regex-type-name
    "[A-Z][A-Za-z0-9_]*"
    "Regex describing a type identifier in C#.")

  (defconst csharp--regex-type-name-matcher
    (concat "\\(" csharp--regex-type-name "\\)")
    "Regex matching a type identifier in C#.")

  (defconst csharp--regex-using-or-namespace
    (concat "^using" "\\|" "namespace"
            "\\s *"
            csharp--regex-type-name-matcher)
    "Regex matching identifiers after a using or namespace
    declaration."))

(eval-and-compile
  (c-add-language 'csharp-mode 'java-mode)

  (defun csharp--make-mode-syntax-table ()
    (let ((table (make-syntax-table)))
      (c-populate-syntax-table table)
      (modify-syntax-entry ?@ "_" table)
      table))
  (defvar csharp--make-mode-syntax-table #'csharp--make-mode-syntax-table
    "Workaround for Emacs bug#57065."))

(c-lang-defconst c-make-mode-syntax-table
  csharp #'csharp--make-mode-syntax-table)

(c-lang-defconst c-identifier-syntax-modifications
  csharp (append '((?@ . "w"))
                 (c-lang-const c-identifier-syntax-modifications)))

(c-lang-defconst c-symbol-start
  csharp (concat "[" c-alpha "_@]"))

(c-lang-defconst c-opt-type-suffix-key
  csharp (concat "\\(\\[" (c-lang-const c-simple-ws) "*\\]\\|\\?\\)"))

(c-lang-defconst c-identifier-ops
  csharp '((left-assoc ".")))

(c-lang-defconst c-overloadable-operators
  csharp '("+" "-" "*" "/" "%" "&" "|" "^" "<<" ">>" "=="
           "!=" ">" "<" ">=" "<="))

(c-lang-defconst c-multiline-string-start-char
  csharp ?@)

(c-lang-defconst c-ml-string-opener-re
  ;; "\\(\\(?:@\\$?\\)\\(\"\\)\\)"
  csharp
  (rx
   (group
    (or "@" "@$")
    (group "\""))))

(c-lang-defconst c-ml-string-max-opener-len
  csharp 3)

(c-lang-defconst c-ml-string-max-closer-len
  csharp 2)

(c-lang-defconst c-ml-string-any-closer-re
  ;; "\\(?:\"\"\\)*\\(\\(\"\\)\\)\\(?:[^\"]\\|\\'\\)"
  csharp
  (rx
   (seq
    (zero-or-more "\"\"")
    (group
     (group "\""))
    (or (not (any "\"")) eos))))

(c-lang-defconst c-ml-string-back-closer-re
  ;; "\\(?:\\`\\|[^\"]\\)\"*"
  csharp
  (rx
   (seq
    (or bos
        (not (any "\"")))
    (zero-or-more "\""))))

(c-lang-defconst c-type-prefix-kwds
  csharp '("class" "interface" "struct"))

(c-lang-defconst c-class-decl-kwds
  csharp '("class" "interface" "struct"))

;;; Keyword lists

(c-lang-defconst c-primitive-type-kwds
  csharp '("bool" "byte" "sbyte" "char" "decimal" "double" "float" "int" "uint"
           "long" "ulong" "short" "ushort" "void" "object" "string" "var"))

(c-lang-defconst c-other-decl-kwds
  csharp nil)

(c-lang-defconst c-type-list-kwds
  csharp nil)

(c-lang-defconst c-other-block-decl-kwds
  csharp nil)

(c-lang-defconst c-return-kwds
  csharp '("return"))

(c-lang-defconst c-typedef-kwds
  csharp nil)

(c-lang-defconst c-typeof-kwds
  csharp '("typeof" "is" "as"))

(c-lang-defconst c-type-modifier-prefix-kwds
  csharp '("volatile"))

(c-lang-defconst c-type-modifier-kwds
  csharp '("readonly" "new"))

(c-lang-defconst c-brace-list-decl-kwds
  csharp '("enum" "new"))

(c-lang-defconst c-recognize-post-brace-list-type-p
  csharp t)

(c-lang-defconst c-ref-list-kwds
  csharp nil)

(c-lang-defconst c-using-kwds
  csharp '("using"))

(c-lang-defconst c-equals-type-clause-kwds
  csharp '("using"))

(defun csharp-at-vsemi-p (&optional pos)
  (if pos (goto-char pos))
  (save-excursion
    (beginning-of-line)
    (c-forward-syntactic-ws)
    (looking-at "using\\s *(")))

(c-lang-defconst c-at-vsemi-p-fn
  csharp 'csharp-at-vsemi-p)

(defun csharp-vsemi-status-unknown () t)

(c-lang-defconst c-vsemi-status-unknown-p-fn
  csharp 'csharp-vsemi-status-unknown-p)


(c-lang-defconst c-modifier-kwds
  csharp '("abstract" "default" "final" "native" "private" "protected"
           "public" "partial" "internal" "readonly" "static" "event" "transient"
           "volatile" "sealed" "ref" "out" "virtual" "implicit" "explicit"
           "fixed" "override" "params" "async" "await" "extern" "unsafe"
           "get" "set" "this" "const" "delegate"))

(c-lang-defconst c-other-kwds
  csharp '("select" "from" "where" "join" "in" "on" "equals" "into"
           "orderby" "ascending" "descending" "group" "when"
           "let" "by" "namespace"))

(c-lang-defconst c-colon-type-list-kwds
  csharp '("class" "struct" "interface"))

(c-lang-defconst c-block-stmt-1-kwds
  csharp '("do" "else" "finally" "try"))

(c-lang-defconst c-block-stmt-1-2-kwds
  csharp '("try"))

(c-lang-defconst c-block-stmt-2-kwds
  csharp '("for" "if" "switch" "while" "catch" "foreach" "fixed" "checked"
           "unchecked" "using" "lock"))

(c-lang-defconst c-simple-stmt-kwds
  csharp '("break" "continue" "goto" "throw" "return" "yield"))

(c-lang-defconst c-constant-kwds
  csharp  '("true" "false" "null" "value"))

(c-lang-defconst c-primary-expr-kwds
  csharp '("this" "base" "operator"))

(c-lang-defconst c-inexpr-class-kwds
  csharp nil)

(c-lang-defconst c-class-decl-kwds
  csharp '("class" "struct" "interface"))

(c-lang-defconst c-std-abbrev-keywords
  csharp (append (c-lang-const c-std-abbrev-keywords) '("catch" "finally")))

(c-lang-defconst c-decl-prefix-re
  csharp "\\([{}(;,<]+\\)")

(c-lang-defconst c-recognize-typeless-decls
  csharp t)

(c-lang-defconst c-recognize-<>-arglists
  csharp t)

(c-lang-defconst c-opt-cpp-prefix
  csharp "\\s *#\\s *")

(c-lang-defconst c-opt-cpp-macro-define
  csharp (if (c-lang-const c-opt-cpp-prefix)
             "define"))

(c-lang-defconst c-cpp-message-directives
  csharp '("error" "warning" "region"))

(c-lang-defconst c-cpp-expr-directives
  csharp '("if" "elif"))

(c-lang-defconst c-other-op-syntax-tokens
  csharp  (append '("#")
                  (c-lang-const c-other-op-syntax-tokens)))

(c-lang-defconst c-line-comment-starter
  csharp "//")

(c-lang-defconst c-doc-comment-start-regexp
  csharp "///")

(c-add-style "csharp"
             '("java"
               (c-basic-offset . 4)
               (c-comment-only-line-offset . (0 . 0))
               (c-offsets-alist . ((inline-open           . 0)
                                   (arglist-intro         . +)
                                   (arglist-close         . 0)
                                   (inexpr-class          . 0)
                                   (case-label            . +)
                                   (cpp-macro             . c-lineup-dont-change)
                                   (substatement-open     . 0)))))

(eval-and-compile
  (unless (or (stringp c-default-style)
              (assoc 'csharp-mode c-default-style))
    (setq c-default-style
          (cons '(csharp-mode . "csharp")
                c-default-style))))

(defun csharp--color-forwards (font-lock-face)
  (let (id-beginning)
    (goto-char (match-beginning 0))
    (forward-word)
    (while (and (not (or (eq (char-after) ?\;)
                         (eq (char-after) ?\{)))
                (progn
                  (forward-char)
                  (c-forward-syntactic-ws)
                  (setq id-beginning (point))
                  (> (skip-chars-forward
                      (c-lang-const c-symbol-chars))
                     0))
                (not (get-text-property (point) 'face)))
      (c-put-font-lock-face id-beginning (point) font-lock-face)
      (c-forward-syntactic-ws))))

(c-lang-defconst c-basic-matchers-before
  csharp `(
           ;; Warning face on unclosed strings
           ,@(if (version< emacs-version "27.0")
                 ;; Taken from 26.1 branch
                 `(,(c-make-font-lock-search-function
                     (concat ".\\(" c-string-limit-regexp "\\)")
                     '((c-font-lock-invalid-string))))
               `(("\\s|" 0 font-lock-warning-face t nil)))

           ;; Invalid single quotes
           c-font-lock-invalid-single-quotes

           ;; Keyword constants
           ,@(when (c-lang-const c-constant-kwds)
               (let ((re (c-make-keywords-re nil (c-lang-const c-constant-kwds))))
                 `((eval . (list ,(concat "\\<\\(" re "\\)\\>")
                                 1 c-constant-face-name)))))

           ;; Keywords except the primitive types.
           ,`(,(concat "\\<" (c-lang-const c-regular-keywords-regexp))
              1 font-lock-keyword-face)

           ;; Chained identifiers in using/namespace statements
           ,`(,(c-make-font-lock-search-function
                csharp--regex-using-or-namespace
                `((csharp--color-forwards font-lock-variable-name-face)
                  nil
                  (goto-char (match-end 0)))))


           ;; Negation character
           (eval . (list "\\(!\\)[^=]" 1 c-negation-char-face-name))

           ;; Types after 'new'
           (eval . (list (concat "\\<new\\> *" csharp--regex-type-name-matcher)
                         1 font-lock-type-face))

           ;; Single identifier in attribute
           (eval . (list (concat "\\[" csharp--regex-type-name-matcher "\\][^;]")
                         1 font-lock-variable-name-face t))

           ;; Function names
           (eval . (list "\\([A-Za-z0-9_]+\\)\\(<[a-zA-Z0-9, ]+>\\)?("
                         1 font-lock-function-name-face))

           ;; Nameof
           (eval . (list (concat "\\(\\<nameof\\>\\) *(")
                         1 font-lock-function-name-face))

           (eval . (list (concat "\\<nameof\\> *( *"
                                 csharp--regex-identifier-matcher
                                 " *) *")
                         1 font-lock-variable-name-face))

           ;; Catch statements with type only
           (eval . (list (concat "\\<catch\\> *( *"
                                 csharp--regex-type-name-matcher
                                 " *) *")
                         1 font-lock-type-face))
           ))

(c-lang-defconst c-basic-matchers-after
  csharp (append
          ;; Merge with cc-mode defaults - enables us to add more later
          (c-lang-const c-basic-matchers-after)))

(defcustom csharp-codedoc-tag-face 'c-doc-markup-face-name
  "Face to be used on the codedoc docstring tags.

Should be one of the font lock faces, such as
`font-lock-variable-name-face' and friends.

Needs to be set before `csharp-mode' is loaded, because of
compilation and evaluation time conflicts."
  :type 'symbol)

(defcustom csharp-font-lock-extra-types
  (list csharp--regex-type-name)
  (c-make-font-lock-extra-types-blurb "C#" "csharp-mode" (concat))
  :type 'c-extra-types-widget
  :group 'c)

(defconst csharp-font-lock-keywords-1 (c-lang-const c-matchers-1 csharp)
  "Minimal font locking for C# mode.")

(defconst csharp-font-lock-keywords-2 (c-lang-const c-matchers-2 csharp)
  "Fast normal font locking for C# mode.")

(defconst csharp-font-lock-keywords-3 (c-lang-const c-matchers-3 csharp)
  "Accurate normal font locking for C# mode.")

(defvar csharp-font-lock-keywords csharp-font-lock-keywords-3
  "Default expressions to highlight in C# mode.")

(defun csharp-font-lock-keywords-2 ()
  (c-compose-keywords-list csharp-font-lock-keywords-2))
(defun csharp-font-lock-keywords-3 ()
  (c-compose-keywords-list csharp-font-lock-keywords-3))
(defun csharp-font-lock-keywords ()
  (c-compose-keywords-list csharp-font-lock-keywords))

;;; Doc comments

(defconst codedoc-font-lock-doc-comments
  ;; Most of this is taken from the javadoc example, however, we don't use the
  ;; '@foo' syntax, so I removed that. Supports the XML tags only
  `((,(concat "</?\\sw"         ; XML tags.
              "\\("
              (concat "\\sw\\|\\s \\|[=\n\r*.:]\\|"
                      "\"[^\"]*\"\\|'[^']*'")
              "\\)*/?>")
     0 ,csharp-codedoc-tag-face prepend nil)
    ;; ("\\([a-zA-Z0-9_]+\\)=" 0 font-lock-variable-name-face prepend nil)
    ;; ("\".*\"" 0 font-lock-string-face prepend nil)
    ("&\\(\\sw\\|[.:]\\)+;"     ; XML entities.
     0 ,csharp-codedoc-tag-face prepend nil)))

(defconst codedoc-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "///" limit
          codedoc-font-lock-doc-comments)))))

;;; End of doc comments

;;; Adding syntax constructs

(advice-add 'c-looking-at-inexpr-block
            :around #'csharp-looking-at-inexpr-block)

(defun csharp-looking-at-inexpr-block (orig-fun &rest args)
  (let ((res (csharp-at-lambda-header)))
    (if res
        res
      (apply orig-fun args))))

(defun csharp-at-lambda-header ()
  (save-excursion
    (c-backward-syntactic-ws)
    (unless (bobp)
      (backward-char)
      (c-safe (goto-char (scan-sexps (point) -1)))
      (when (or (looking-at "([[:alnum:][:space:]_,]*)[ \t\n]*=>[ \t\n]*{")
                (looking-at "[[:alnum:]_]+[ \t\n]*=>[ \t\n]*{"))
        ;; If we are at a C# lambda header
        (cons 'inexpr (point))))))

(advice-add 'c-guess-basic-syntax
            :around #'csharp-guess-basic-syntax)

(defun csharp-guess-basic-syntax (orig-fun &rest args)
  (cond
   (;; Attributes
    (save-excursion
      (goto-char (c-point 'iopl))
      (and
       (eq (char-after) ?\[)
       (save-excursion
         (c-go-list-forward)
         (and (eq (char-before) ?\])
              (not (eq (char-after) ?\;))))))
    `((annotation-top-cont ,(c-point 'iopl))))

   ((and
     ;; Heuristics to find object initializers
     (save-excursion
       ;; Next non-whitespace character should be '{'
       (goto-char (c-point 'boi))
       (eq (char-after) ?{))
     (save-excursion
       ;; 'new' should be part of the line
       (goto-char (c-point 'iopl))
       (looking-at ".*new.*"))
     ;; Line should not already be terminated
     (save-excursion
       (goto-char (c-point 'eopl))
       (or (not (eq (char-before) ?\;))
           (not (eq (char-before) ?\{)))))
    (if (save-excursion
          ;; if we have a hanging brace on line before
          (goto-char (c-point 'eopl))
          (eq (char-before) ?\{))
        `((brace-list-intro ,(c-point 'iopl)))
      `((block-open) (statement ,(c-point 'iopl)))))
   (t
    (apply orig-fun args))))

;;; End of new syntax constructs



;;; Fix for strings on version 27.1

(when (version= emacs-version "27.1")
  ;; See:
  ;; https://github.com/emacs-csharp/csharp-mode/issues/175
  ;; https://github.com/emacs-csharp/csharp-mode/issues/151
  ;; for the full story.
  (defun c-pps-to-string-delim (end)
    (let* ((start (point))
           (no-st-s `(0 nil nil ?\" nil nil 0 nil ,start nil nil))
           (st-s `(0 nil nil t nil nil 0 nil ,start nil nil))
           no-st-pos st-pos
           )
      (parse-partial-sexp start end nil nil no-st-s 'syntax-table)
      (setq no-st-pos (point))
      (goto-char start)
      (while (progn
               (parse-partial-sexp (point) end nil nil st-s 'syntax-table)
               (unless (bobp)
                 (c-clear-syn-tab (1- (point))))
               (setq st-pos (point))
               (and (< (point) end)
                    (not (eq (char-before) ?\")))))
      (goto-char (min no-st-pos st-pos))
      nil))

  (defun c-multiline-string-check-final-quote ()
    (let (pos-ll pos-lt)
      (save-excursion
        (goto-char (point-max))
        (skip-chars-backward "^\"")
        (while
            (and
             (not (bobp))
             (cond
              ((progn
                 (setq pos-ll (c-literal-limits)
                       pos-lt (c-literal-type pos-ll))
                 (memq pos-lt '(c c++)))
               ;; In a comment.
               (goto-char (car pos-ll)))
              ((save-excursion
                 (backward-char)        ; over "
                 (c-is-escaped (point)))
               ;; At an escaped string.
               (backward-char)
               t)
              (t
               ;; At a significant "
               (c-clear-syn-tab (1- (point)))
               (setq pos-ll (c-literal-limits)
                     pos-lt (c-literal-type pos-ll))
               nil)))
          (skip-chars-backward "^\""))
        (cond
         ((bobp))
         ((eq pos-lt 'string)
          (c-put-syn-tab (1- (point)) '(15)))
         (t nil))))))

;;; End of fix for strings on version 27.1

;; When invoked by MSBuild, csc’s errors look like this:
;; subfolder\file.cs(6,18): error CS1006: Name of constructor must
;; match name of class [c:\Users\user\project.csproj]

(defun csharp--compilation-error-file-resolve ()
  "Resolve an msbuild error to a (filename . dirname) cons cell."
  ;; http://stackoverflow.com/a/18049590/429091
  (cons (match-string 1) (file-name-directory (match-string 4))))

(defconst csharp-compilation-re-msbuild-error
  (concat
   "^[[:blank:]]*\\(?:[[:digit:]]+>\\)?"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?): "
   "error [[:alnum:]]+: [^\r\n]+\\[\\([^]\r\n]+\\)\\]$")
  "Regexp to match compilation error from msbuild.")

(defconst csharp-compilation-re-msbuild-warning
  (concat
   "^[[:blank:]]*\\(?:[[:digit:]]+>\\)?"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?): "
   "warning [[:alnum:]]+: [^\r\n]+\\[\\([^]\r\n]+\\)\\]$")
  "Regexp to match compilation warning from msbuild.")

;; Notes on xbuild and devenv commonalities
;;
;; These regexes were tailored for xbuild, but apart from the concurrent
;; build-marker ("1>") they share exactly the same match-markers.
;;
;; If we don't exclude the match-markers explicitly, these regexes
;; will also be used to match for devenv as well, including the build-marker
;; in the file-name, causing the lookup to fail.
;;
;; So if we don't want devenv to fail, we actually need to handle it in our
;; xbuild-regexes, but then we automatically get devenv-support for free.

(defconst csharp-compilation-re-xbuild-error
  (concat
   "^[[:blank:]]*\\(?:[[:digit:]]+>\\)?"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?"
   ;; handle weird devenv output format with 4 numbers, not 2 by having optional
   ;; extra capture-groups.
   "\\(?:,\\([0-9]+\\)\\)*): "
   "error [[:alnum:]]+: .+$")
  "Regexp to match compilation error from xbuild.")

(defconst csharp-compilation-re-xbuild-warning
  (concat
   "^[[:blank:]]*\\(?:[[:digit:]]+>\\)?"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?"
   ;; handle weird devenv output format with 4 numbers, not 2 by having optional
   ;; extra capture-groups.
   "\\(?:,\\([0-9]+\\)\\)*): "
   "warning [[:alnum:]]+: .+$")
  "Regexp to match compilation warning from xbuild.")

(defconst csharp-compilation-re-dotnet-error
  "\\([^\r\n]+\\) : error [A-Z]+[0-9]+:")

(defconst csharp-compilation-re-dotnet-warning
  "\\([^\r\n]+\\) : warning [A-Z]+[0-9]+:")

(defconst csharp-compilation-re-dotnet-testfail
  (concat
   "[[:blank:]]+Stack Trace:\n"
   "[[:blank:]]+at [^\n]+ in \\([^\n]+\\):line \\([0-9]+\\)"))


(eval-after-load 'compile
  (lambda ()
    (dolist
        (regexp
         `((dotnet-testfail
            ,csharp-compilation-re-dotnet-testfail
            1 2)
           (xbuild-error
            ,csharp-compilation-re-xbuild-error
            1 2 3 2)
           (xbuild-warning
            ,csharp-compilation-re-xbuild-warning
            1 2 3 1)
           (msbuild-error
            ,csharp-compilation-re-msbuild-error
            csharp--compilation-error-file-resolve
            2
            3
            2
            nil
            (1 compilation-error-face)
            (4 compilation-error-face))
           (msbuild-warning
            ,csharp-compilation-re-msbuild-warning
            csharp--compilation-error-file-resolve
            2
            3
            1
            nil
            (1 compilation-warning-face)
            (4 compilation-warning-face))
           (dotnet-error
            ,csharp-compilation-re-dotnet-error
            1)
           (dotnet-warning
            ,csharp-compilation-re-dotnet-warning
            1 nil nil 1)))
      (add-to-list 'compilation-error-regexp-alist-alist regexp)
      (add-to-list 'compilation-error-regexp-alist (car regexp)))))

(defvar csharp-mode-syntax-table
  (funcall (c-lang-const c-make-mode-syntax-table csharp))
  "Syntax table used in `csharp-mode' buffers.")

(defvar csharp-mode-map
  (let ((map (c-make-inherited-keymap)))
    map)
  "Keymap used in `csharp-mode' buffers.")

(easy-menu-define csharp-mode-menu csharp-mode-map "C# Mode Commands."
  (cons "C#" (c-lang-const c-mode-menu csharp)))

;;; Tree-sitter support

(defcustom csharp-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `csharp-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'csharp)

(defvar csharp-ts-mode--indent-rules
  `((c-sharp
     ((parent-is "compilation_unit") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((parent-is "namespace_declaration") parent-bol 0)
     ((parent-is "class_declaration") parent-bol 0)
     ((parent-is "constructor_declaration") parent-bol 0)
     ((parent-is "method_declaration") parent-bol 0)
     ((parent-is "enum_declaration") parent-bol 0)
     ((parent-is "operator_declaration") parent-bol 0)
     ((parent-is "field_declaration") parent-bol 0)
     ((parent-is "struct_declaration") parent-bol 0)
     ((parent-is "declaration_list") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "argument_list") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "interpolation") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "binary_expression") parent 0)
     ((parent-is "block") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "local_function_statement") parent-bol 0)
     ((parent-is "if_statement") parent-bol 0)
     ((parent-is "for_statement") parent-bol 0)
     ((parent-is "for_each_statement") parent-bol 0)
     ((parent-is "while_statement") parent-bol 0)
     ((match "{" "switch_expression") parent-bol 0)
     ((parent-is "switch_statement") parent-bol 0)
     ((parent-is "switch_body") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "switch_section") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "switch_expression") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "case_statement") parent-bol 0)
     ((parent-is "do_statement") parent-bol 0)
     ((parent-is "equals_value_clause") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "ternary_expression") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "conditional_expression") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "statement_block") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "type_arguments") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "variable_declarator") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "arguments") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "array") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "formal_parameters") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "template_substitution") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "object_pattern") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "object") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "object_type") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "enum_body") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "arrow_function") parent-bol csharp-ts-mode-indent-offset)
     ((parent-is "parenthesized_expression") parent-bol csharp-ts-mode-indent-offset))))

(defvar csharp-ts-mode--keywords
  '("using" "namespace" "class" "if" "else" "throw" "new" "for"
    "return" "await" "struct" "enum" "switch" "case"
    "default" "typeof" "try" "catch" "finally" "break"
    "foreach" "in" "yield" "get" "set" "when" "as" "out"
    "is" "while" "continue" "this" "ref" "goto" "interface"
    "from" "where" "select" "lock" "base" "record" "init"
    "with" "let" "static" "var" "do" "public" "private"
    "readonly" "unmanaged")
  "C# keywords for tree-sitter font-locking.")

(defvar csharp-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'c-sharp
   :override t
   :feature 'comment
   '((comment)  @font-lock-comment-face)
   :language 'c-sharp
   :override t
   :feature 'keyword
   `([,@csharp-ts-mode--keywords] @font-lock-keyword-face
     (modifier) @font-lock-keyword-face
     (this_expression) @font-lock-keyword-face)
   :language 'c-sharp
   :override t
   :feature 'attribute
   `((attribute (identifier) @font-lock-property-face (attribute_argument_list))
     (attribute (identifier) @font-lock-property-face))
   :language 'c-sharp
   :override t
   :feature 'escape-sequence
   '((escape_sequence) @font-lock-escape-face)
   :language 'c-sharp
   :override t
   :feature 'literal
   `((integer_literal) @font-lock-constant-face
     (real_literal) @font-lock-constant-face
     (null_literal) @font-lock-constant-face
     (boolean_literal) @font-lock-constant-face)
   :language 'c-sharp
   :override t
   :feature 'string
   `([(string_literal)
      (verbatim_string_literal)
      (interpolated_string_text)
      (interpolated_verbatim_string_text)
      (character_literal)
      "\""
      "$\""
      "@$\""
      "$@\""] @font-lock-string-face)
   :language 'c-sharp
   :override t
   :feature 'type
   '((predefined_type) @font-lock-type-face
     (implicit_type) @font-lock-type-face
     (nullable_type) @font-lock-type-face
     (type_parameter
      (identifier) @font-lock-type-face)
     (type_argument_list
      (identifier) @font-lock-type-face)
     (generic_name
      (identifier) @font-lock-type-face)
     (array_type
      (identifier) @font-lock-type-face)
     (cast_expression (identifier) @font-lock-type-face)
     ["operator"] @font-lock-type-face
     (type_parameter_constraints_clause
      target: (identifier) @font-lock-type-face))
   :language 'c-sharp
   :feature 'definition
   :override t
   '((qualified_name (identifier) @font-lock-variable-name-face)
     (using_directive (identifier) @font-lock-type-face)

     (enum_declaration (identifier) @font-lock-type-face)
     (enum_member_declaration (identifier) @font-lock-variable-name-face)

     (interface_declaration (identifier) @font-lock-type-face)

     (struct_declaration (identifier) @font-lock-type-face)

     (record_declaration (identifier) @font-lock-type-face)
     (namespace_declaration (identifier) @font-lock-type-face)
     (base_list (identifier) @font-lock-type-face)
     (property_declaration (generic_name))
     (property_declaration
      type: (nullable_type) @font-lock-type-face
      name: (identifier) @font-lock-variable-name-face)
     (property_declaration
      type: (predefined_type) @font-lock-type-face
      name: (identifier) @font-lock-variable-name-face)
     (property_declaration
      type: (identifier) @font-lock-type-face
      name: (identifier) @font-lock-variable-name-face)
     (class_declaration (identifier) @font-lock-type-face)

     (constructor_declaration name: (_) @font-lock-type-face)

     (method_declaration type: (_) @font-lock-type-face)
     (method_declaration name: (_) @font-lock-function-name-face)

     (invocation_expression
      (member_access_expression
       (generic_name (identifier) @font-lock-function-name-face)))
     (invocation_expression
      (member_access_expression
       ((identifier) @font-lock-variable-name-face
        (identifier) @font-lock-function-name-face)))
     (invocation_expression
      (identifier) @font-lock-function-name-face)
     (invocation_expression
      (member_access_expression (identifier) @font-lock-function-name-face))

     (variable_declaration (identifier) @font-lock-type-face)
     (variable_declarator (identifier) @font-lock-variable-name-face)

     (parameter type: (identifier) @font-lock-type-face)
     (parameter name: (identifier) @font-lock-variable-name-face))
   :language 'c-sharp
   :feature 'expression
   '((conditional_expression (identifier) @font-lock-variable-name-face)
     (postfix_unary_expression (identifier)* @font-lock-variable-name-face)
     (assignment_expression (identifier) @font-lock-variable-name-face))
   :language 'c-sharp
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'c-sharp
   :feature 'delimiter
   '((["," ":" ";"]) @font-lock-delimiter-face)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

(defun csharp-ts-mode--imenu-1 (node)
  "Helper for `csharp-ts-mode--imenu'.
Find string representation for NODE and set marker, then recurse
the subtrees."
  (let* ((ts-node (car node))
         (subtrees (mapcan #'csharp-ts-mode--imenu-1 (cdr node)))
         (name (when ts-node
                 (or (treesit-node-text
                      (or (treesit-node-child-by-field-name
                           ts-node "name"))
                      t)
                     "Unnamed node")))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond
     ((null ts-node) subtrees)
     (subtrees
      `((,name ,(cons name marker) ,@subtrees)))
     (t
      `((,name . ,marker))))))

(defun csharp-ts-mode--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((node (treesit-buffer-root-node))
         (class-tree (treesit-induce-sparse-tree
                      node "^class_declaration$" nil 1000))
         (interface-tree (treesit-induce-sparse-tree
                          node "^interface_declaration$" nil 1000))
         (enum-tree (treesit-induce-sparse-tree
                     node "^enum_declaration$" nil 1000))
         (struct-tree (treesit-induce-sparse-tree
                       node "^struct_declaration$"  nil 1000))
         (record-tree (treesit-induce-sparse-tree
                       node "^record_declaration$"  nil 1000))
         (method-tree (treesit-induce-sparse-tree
                       node "^method_declaration$" nil 1000))
         (class-index (csharp-ts-mode--imenu-1 class-tree))
         (interface-index (csharp-ts-mode--imenu-1 interface-tree))
         (enum-index (csharp-ts-mode--imenu-1 enum-tree))
         (record-index (csharp-ts-mode--imenu-1 record-tree))
         (struct-index (csharp-ts-mode--imenu-1 struct-tree))
         (method-index (csharp-ts-mode--imenu-1 method-tree)))
    (append
     (when class-index `(("Class" . ,class-index)))
     (when interface-index `(("Interface" . ,interface-index)))
     (when enum-index `(("Enum" . ,enum-index)))
     (when record-index `(("Record" . ,record-index)))
     (when struct-index `(("Struct" . ,struct-index)))
     (when method-index `(("Method" . ,method-index))))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;;;###autoload
(define-derived-mode csharp-mode prog-mode "C#"
  "Major mode for editing Csharp code.

Key bindings:
\\{csharp-mode-map}"
  :after-hook (c-update-modeline)
  (c-initialize-cc-mode t)
  (c-init-language-vars csharp-mode)
  (c-common-init 'csharp-mode)
  (setq-local c-doc-comment-style '((csharp-mode . codedoc)))
  (run-mode-hooks 'c-mode-common-hook))

;;;###autoload
(define-derived-mode csharp-ts-mode prog-mode "C#"
  "Major mode for editing C# code."

  (unless (treesit-ready-p 'c-sharp)
    (error "Tree-sitter for C# isn't available"))

  ;; Tree-sitter.
  (treesit-parser-create 'c-sharp)

  ;; Comments.
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")

  ;; Indent.
  (setq-local treesit-simple-indent-rules csharp-ts-mode--indent-rules)

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))

  ;; Navigation.
  (setq-local treesit-defun-type-regexp "declaration")

  ;; Font-lock.
  (setq-local treesit-font-lock-settings csharp-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment keyword constant string)
                (type definition expression literal attribute)
                (bracket delimiter)))

  ;; Imenu.
  (setq-local imenu-create-index-function #'csharp-ts-mode--imenu)
  (setq-local which-func-functions nil) ;; Piggyback on imenu
  (treesit-major-mode-setup))

(provide 'csharp-mode)

;;; csharp-mode.el ends here
