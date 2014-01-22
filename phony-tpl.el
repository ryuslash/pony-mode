;;
;; Phony-tpl-minor-mode
;;
;; Commentary:
;;
;; This minor mode provides syntax highlighting and some useful default
;; shortcuts for editing django template (html) files.
;;

;;; Code:

(require 'sgml-mode)

(defgroup phony-tpl nil
  "Djangification for Templates in Emacs"
  :group 'pony
  :prefix "phony-tpl-")

;;
;; Indentation of Django tags
;;
;; Commentary:
;;
;; This implementation "borrows" (read: Steals Liberally) from Florian Mounier's Jinja2 Mode
;; https://github.com/paradoxxxzero/jinja2-mode
;;
;; All we really do here is redefine the relevant functions, alter the keywords and
;; make sure that TAB doesn't affect (point)
;;

(defvar phony-nonindenting-tags
  '("break" "continue" "do" "extends" "from" "import" "include" "set")
  "List of tags that do not imply indentation (or require an end tag).")

(defvar phony-indenting-tags
  '("autoescape" "block" "call" "elif" "else" "filter" "for" "if" "macro"
    "pluralize" "trans" "with")
  "List of template tags that imply indentation.")

(defvar phony-indenting-tags-regexp
  (regexp-opt phony-indenting-tags)
  "Regular expression matching a template tag that implies indentation.")

(defun sgml-indent-line-num ()
  "Indent the current line as SGML."
  (let* ((savep (point))
         (indent-col
          (save-excursion
            (back-to-indentation)
            (if (>= (point) savep) (setq savep nil))
            (sgml-calculate-indent))))
    (if (null indent-col)
        0
      (if savep
          (save-excursion indent-col)
        indent-col))))

(defun phony-calculate-indent-backward (default)
  "Return indent column based on previous lines"
  (let ((indent-width sgml-basic-offset))
    (forward-line -1)
    (if (looking-at "^[ \t]*{%-? *end") ; Don't indent after end
        (current-indentation)
      (if (looking-at (concat "^[ \t]*{%-? *.*?{%-? *end" phony-indenting-tags-regexp "\\>"))
          (current-indentation)
        (if (looking-at (concat "^[ \t]*{%-? *" phony-indenting-tags-regexp "\\>")) ; Check start tag
            (+ (current-indentation) indent-width)
          (if (looking-at "^[ \t]*<") ; Assume sgml block trust sgml
              default
            (if (bobp)
                0
              (phony-calculate-indent-backward default))))))))

(defun phony-calculate-indent ()
  "Return indent column"
  (save-excursion
    (beginning-of-line)
    (if (bobp)  ; Check begining of buffer
        0
      (let ((indent-width sgml-basic-offset) (default (sgml-indent-line-num)))
        (if (looking-at "^[ \t]*{%-? *\\(?:e\\(nd\\|lse\\|lif\\)\\|pluralize\\)") ; Check close tag
            (progn
              (forward-line -1)
              (if
                  (and
                   (looking-at (concat "^[ \t]*{%-? *" phony-indenting-tags-regexp "\\>"))
                   (not (looking-at (concat "^[ \t]*{%-? *.*?{% *end" phony-indenting-tags-regexp "\\>"))))
                  (current-indentation)
                (- (current-indentation) indent-width)))
          (if (looking-at "^[ \t]*</") ; Assume sgml end block trust sgml
              default
            (phony-calculate-indent-backward default)))))))

(defun phony-indent ()
  "Indent current line as Jinja code"
  (interactive)
  (let ((pos (- (point-max) (point)))
        (indent (phony-calculate-indent)))
    (if (< indent 0)
        (setq indent 0))
    (indent-line-to indent)
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (let ((moved-pos (- (point-max) pos)))
     (if (> moved-pos (point))
         (goto-char moved-pos)))))

(defvar phony-tpl-mode-hook nil)

(defconst phony-tpl-font-lock-keywords
  (append
   sgml-font-lock-keywords
   (list
    '("{%.*\\(\\bor\\b\\).*%}" . (1 font-lock-builtin-face))
    ;'("{% ?comment ?%}\\(\n?.*?\\)+?{% ?endcomment ?%}" . font-lock-comment-face)
    '("{#.*#}" . font-lock-comment-face)
    (cons (concat "{% *\\(\\(?:end\\)?" (regexp-opt phony-indenting-tags) "\\|" (regexp-opt phony-nonindenting-tags) "\\>\\).*?%}") 1)
    '("{{ ?\\(.*?\\) ?}}" . (1 font-lock-variable-name-face))
    '("{%\\|\\%}\\|{{\\|}}" . font-lock-builtin-face)
    ))
  "Highlighting for phony-tpl-mode")

(define-minor-mode phony-tpl-minor-mode
  "Phony-templatin-riffic"
  :initial nil
  :lighter " PhonyTpl")

(defun phony-tpl-mode()
  "Minor mode for editing phony templates"
  (interactive)
  (phony-tpl-minor-mode t)
  (run-hooks 'phony-tpl-mode-hook)
  (set (make-local-variable 'font-lock-defaults)
       '(phony-tpl-font-lock-keywords))
  (if (> emacs-major-version 23)
      (font-lock-refresh-defaults))
  (set (make-local-variable 'indent-line-function) 'phony-indent)
   (phony-load-snippets))

;; phony-tpl-minor-mode ends
