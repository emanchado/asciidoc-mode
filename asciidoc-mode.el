(defvar asciidoc-mode-hook nil)
(defvar asciidoc-mode-map
  (let ((asciidoc-mode-map (make-sparse-keymap)))
    asciidoc-mode-map)
  "Keymap for AsciiDoc major mode")

(defconst asciidoc-font-lock-keywords-1
  (list
   ; Document title
   '("^.+\n=+\n\\([^:].+\n\\)?\\(:.+:.+\n\\)*"   . font-lock-keyword-face)
   ; Block with lines
   '("^\\[.+\\]\n\\.[^ ].+\n-+\\(\n.*\\)+?\n-+$" . font-lock-doc-face)
   ; Headings
   '("^[^ .].*\n-+$"                  . font-lock-keyword-face)
   '("^[^ ].*\n\\~+$"                 . font-lock-builtin-face)
   '("^[^ ].*\n\\^+$"                 . font-lock-type-face)
   '("^[^ ].*\n\\++$"                 . font-lock-constant-face)
   '("^== .+"                         . font-lock-keyword-face)
   '("^=== .+"                        . font-lock-builtin-face)
   '("^==== .+"                       . font-lock-type-face)
   '("^===== .+"                      . font-lock-constant-face)
   ; Block without lines
   '("^\\[.+\\]\\(\n\\.[^ ].+\\)?\n[^-.\n].*\\(\n.+\\)*" . font-lock-doc-face)
   ; Lists
   '("^ *[*.-]+ "                     . font-lock-comment-face)
   '("^ *[a-z0-9]+\\. "               . font-lock-comment-face)
   ; Emphasis, teletype font
   '("_\\(.*?\\)_"                    . font-lock-function-name-face)
   '("\\*\\([^ ]\\(.*?[^ ]\\)?\\)\\*" . font-lock-variable-name-face)
   '("\\+\\(.+?\\)\\+"                . font-lock-string-face)
   '("`\\(.*?\\)`"                    . font-lock-string-face)
   ; Links
   '("link:[^\\[]+?\\[[^\\[]+\\]"     . font-lock-preprocessor-face))
  "Minimal highlighting expressions for AsciiDoc mode")

(defvar asciidoc-font-lock-keywords asciidoc-font-lock-keywords-1
  "Default highlighting expressions for AsciiDoc mode")

(defvar asciidoc-mode-syntax-table
  (let ((asciidoc-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_  "w" asciidoc-mode-syntax-table)
    (modify-syntax-entry ?\" "w" asciidoc-mode-syntax-table)
    asciidoc-mode-syntax-table)
  "Syntax table for asciidoc-mode")

(defun asciidoc-font-lock-extend-region nil
   (save-excursion
     (goto-char font-lock-beg)
     (let ((found-point (re-search-backward "^\\[.+\\]\n" nil t)))
       (if found-point
 	  (progn
	    (goto-char font-lock-end)
	    (if (re-search-forward "\n--+\n" nil t)
		(progn
		  (beginning-of-line)
		  (setq font-lock-end (point))))
	    (setq font-lock-beg found-point))))))

(defun asciidoc-mode ()
  "Major mode for editing AsciiDoc files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table asciidoc-mode-syntax-table)
  (use-local-map asciidoc-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(asciidoc-font-lock-keywords))
  (setq major-mode 'asciidoc-mode)
  (setq mode-name "AsciiDoc")
  (setq font-lock-extend-region-functions '(asciidoc-font-lock-extend-region))
  (setq-default font-lock-multiline t)
  (run-hooks 'asciidoc-mode-hook))

(provide 'asciidoc-mode)
