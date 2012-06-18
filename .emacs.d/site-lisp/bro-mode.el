;; A mode for the Bro policy language.
;;
;; Largely trial-and-errored from
;; - http://two-wugs.net/emacs/mode-tutorial.html
;; - http://users.ox.ac.uk/~wadh1342/gle-mode.el
;;
;; Written by Christian Kreibich
;; (clearly without any clue of Lisp/Emacs)
;;
;; Last update: Wed Aug  3 17:41:17 BST 2005


(defvar bro-mode-hook nil)

;; (defvar bro-mode-map
;;   (let ((bro-mode-map (make-keymap)))
;;     (define-key bro-mode-map "\C-j" 'newline-and-indent)
;;     bro-mode-map)
;;   "Keymap for BRO major mode")

(add-to-list 'auto-mode-alist '("\\.bro\\'" . bro-mode))


;; ---- Syntax Highlighting --------------------------------------------

(defvar bro-mode-keywords
  `(("\\(@.*$\\)" (0 font-lock-doc-face))
    (,(concat "\\<"
              (regexp-opt '("const" "redef") t)
              "\\>") (0 font-lock-constant-face))
    (,(concat "\\<"
              (regexp-opt '("addr" "bool" "count" "counter" "double" "enum"
                            "file" "int" "interval" "list" "net" "pattern"
                            "port" "record" "set" "string" "subnet" "table"
                            "timer" "time" "union" "vector") t)
              "\\>") (0 font-lock-type-face))
    (,(concat "\\<"
              (regexp-opt '("add" "alarm" "any" "break" "case" "default"
                            "delete" "else" "event" "export" "fmt" "for"
                            "function" "global" "global_attr" "if" "in"
                            "local" "match" "module" "next" "of" "print"
                            "return" "schedule" "switch" "this" "type"
                            "using") t)	      
              "\\>") (0 font-lock-keyword-face))
    (,(concat "\\<"
              (regexp-opt '("day" "days" "hr" "hrs" "min" "mins" "sec" "secs"
                            "msec" "msecs" "usec" "usecs") t)
              "\\>") (0 font-lock-function-name-face))
    ("\\(&[a-zA-Z_0-9]+\\)" (0 font-lock-builtin-face))
    )
  "Keyword hilghlighting specification for Bro mode")

(font-lock-add-keywords 'bro-mode bro-mode-keywords)

;; ---- The Syntax Table -----------------------------------------------

(defvar bro-mode-syntax-table
  (let ((bro-mode-syntax-table (make-syntax-table)))

    ;; Underscores and hyphens are valid parts of tokens
    (modify-syntax-entry ?_ "w" bro-mode-syntax-table)
    (modify-syntax-entry ?- "w" bro-mode-syntax-table)
    (modify-syntax-entry ?. "w" bro-mode-syntax-table)
    (modify-syntax-entry ?& "w" bro-mode-syntax-table)

    ;; What's this? Obviously, it is the definition of a comment in Bro! :)
    (modify-syntax-entry ?# "<" bro-mode-syntax-table)
    (modify-syntax-entry ?\n ">" bro-mode-syntax-table)

    bro-mode-syntax-table)
  "Syntax table for Bro mode")


;; ---- Main definitions -----------------------------------------------

(defun bro-mode ()
  "Major mode for editing Bro policy files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table bro-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(bro-mode-keywords))
  (setq major-mode 'bro-mode)
  (setq mode-name "Bro")
  (run-hooks 'bro-mode-hook))

(provide 'bro-mode)
