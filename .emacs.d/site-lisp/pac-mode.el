;; This was done for personal edification and steals from Scott Andrew Borton's
;; Mode Tutorial with aplomb and apologies.

(defvar pac-mode-hook nil)

(defvar pac-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for BinPAC major mode")

(add-to-list 'auto-mode-alist '("\\.pac\\'" . pac-mode))
(add-to-list 'auto-mode-alist '("\\.bif\\'" . pac-mode))

; builtins
; (regexp-opt '("analyzer" "enum" "extern" "flow" "function" "let" "refine" "type" "align" "case" "casefunc" "casetype" "connection" "datagram" "default" "downflow" "flowunit" "of" "offsetof" "padding" "record" "sizeof" "to" "typeattr" "upflow" "withcontext" "withinput" "==" "!=" ">=" "<=" "<<" ">>" "&&" "||" "+=" "->" "int8" "int16" "int32" "uint8" "uint16" "uint32" "bytestring"))

; keywords
; (regexp-opt '("&also" "&byteorder" "&check" "&chunked" "&exportsourcedata" "&if" "&length" "&let" "&linebreaker" "&oneline" "&refcount" "&requires" "&restofdata" "&restofflow" "&transient" "&until" ))

; preprocessor
; (regexp-opt '("%include"))

(defconst pac-font-lock-keywords
  (list
   '("\\(?:&\\(?:also\\|byteorder\\|ch\\(?:eck\\|unked\\)\\|exportsourcedata\\|if\\|l\\(?:e\\(?:ngth\\|t\\)\\|inebreaker\\)\\|oneline\\|re\\(?:fcount\\|quires\\|stof\\(?:data\\|flow\\)\\)\\|transient\\|until\\)\\)" . font-lock-keyword-face)
   '("^\\(?:!=\\|&&\\|\\+=\\|->\\|<[<=]\\|==\\|>[=>]\\|a\\(?:lign\\|nalyzer\\)\\|bytestring\\|c\\(?:ase\\(?:func\\|type\\)?\\|onnection\\)\\|d\\(?:atagram\\|efault\\|ownflow\\)\\|e\\(?:num\\|xtern\\)\\|f\\(?:low\\(?:unit\\)?\\|unction\\)\\|int\\(?:16\\|32\\|8\\)\\|let\\|of\\(?:fsetof\\)?\\|padding\\|re\\(?:cord\\|fine\\)\\|sizeof\\|t\\(?:o\\|ype\\(?:attr\\)?\\)\\|u\\(?:int\\(?:16\\|32\\|8\\)\\|pflow\\)\\|with\\(?:\\(?:contex\\|inpu\\)t\\)\\|||\\)" . font-lock-builtin-face)
   '("\\s-\\(?:!=\\|&&\\|\\+=\\|->\\|<[<=]\\|==\\|>[=>]\\|a\\(?:lign\\|nalyzer\\)\\|bytestring\\|c\\(?:ase\\(?:func\\|type\\)?\\|onnection\\)\\|d\\(?:atagram\\|efault\\|ownflow\\)\\|e\\(?:num\\|xtern\\)\\|f\\(?:low\\(?:unit\\)?\\|unction\\)\\|int\\(?:16\\|32\\|8\\)\\|let\\|of\\(?:fsetof\\)?\\|padding\\|re\\(?:cord\\|fine\\)\\|sizeof\\|t\\(?:o\\|ype\\(?:attr\\)?\\)\\|u\\(?:int\\(?:16\\|32\\|8\\)\\|pflow\\)\\|with\\(?:\\(?:contex\\|inpu\\)t\\)\\|||\\)" . font-lock-builtin-face)
   '("\\(?:%include\\)" . font-lock-preprocessor-face)
   ; #.*
   '("\\(#.*$\\)" 1 font-lock-comment-face)
   ; //.*
   '("\\(//.*$\\)" 1 font-lock-comment-face)
   ; $\sw+
   '("\\(\\$\\sw+\\)" 1 font-lock-keyword-face)
   ; RE/.*/
   '("RE/\\(.*\\)/" 1 font-lock-string-face)
   ; \sw+\s-*
   '("\\(\\sw+\\)\\s-*:" 1 font-lock-variable-name-face)
   ; \sw+.
   '("\\(\\sw+\\)\\." 1 font-lock-variable-name-face)
   ; .\sw+
   '("\\.\\(\\sw+\\)" 1 font-lock-variable-name-face)
   ; parameters
   '("(\\s-*\\(\\sw+\\).*)" 1 font-lock-variable-name-face)
   '("(.*,\\s-*\\(\\sw+\\),.*)" 1 font-lock-variable-name-face)
   '("(.*,\\s-*\\(\\sw+\\).*)" 1 font-lock-variable-name-face)
   ; \s- digit+ \s-
   '("\\s-\\([[:digit:]]+\\)[\\s-\\s_\\s.]*" 1 font-lock-constant-face)
   ; enum|type \sw+
   '("\\(enum\\|type\\) \\(\\sw+\\)" 2 font-lock-function-name-face)
   ; &length=
   '("\\&length=\\([[:digit:]]+\\)" 1 font-lock-constant-face)
   '("\\&length=\\(\\sw+\\)" 1 font-lock-variable-name-face)
   ; 0xabc
   '("\\(0x[0-9a-fA-F]+\\)" 1 font-lock-constant-face)
   ; = 
   '("\\s-+\\(\\sw+\\).*=.*$" 1 font-lock-constant-face)
   ; ->
   '("\\s-+\\(\\sw+\\)\\s-*->.*:.*" 1 font-lock-constant-face)
   ; function
   '("\\(function\\) \\(\\sw+\\)" 2 font-lock-function-name-face)
   ; %{ %}
   '("\\(%[\\sw{}]+\\)" 1 font-lock-keyword-face)
   ; : \sw+
   '(":\\s-*\\(\\sw+\\)" 1 font-lock-type-face)
   ; case \sw+ of
   '("case\\s-+\\(\\sw+\\)\\s-+of" 1 font-lock-variable-name-face)
)
  "Highlighting for basic keywords")

(defvar pac-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "w" st)
    (modify-syntax-entry ?& "w" st)
    (modify-syntax-entry ?% "w" st)
    (modify-syntax-entry ?: "_" st)
    (modify-syntax-entry ?\s " " st)
    (modify-syntax-entry ?\t " " st)    
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for pac-mode")

(defun pac-indent-line ()
  (interactive)
  (beginning-of-line)
    (if (bobp)
        (indent-line-to 0)
      ) ;; end if
    (let ((not-indented t) (cur-indent 0))
      (if (looking-at ".*};?$")
          (progn
            (setq cur-indent (- (current-indentation) default-tab-width))
            (setq not-indented nil)))
      (if (and not-indented (looking-at ".*%}$"))
          (progn
            (setq cur-indent (- (current-indentation) default-tab-width))
            (setq not-indented nil)))
      (save-excursion
        (while not-indented
          (forward-line -1) ;; go back a line
          (if (bobp) (progn 
                       (indent-line-to 0)
                       (setq not-indented nil)))
          (unless (or (looking-at "^[ \t]*$") (looking-at "^[ \t]*#+.*$"))
            (if (looking-at ".*{$")
                (progn
                  (setq cur-indent (+ (current-indentation) default-tab-width))
                  (setq not-indented nil)))
            (if (and not-indented (looking-at "^}.*"))
                (progn
                  (setq cur-indent (- (current-indentation) default-tab-width))
                  (setq not-indented nil)))
            ) ;; end unless empty or tab
          ) ;; end while not-indented
        ) ;; end save-excursion
      (if (< cur-indent 0)
          (setq cur-indent 0))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0))
      ) ;; end let
    )

(eval-after-load "align"
  '(progn
     (add-to-list 'align-rules-list
                  '(binpac-equals
                    (regexp . "[^\\s-]\\(\\s-*\\)\\(=\\)\\(\\s-*\\)[^\\s-]")
                    (valid lambda nil (and (looking-at ".*,$") (looking-back "^\\s-.*")))
                    (group 1 2 3)
;                    (spacing 1 1 1)
                    (modes . (list 'pac-mode))
                    ))
     ;; (add-to-list 'align-rules-list
     ;;              '(binpac-colon
     ;;                (regexp . "\\(.\\)\\(:\\)\\(.\\)")
     ;;                (valid lambda nil (looking-back "^\\s-.*"))
     ;;                (group 1 2 3)
     ;;                (spacing 2 2 1)
     ;;                (tab-stop nil nil nil)
     ;;                (modes . (list 'pac-mode))
     ;;                (separate . group)
     ;;                ))
     (add-to-list 'align-rules-list
                  '(binpac-assign
                    (regexp . "^\\s-+\\sw+\\(\\s-*\\)->\\(\\s-*\\)\\sw+\\(\\s-*\\):\\(\\s-*\\)\\sw+")
                    (valid lambda nil (looking-back "^\\s-+\\sw+\\s-*->.*"))
                    (group 1 2 3 4)
		    (spacing 1 1 1 1)
                    (modes . (list 'pac-mode))
                    (separate . group)
                    ))
     (defadvice align (around smart-tabs activate)
       (let ((indent-tabs-mode nil)) ad-do-it))

     (defadvice align-current (around smart-tabs activate)
       (let ((indent-tabs-mode nil)) ad-do-it))
     )
)

(defun pac-mode ()
  "Major mode for editing BinPAC files"
  (interactive)
  (kill-all-local-variables)
  (setq-local indent-line-function 'pac-indent-line)

  (use-local-map pac-mode-map)

  (set-syntax-table pac-mode-syntax-table)
  (setq major-mode 'pac-mode)
  (setq mode-name "BinPAC")

  (setq-local comment-start "#")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults (list 'pac-font-lock-keywords nil))
  (run-hooks 'pac-mode-hook))

(provide 'pac-mode)
