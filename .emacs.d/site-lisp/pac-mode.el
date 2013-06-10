
;; This was done for personal edification and steals from Scott Andrew Borton's
;; Mode Tutorial with aplomb and apologies.

(defvar pac-mode-hook nil)

(defvar pac-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for BinPAC major mode")

(add-to-list 'auto-mode-alist '("\\.pac\\'" . pac-mode))

;; keywords


(defconst pac-font-lock-keywords-1
  (list
   '("\\($\\w+\\)" 1 font-lock-keyword-face)
   '("RE/\\(.*\\)/" 1 font-lock-string-face)
   '("\\(enum\\|type\\) \\(\\w+\\)" 2 font-lock-constant-face)
   '("\\(function\\) \\(\\w+\\)" 2 font-lock-function-name-face)
   '("\\(%\\w+\\)" 1 font-lock-keyword-face)
   '("\\(\\w+\\):" 1 font-lock-variable-name-face)
   '("\\(&\\w+\\)" 1 font-lock-builtin-face))
  "Highlighting for basic keywords")

(defconst pac-font-lock-keywords-2
  (append pac-font-lock-keywords-1
          (list
           '("\\<\\(analyzer\\|enum\\|extern\\|flow\\|function\\|let\\|refine\\|type\\|align\\|case\\|casefunc\\|casetype\\|connection\\|datagram\\|default\\|downflow\\|flowunit\\|of\\|offsetof\\|padding\\|record\\|sizeof\\|to\\|typeattr\\|upflow\\|withcontext\\|withinput\\)\\>" . font-lock-builtin-face)))
           "Highlighting for operators and event builtins.")

(defvar pac-font-lock-keywords pac-font-lock-keywords-2
  "Default highlighting expressions for BinPAC mode")

(defvar pac-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?" "w" st)
    st)
  "Syntax table for pac-mode")

(defun pac-mode ()
  "Major mode for eding Pac scripting files"
  (interactive)
  (kill-all-local-variables)
  (use-local-map pac-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(pac-font-lock-keywords))
  (set-syntax-table pac-mode-syntax-table)
  (setq major-mode 'pac-mode)
  (setq mode-name "BinPAC")
  (run-hooks 'pac-mode-hook))

(provide 'pac-mode)
