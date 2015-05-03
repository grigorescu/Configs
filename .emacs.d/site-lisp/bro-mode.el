;;;; A few basic settings first

; Redef this to do stuff when bro-mode loads:
(defvar bro-mode-hook nil)

; <RET> will indent as well
(defvar bro-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "RET") 'align-newline-and-indent)
    map)
  "Keymap for Bro scripting major mode")

; Load for .bro files
(add-to-list 'auto-mode-alist '("\\.bro\\'" . bro-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1 - Syntax Table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bro-mode-syntax-table
  (let ((st (make-syntax-table)))
    ; whitespace is [ \t]
    (modify-syntax-entry ?\s " " st)
    (modify-syntax-entry ?\t " " st)
    ; symbols are words + _-&%
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?- "_" st)
    (modify-syntax-entry ?& "_" st)
    (modify-syntax-entry ?% "_" st)
    ; # starts a comment
    (modify-syntax-entry ?# "<" st)
    ; Newlines end comments
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for bro-mode")

 ;; -   whitespace character           [ \t]
 ;; w   word constituent               [A-Za-z0-9_-]
 ;; _   symbol constituent             
 ;; .   punctuation character      
 ;; (   open delimiter character   
 ;; )   close delimiter character  
 ;; "   string quote character     
 ;; \   escape character           
 ;; /   character quote character		  
 ;; $   paired delimiter         		  
 ;; '   expression prefix        		  
 ;; <   comment starter          		  
 ;; >   comment ender            		  
 ;; !   generic comment delimiter		  
 ;; |   generic string delimiter 		  

; types
; (regexp-opt '("add" "addr" "any" "bool" "break" "case" "const" "copy" "count" "counter" "default" "delete" "double" "else" "enum" "event" "export" "fallthrough" "file" "for" "function" "global" "hook" "if" "in" "int" "interval" "list" "local" "module" "next" "of" "opaque" "pattern" "port" "print" "record" "redef" "schedule" "set" "string" "subnet" "switch" "table" "time" "timeout" "timer" "type" "union" "vector" "when"))
;
; keywords
; (regexp-opt '("&add_func" "&create_expire" "&default" "&delete_func" "&deprecated" "&raw_output" "&encrypt" "&error_handler" "&expire_func" "&log" "&mergeable" "&optional" "&persistent" "&priority" "&type_column" "&read_expire" "&redef" "&rotate_interval" "&rotate_size" "&synchronized" "&write_expire"))"
;
; operators
; (regexp-opt '("--" "++" "+=" "-=" "==" "!=" ">=" "<=" "&&" "||" "?$" "$"))
;
; preprocessor
; (regexp-opt '("@DEBUG" "@DIR" "@FILENAME" "@load" "@load-sigs" "@load-plugin" "@unload" "@prefixes" "@if" "@ifdef" "@ifndef" "@else" "@endif"))
;
; preprocessor (Broxygen)
; (regexp-opt '("##!" "##<" "##"))
;
; types
; (regexp-opt '("/tcp" "/udp" "/icmp" "/unknown"))

(defconst bro-font-lock-keywords
  (list
   '("\\(##[!<]?.*\\)$" 1 font-lock-preprocessor-face)
   '("\\(#.*\\)$" 1 font-lock-comment-face)
   '(" \\(of\\) " 1 font-lock-builtin-face)
   '("\\(const\\|global\\|local\\) \\(\sw+\\)" 2 font-lock-variable-name-face)
   '("^\\(event\\|function\\) \\(\\sw+\\)(" 2 font-lock-function-name-face)
   '("\\(\\sw+\\)(" 1 font-lock-function-name-face)
   '("redef record \\(\\sw+\\) " 1 font-lock-variable-name-face)
   '("^module \\(\\sw+\\);" 1 font-lock-keyword-face)
   '("\\(\\sw+\\)::" 1 font-lock-keyword-face)
   '("\\(\\sw+\\)::\\(\\sw+\\)" 2 font-lock-variable-name-face)
   '("^\\(event\\|function\\) \\(\\sw+\\)(\\(\\sw+\\):" 3 font-lock-variable-name-face)
   '("^\\(event\\|function\\) \\(\\sw+\\)(.*, \\(\\sw+\\):" 3 font-lock-variable-name-face)
   '("^\\(event\\|function\\) \\(\\sw+\\)(.*\\(\\sw+\\): \\(\\sw+\\)" 4 font-lock-type-face)
   '("\\(?:&\\(?:add_func\\|create_expire\\|de\\(?:fault\\|lete_func\\|precated\\)\\|e\\(?:ncrypt\\|rror_handler\\|xpire_func\\)\\|log\\|mergeable\\|optional\\|p\\(?:ersistent\\|riority\\)\\|r\\(?:aw_output\\|e\\(?:ad_expire\\|def\\)\\|otate_\\(?:interval\\|size\\)\\)\\|synchronized\\|type_column\\|write_expire\\)\\)" . font-lock-keyword-face)
   '("^\\(?:a\\(?:ddr?\\|ny\\)\\|b\\(?:ool\\|reak\\)\\|c\\(?:ase\\|o\\(?:nst\\|py\\|unt\\(?:er\\)?\\)\\)\\|d\\(?:e\\(?:fault\\|lete\\)\\|ouble\\)\\|e\\(?:lse\\|num\\|\\(?:ven\\|xpor\\)t\\)\\|f\\(?:allthrough\\|ile\\|or\\|unction\\)\\|global\\|hook\\|i\\(?:nt\\(?:erval\\)?\\|[fn]\\)\\|l\\(?:ist\\|ocal\\)\\|module\\|next\\|o\\(?:f\\|paque\\)\\|p\\(?:attern\\|\\(?:or\\|rin\\)t\\)\\|re\\(?:cord\\|def\\)\\|s\\(?:chedule\\|et\\|tring\\|ubnet\\|witch\\)\\|t\\(?:able\\|ime\\(?:out\\|r\\)?\\|ype\\)\\|union\\|vector\\|when\\)" . font-lock-type-face)
   '("\\s-\\(?:a\\(?:ddr?\\|ny\\)\\|b\\(?:ool\\|reak\\)\\|c\\(?:ase\\|o\\(?:nst\\|py\\|unt\\(?:er\\)?\\)\\)\\|d\\(?:e\\(?:fault\\|lete\\)\\|ouble\\)\\|e\\(?:lse\\|num\\|\\(?:ven\\|xpor\\)t\\)\\|f\\(?:allthrough\\|ile\\|or\\|unction\\)\\|global\\|hook\\|i\\(?:nt\\(?:erval\\)?\\|[fn]\\)\\|l\\(?:ist\\|ocal\\)\\|module\\|next\\|o\\(?:f\\|paque\\)\\|p\\(?:attern\\|\\(?:or\\|rin\\)t\\)\\|re\\(?:cord\\|def\\)\\|s\\(?:chedule\\|et\\|tring\\|ubnet\\|witch\\)\\|t\\(?:able\\|ime\\(?:out\\|r\\)?\\|ype\\)\\|union\\|vector\\|when\\)" . font-lock-type-face)
   '("\\(?:@\\(?:D\\(?:EBUG\\|IR\\)\\|FILENAME\\|e\\(?:lse\\|ndif\\)\\|if\\(?:n?def\\)?\\|load\\(?:-\\(?:plugin\\|sigs\\)\\)?\\|prefixes\\|unload\\)\\)" . font-lock-preprocessor-face)
   '("\\(?:!=\\|\\$\\|&&\\|\\+[+=]\\|-[=-]\\|<=\\|==\\|>=\\|\\?\\$\\|||\\)" . font-lock-builtin-face)
   '("[[:digit:]]+\\(/icmp\\|/tcp\\|/udp\\|/unknown\\)" 1 font-lock-type-face)
   '("^\\s-+\\(\\sw+\\):" 1 font-lock-variable-name-face)
   '("^\\s-+\\sw+:\\s-+\\(\\sw+\\)\\s-+\\(&\\|;\\)" 1 font-lock-type-face)
   '("^\\s-+type\\s-+\\(\\sw+\\):" 1 font-lock-variable-name-face)
   '("[0-9eE-+\\.]\s-*\\(day\\|days\\|hr\\|hrs\\|min\\|mins\\|sec\\|secs\\|msec\\|msecs\\|usec\\|usecs\\)" 1 font-lock-type-face)
   )
  "Highlighting for basic keywords")

(defun bro-indent-line ()
  (interactive)
  (beginning-of-line)
  (if (bobp) (indent-line-to 0) )
  (let ((not-indented t) (cur-indent 0))
    
    (if (looking-at ".*};?$")
	(progn
	  (setq cur-indent (- (current-indentation) default-tab-width))
	  (setq not-indented nil)))
    (save-excursion
      (while not-indented
	(if (bobp) (progn 
		     (indent-line-to 0)
		     (setq not-indented nil)))
	(if (looking-at ".*{$")
	    (progn
	      (forward-line -1)
	      (setq cur-indent (+ (current-indentation) default-tab-width))
	      (setq not-indented nil)))
	(forward-line -1) ;; go back a line
	(unless (or (looking-at "^[ \t]*$") (looking-at "^[ \t]*#+.*$"))
	  (if (and not-indented (looking-at ".*};?$"))
	      (progn
		(setq cur-indent (- (current-indentation) default-tab-width))
		(setq not-indented nil)))
	  (if (and not-indented (looking-at "\\s-*if.*[^{]\\s-*$"))
	      (progn
		(setq cur-indent (+ (current-indentation) default-tab-width))
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
                  '(bro-record-attr
                    (regexp . "^\\s-+\\sw+:\\(\\s-+\\)?xre\\sw+\\(\\s-+\\)&.*$")
                    (valid lambda nil (and (looking-back ".*:.*$") (not (or (looking-back "^\\s-+#.*$") (looking-back "^\\s-+type ")))))
                    (group 1 2)
                    (spacing 1 1)
                    (modes . (list 'bro-mode))
                    ))
     (add-to-list 'align-rules-list
                  '(bro-record-no-attr
                    (regexp . "^\\s-+\\sw+:\\(\\s-*\\)\\sw+")
;                    (valid lambda nil (and (looking-back ".*:.*$") (not (or (looking-back "^\\s-+#.*$") (looking-back "^\\s-+type ")))))
                    (group 1)
                    (spacing 1)
                    (modes . (list 'bro-mode))
                    ))
     (defadvice align (around smart-tabs activate)
       (let ((indent-tabs-mode nil)) ad-do-it))

     (defadvice align-current (around smart-tabs activate)
       (let ((indent-tabs-mode nil)) ad-do-it))
     )
)

(defun bro-mode ()
  "Major mode for editing Bro scripts"
  (interactive)
  (kill-all-local-variables)
  (setq-local indent-line-function 'bro-indent-line)

;  (use-local-map bro-mode-map)

;  (set-syntax-table bro-mode-syntax-table)
  (setq major-mode 'bro-mode)
  (setq mode-name "Bro Script")

;  (setq-local comment-start "#")
;  (require 'wisent-bro-wy)
 ; (load-library 'wisent-bro-wy)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults (list 'bro-font-lock-keywords nil))
  (run-hooks 'bro-mode-hook))

(provide 'bro-mode)


;; END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the code from some old Bro modes that this was shamelessly stolen from
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; indenting
;; (defun bro-indent-line ()
;;   "Indent current line as Bro code"
;;   (interactive)
;;   (beginning-of-line)
;;   (if (bobp)
;;       (progn
;;         (indent-line-to 0))
;;     (let ((not-indented t)
;;           (cur-indent 0))
;;       (save-excursion
;;         (while not-indented
;;           (forward-line -1)
;;           (if (bobp)
;;               (progn
;;                 1                (indent-line-to 0)
;;                 (setq not-indented nil))
;;             (unless (or (looking-at "^[ \t]*$") (looking-at "^[ \t]*#+.*$"))
;;               (if (looking-at "^[ \t]*};$")
;;                   (progn
;;                     (setq cur-indent (- (current-indentation) default-tab-width))
;;                     (setq not-indented nil)
;;                     )
;;                 (if (looking-at "^.*};$")
;;                     (progn
;;                       ;; line ending in '};'"
;;                       (setq cur-indent (current-indentation))
;;                       ;;(setq cur-indent (- (current-indentation) default-tab-width))
;;                       (setq not-indented nil))
;;                   (if (looking-at "^[ \t]*}$")
;;                       (progn
;;                         ;; blank line ending in '}"
;;                         (setq cur-indent (- (current-indentation) default-tab-width))
;;                         (setq not-indented nil))
;;                     (if (looking-at "^[ \t]*\\(^event\\|function\\|if\\|else\\|when\\|for\\|export\\|while\\|redef.*{\\|type.*\{\\)")
;;                         (progn
;;                           ;; an event, if, for, export, while or redef block
;;                           (setq cur-indent (+ (current-indentation) default-tab-width))
;;                           (setq not-indented nil))
;;                       (if (looking-at ".*;$")
;;                           (progn
;;                             ;; a line ending in a ";"
;;                             (save-excursion
;;                               (forward-line -1)
;;                               (if (looking-at ".*;$")
;;                                   (progn
;;                                     (setq cur-indent (current-indentation))
;;                                     (setq not-indented nil))
;;                                 (if (looking-at ".*,$")
;;                                     (progn
;;                                       ;; a line ending in a comma followed by a line ending in a semicolon
;;                                       (setq cur-indent (- (current-indentation) default-tab-width))
;;                                       (setq not-indented nil)))
;;                                 )
;;                               ) ;; _save excursion
;;                             ) ;; _progn
;;                         (if (looking-at ".*,$")
;;                             ;; first line not ending in semi-colon
;;                             (progn
;;                               (save-excursion
;;                                 (forward-line -1)
;;                                 (if (looking-at "^[ \t]*$\\|^[ \t]*{$\\|^[ \t]*#+.*$")
;;                                     ;; second line is an empty line
;;                                     (progn
;;                                       (forward-line)
;;                                       (setq cur-indent (+ (current-indentation) default-tab-width))
;;                                       (setq not-indented nil))
;;                                   (if (looking-at ".*,$")
;;                                       ;; second line not ending in semi-colon
;;                                       (progn
;;                                         ;;(setq cur-indent (+ (current-indentation) default-tab-width))
;;                                         (forward-line)
;;                                         (setq cur-indent (current-indentation))
;;                                         (setq not-indented nil))
;;                                     (progn
;;                                       (setq cur-indent (current-indentation))
;;                                       (setq not-indented nil)
;;                                       ) ;; _progn
;;                                     ) ;; _if ".*,$"
;;                                   ) ;; _if "^[ \t]*$"
;;                                 ) ;; _save-excursion
;;                               ) ;; _progn
;;                           (if (looking-at "[ \t]*{$")
;;                               (progn
;;                                 (setq cur-indent (current-indentation))
;;                                 (setq not-indented nil)
;;                                 ) ; _progn
;;                             (if (looking-at ".*[^,;]$")
;;                                 (progn
;;                                   (setq cur-indent (- (current-indentation) default-tab-width))
;;                                   (setq not-indented nil)
;;                                   ) ;; _progn
;;                               (if (bobp)
;;                                   (setq not-indented nil))))))))))))))
;;       (if (< cur-indent 0)
;;           (setq cur-indent 0))
;;       (if cur-indent
;;           (indent-line-to cur-indent)
;;         (indent-line-to 0)))))

;; (defvar bro-mode-syntax-table
;;   (let ((st (make-syntax-table)))
;;     (modify-syntax-entry ?_ "w" st)
;;     (modify-syntax-entry ?# "<" st)
;;     (modify-syntax-entry ?\n ">" st)
;;     st)
;;   "Syntax table for bro-mode")

;; (defun bro-mode ()
;;   "Major mode for eding Bro scripting files"
;;   (interactive)
;;   (kill-all-local-variables)
;;   (set-syntax-table bro-mode-syntax-table)
;;   (use-local-map bro-mode-map)
;;   (set (make-local-variable 'font-lock-defaults) '(bro-font-lock-keywords))
;;   (set (make-local-variable 'indent-line-function) 'bro-indent-line)
;;   (setq major-mode 'bro-mode)
;;   (setq mode-name "Bro")
;;   (run-hooks 'bro-mode-hook))

;; (defun bro-event-lookup ()
;;   "Retrieves the documentation for the event at point.

;; Requires that the bro-event-bif be set with a valid path and filename."
;;   (interactive)
;;   (let ( (bro-event-name (thing-at-point 'symbol))
;;          (start-pos)
;;          (end-pos)
;;          (bro-event-doc)
;;          (bro-event-buffer))
;;     (message "Looking for %s in %s" bro-event-name bro-event-bif)
;;     (if (file-exists-p bro-event-bif)
;;         (progn
;;           (message "Found valid event.bif file.")
;;           (setq bro-event-buffer (find-file-noselect bro-event-bif))
;;           (save-excursion
;;             ;; switch to a buffer with the event.bif.bro file
;;             (set-buffer bro-event-buffer)
;;             (beginning-of-buffer)
;;             ;; search for the string
;;             (if (search-forward (format
;;                                  "global %s: event"
;;                                  bro-event-name (beginning-of-buffer))
;;                                 nil 1)
;;                 (progn
;;                   (end-of-line)
;;                   (setq end-pos (point))
;;                   (re-search-backward "^[^#]" nil t 2)
;;                   (setq start-pos (+ 1 (point)))
;;                   (setq bro-event-doc (buffer-substring start-pos end-pos))
;;                   (with-output-to-temp-buffer "bro-event"
;;                     (princ (format "%s" bro-event-doc)))
;;                   (save-window-excursion
;;                     (save-excursion
;;                       (switch-to-buffer "bro-event")
;;                       (setq buffer-read-only nil)
;;                       (bro-mode)))
;;                   (kill-buffer bro-event-buffer))
;;               (message "unable to find the event specified"))))
;;       (message "Did not find valid event.bif file."))))

;; (defun bro-event-query (query)
;;   "Query for related events

;; Opens a new buffer with all global events that match the query"
;;   (interactive "sEvent Query: ")
;;   (let ((start-pos)
;;         (end-pos)
;;         (bro-query-buffer "bro-queries")
;;         (bro-query-results '()))
;;     (message "Looking for \"%s\" in %s" query bro-event-bif)
;;     (if (file-exists-p bro-event-bif)
;;         (progn
;;           (message "Found valid event.bif file.")
;;           (setq bro-event-buffer (find-file-noselect bro-event-bif))
;;           (save-excursion
;;             (set-buffer bro-event-buffer)
;;             (beginning-of-buffer)
;;             (while (re-search-forward (format "global .*%s.*: event" query) nil t)
;;               (progn
;;                 (beginning-of-line)
;;                 (setq start-pos (point))
;;                 (end-of-line)
;;                 (setq end-pos (point))
;;                 (add-to-list 'bro-query-results (buffer-substring start-pos end-pos))
;;                 (with-output-to-temp-buffer "bro-event-list"
;;                   (mapc
;;                    (lambda (x)
;;                      (princ (format "%s\n" x)))
;;                    bro-query-results))))
;;             (save-window-excursion
;;               (save-excursion
;;                 (switch-to-buffer "bro-event-list")
;;                 (setq buffer-read-only nil)
;;                 (bro-mode)
;;                 (toggle-truncate-lines))))
;;           (kill-buffer bro-event-buffer))
;;       (message "No valid event.bif found."))))

;; (defun bro-insert-event()
;;   "Insert the built-in function at point into the kill ring."
;;   (interactive)
;;   (let ((bro-event-builtin (buffer-substring
;;                             (line-beginning-position)
;;                             (line-end-position))))
;;     (beginning-of-line)
;;     (if (looking-at "^global.*event(.*).*;")
;;         (progn
;;           (kill-new (replace-regexp-in-string
;;                      "global \\(.*\\): event\\(.*)\\).*;"
;;                      "event \\1\\2\n\t{\n\t}"
;;                      bro-event-builtin))
;;           (message "Event sent to kill-ring"))
;;       (message "Not a bro built in event handler;"))))

;; (defun bro-run(tracefile sigfile)
;;   "Will run the entire buffer through bro.

;; Will ask for a tracefile(based on bro-tracefiles) and a signature file"
;;   (interactive "sTracefile: \nsSignature file: ")
;;   (shell-command (format "bro %s %s %s"
;;                          (if (equal tracefile "")
;;                              (concat " ")
;;                            (concat " -r " bro-tracefiles "/" tracefile))
;;                          (if (equal sigfile "")
;;                              (concat " ")
;;                            (concat " -s " sigfile))
;;                          (concat " -e '" (buffer-substring
;;                                           (point-min)
;;                                           (point-max)) "'")
;;                          )))

;; (defun bro-run-region(tracefile sigfile)
;;   "Will run the region through bro

;; Will ask for a tracefile(based on bro-tracefiles) and a signature file"
;;   (interactive "sTracefile: \nsSignature file: ")
;;   (shell-command (format "bro %s %s %s"
;;                          (if (equal tracefile "")
;;                              (concat " ")
;;                            (concat " -r " bro-tracefiles "/" tracefile))
;;                          (if (equal sigfile "")
;;                              (concat " ")
;;                            (concat " -s " sigfile))
;;                          (concat " -e '" (buffer-substring
;;                                           (region-beginning)
;;                                           (region-end)) "'"))))


;; (provide 'bro-mode)


;; ;; ;; A mode for the Bro policy language.
;; ;; ;;
;; ;; ;; Largely trial-and-errored from
;; ;; ;; - http://two-wugs.net/emacs/mode-tutorial.html
;; ;; ;; - http://users.ox.ac.uk/~wadh1342/gle-mode.el
;; ;; ;;
;; ;; ;; Written by Christian Kreibich
;; ;; ;; (clearly without any clue of Lisp/Emacs)
;; ;; ;;
;; ;; ;; Last update: Wed Aug  3 17:41:17 BST 2005


;; ;; (defvar bro-mode-hook nil)

;; ;; ;; (defvar bro-mode-map
;; ;; ;;   (let ((bro-mode-map (make-keymap)))
;; ;; ;;     (define-key bro-mode-map "\C-j" 'newline-and-indent)
;; ;; ;;     bro-mode-map)
;; ;; ;;   "Keymap for BRO major mode")

;; ;; (add-to-list 'auto-mode-alist '("\\.bro\\'" . bro-mode))


;; ;; ;; ---- Syntax Highlighting --------------------------------------------

;; ;; (defvar bro-mode-keywords
;; ;;   `(("\\(@.*$\\)" (0 font-lock-doc-face))
;; ;;     (,(concat "\\<"
;; ;;               (regexp-opt '("const" "redef") t)
;; ;;               "\\>") (0 font-lock-constant-face))
;; ;;     (,(concat "\\<"
;; ;;               (regexp-opt '("addr" "bool" "count" "counter" "double" "enum"
;; ;;                             "file" "int" "interval" "list" "net" "pattern"
;; ;;                             "port" "record" "set" "string" "subnet" "table"
;; ;;                             "timer" "time" "union" "vector") t)
;; ;;               "\\>") (0 font-lock-type-face))
;; ;;     (,(concat "\\<"
;; ;;               (regexp-opt '("add" "alarm" "any" "break" "case" "default"
;; ;;                             "delete" "else" "event" "export" "fmt" "for"
;; ;;                             "function" "global" "global_attr" "if" "in"
;; ;;                             "local" "match" "module" "next" "of" "print"
;; ;;                             "return" "schedule" "switch" "this" "type"
;; ;;                             "using") t)	      
;; ;;               "\\>") (0 font-lock-keyword-face))
;; ;;     (,(concat "\\<"
;; ;;               (regexp-opt '("day" "days" "hr" "hrs" "min" "mins" "sec" "secs"
;; ;;                             "msec" "msecs" "usec" "usecs") t)
;; ;;               "\\>") (0 font-lock-function-name-face))
;; ;;     ("\\(&[a-zA-Z_0-9]+\\)" (0 font-lock-builtin-face))
;; ;;     )
;; ;;   "Keyword hilghlighting specification for Bro mode")

;; ;; (font-lock-add-keywords 'bro-mode bro-mode-keywords)

;; ;; ;; ---- The Syntax Table -----------------------------------------------

;; ;; (defvar bro-mode-syntax-table
;; ;;   (let ((bro-mode-syntax-table (make-syntax-table)))

;; ;;     ;; Underscores and hyphens are valid parts of tokens
;; ;;     (modify-syntax-entry ?_ "w" bro-mode-syntax-table)
;; ;;     (modify-syntax-entry ?- "w" bro-mode-syntax-table)
;; ;;     (modify-syntax-entry ?. "w" bro-mode-syntax-table)
;; ;;     (modify-syntax-entry ?& "w" bro-mode-syntax-table)

;; ;;     ;; What's this? Obviously, it is the definition of a comment in Bro! :)
;; ;;     (modify-syntax-entry ?# "<" bro-mode-syntax-table)
;; ;;     (modify-syntax-entry ?\n ">" bro-mode-syntax-table)

;; ;;     bro-mode-syntax-table)
;; ;;   "Syntax table for Bro mode")


;; ;; ;; ---- Main definitions -----------------------------------------------

;; ;; (defun bro-mode ()
;; ;;   "Major mode for editing Bro policy files"
;; ;;   (interactive)
;; ;;   (kill-all-local-variables)
;; ;;   (set-syntax-table bro-mode-syntax-table)
;; ;;   (set (make-local-variable 'font-lock-defaults) '(bro-mode-keywords))
;; ;;   (setq major-mode 'bro-mode)
;; ;;   (setq mode-name "Bro")
;; ;;   (run-hooks 'bro-mode-hook))

;; ;; (provide 'bro-mode)
