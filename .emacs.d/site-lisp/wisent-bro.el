;(require 'semantic-wisent)
(require 'wisent-bro-wy)
;(require 'semantic-bro)
(eval-when-compile
  (require 'semantic-util)
  (require 'semantic-ctxt)
  (require 'semantic-imenu)
  (require 'senator))

;;; Enable Semantic in `bro-mode'.
;;
(defun wisent-bro-init-parser-context ()
  "Initialize context of the LR parser engine.
Used as a local `wisent-pre-parse-hook' to cleanup the stack of enum
names in scope."
  (setq wisent-bro-wy--enums nil))

(defun wisent-bro-default-setup ()
  "Hook run to setup Semantic in `bro-mode'."
  ;; Use the Wisent LALR(1) parser to analyze Bro sources.
  (wisent-bro-wy--install-parser)
  (semantic-make-local-hook 'wisent-pre-parse-hook)
  (add-hook 'wisent-pre-parse-hook
            'wisent-bro-init-parser-context nil t)
  (setq
   ;; Lexical analysis
   semantic-lex-number-expression semantic-bro-number-regexp
   semantic-lex-depth nil
   semantic-lex-analyzer 'wisent-bro-lexer
   ;; Parsing
   semantic-tag-expand-function 'semantic-bro-expand-tag
   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-prototype
   semantic-imenu-expandable-tag-classes '(type variable)
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-type-relation-separator-character '(".")
   semantic-command-separation-character ";"
   ;; speedbar and imenu buckets name
   semantic-symbol->name-assoc-list-for-type-parts
   ;; in type parts
   '((type     . "Classes")
     (variable . "Variables")
     (function . "Methods"))
   semantic-symbol->name-assoc-list
   ;; everywhere
   (append semantic-symbol->name-assoc-list-for-type-parts
           '((include  . "Imports")
             (package  . "Package")))
   ;; navigation inside 'type children
   senator-step-at-tag-classes '(function variable)
   )
  ;; Setup brodoc stuff
  (semantic-bro-doc-setup))

(add-hook 'bro-mode-hook 'wisent-bro-default-setup)

;;; Overridden Semantic API.
;;
(define-mode-local-override semantic-tag-components bro-mode (tag)
  "Return a list of components for TAG."
  (if (semantic-tag-of-class-p tag 'function)
      (semantic-tag-function-arguments tag)
    ;; Simply return the value of the :members attribute.
    (semantic-tag-get-attribute tag :members)))

(define-mode-local-override semantic-get-local-variables
  bro-mode ()
  "Get local variable declarations from the current context."
  (let (result
        ;; Ignore funny syntax while doing this.
        semantic-unmatched-syntax-hook)
    (while (not (semantic-up-context (point) 'function))
      (save-excursion
        (forward-char 1)
        (push (semantic-parse-region
               (point)
               (save-excursion (semantic-end-of-context) (point))
               ;; See this production in wisent-bro.wy.
               'block_statement
               nil t)
              result)))
    (apply 'append result)))

(provide 'wisent-bro)

