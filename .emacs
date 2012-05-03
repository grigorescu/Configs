(define-key key-translation-map [?\C-h] [?\C-?])
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(global-font-lock-mode 1)
(setq make-backup-files nil)
(setq delete-auto-save-files t)
(setq auto-save-list-files-prefix nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)          
(setq inhibit-startup-echo-area-message "vlad")
(setq initial-scratch-message nil)
(column-number-mode t)
(setq current-language-environment "UTF-8")

;; Reload .emacs

(defun reload-dot-emacs ()
  "Save the .emacs buffer if needed, then reload .emacs."
  (interactive)
  (let ((dot-emacs "~/.emacs"))
    (and (get-file-buffer dot-emacs)
         (save-buffer (get-file-buffer dot-emacs)))
    (load-file dot-emacs))
  (message "Re-initialized!"))

;; Mail
;;;;;;;

(setq user-mail-address "vladg@illinois.edu")
(setq user-full-name "Vlad Grigorescu")

(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(preview-reference-face ((t (:background "white" :foreground "black")))))
