;; Look and feel

(column-number-mode t)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)          
(setq inhibit-startup-echo-area-message "vlad")
(setq initial-scratch-message nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(if (member "Source Code Pro" (font-family-list))
    (set-face-attribute
     'default nil :font "Source Code Pro 9"))

;; Remove boxes from Powerline arrows
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;; show column number and line number
(dolist (mode '(column-number-mode line-number-mode))
  (when (fboundp mode) (funcall mode t)))

(require 'bro-mode)
(require 'pac-mode)

(dolist (mode-hook '(text-mode-hook prog-mode-hook conf-mode-hook))
  (add-hook mode-hook
            (lambda ()
              (linum-mode 1))))

;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)
;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)
;; Use less bolding
(setq solarized-use-less-bold t)
;; Use more italics
(setq solarized-use-more-italic t)
;; Use less colors for indicators such as git:gutter, flycheck and similar.
(setq solarized-emphasize-indicators nil)

(setq x-underline-at-descent-line t)

;; make the fringe thinner (default is 8 in pixels)
(fringe-mode 4)

;; show parenthesis match
(show-paren-mode 1)
(setq show-paren-style 'parenthesis) 

;; Toggle line highlighting in all buffers
(global-hl-line-mode t)
;; And line numbers
(global-linum-mode t)

(setq current-language-environment "UTF-8")
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(preview-reference-face ((t (:background "white" :foreground "black")))))

;; Behavior

(global-font-lock-mode 1)
(setq make-backup-files nil)
(setq delete-auto-save-files t)
(setq auto-save-list-files-prefix nil)

(global-set-key (kbd "C-x C-[") 'align-current)
(global-set-key (kbd "RET") 'align-newline-and-indent)

;; Includes

; Global modes

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/elpa/")

(require 'smart-tabs-mode)
(require 'helm-config)
(require 'helm-themes)
(helm-mode 1)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-company-mode)

(require 'keychain-environment)
(keychain-refresh-environment)

(defconst my-mode-line-buffer-identification
  (list
   '(:eval
     (let ((host-name
	    (if (file-remote-p default-directory)
		(tramp-file-name-host
		 (tramp-dissect-file-name default-directory))
	      (system-name))))
       (if (string-match "^[^0-9][^.]*\\(\\..*\\)" host-name)
	   (substring host-name 0 (match-beginning 1))
	 host-name)))
   ": %12b"))

(setq-default
 mode-line-buffer-identification
 my-mode-line-buffer-identification)

(add-hook
 'dired-mode-hook
 (lambda ()
   (setq
    mode-line-buffer-identification
    my-mode-line-buffer-identification)))

; Modes for syntax or certain file extensions

(require 'bro-mode)
(require 'pac-mode)

;(setq indent-tabs-mode nil)
(add-hook 'pac-mode-hook
	  (lambda () (progn (setq smart-tabs-mode t) (setq indent-tabs-mode t))))
(add-hook 'bro-mode-hook
	  (lambda () (setq indent-tabs-mode t)))
(add-hook 'c-mode-common-hook
	  (lambda () (progn (setq smart-tabs-mode t) (setq indent-tabs-mode t))))          

(setq c-default-style "whitesmith")

(autoload 'smart-tabs-mode "smart-tabs-mode"
  "Intelligently indent with tabs, align with spaces!")
(autoload 'smart-tabs-mode-enable "smart-tabs-mode")
(autoload 'smart-tabs-advice "smart-tabs-mode")
(autoload 'smart-tabs-insinuate "smart-tabs-mode")

(smart-tabs-add-language-support bro bro-mode-hook
  ((bro-indent-line . default-tab-width)))

(smart-tabs-add-language-support pac pac-mode-hook
  ((pac-indent-line . default-tab-width)))

(smart-tabs-insinuate 'c 'c++)

(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'"     . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))
(setq markdown-command "/usr/bin/markdown_py")

; Modes that add other functionality

(require 'multi-term)
(setq multi-term-program "/bin/zsh")

;; Other configuration

(setq user-mail-address "vlad@broala.com")
(setq user-full-name "Vlad Grigorescu")
(setq smart-tabs-mode t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

(require 'color-theme-sanityinc-tomorrow)
;; Functions

; Reload .emacs
(defun reload-dot-emacs ()
  "Save the .emacs buffer if needed, then reload .emacs."
  (interactive)
  (let ((dot-emacs "~/.emacs"))
    (and (get-file-buffer dot-emacs)
         (save-buffer (get-file-buffer dot-emacs)))
    (load-file dot-emacs))
  (message "Re-initialized!"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))))
