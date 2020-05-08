;; Install `use-package`
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setf use-package-always-ensure t)

;; Disable startup message
(setq inhibit-startup-message t)
;; Disable bars
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1) 
;; Backups and auto-save in one directory
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms `((".*" (concat user-emacs-directory "saves") t)))
;; No lock files, sorry
(setq create-lockfiles nil)

;; Theme
(use-package base16-theme
  :config
  (load-theme 'base16-eighties t))

;; Which key
(use-package which-key
  :config
  (which-key-mode))

;; Ace jump mode
(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))

;; Counsel + Swiper + Ivy
(use-package counsel
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("M-y" . counsel-yank-pop)
	 :map ivy-minibuffer-map
         ("M-y" . ivy-next-line))
  :config (counsel-mode))

(use-package ivy
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode 1))

;; Projectile
(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-mode-line
        '(:eval (format " [%s]" (projectile-project-name))))
  (setq projectile-remember-window-configs t)
  (setq projectile-completion-system 'ivy))

;; Magit
(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :diminish auto-revert-mod)

;; Flycheck
(use-package flycheck)

;; Lsp + Dap 
(setq lsp-keymap-prefix "C-l")

(use-package lsp-mode
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-disabled-clients '(rls))
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(use-package company-lsp :commands company-lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode)

;; Rust
(use-package toml-mode)

(use-package rust-mode
  :hook (rust-mode . lsp))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-count-format "(%d/%d) ")
 '(ivy-use-virtual-buffers t)
 '(package-selected-packages
   (quote
    (ace-jump-mode which-key use-package toml-mode projectile magit lsp-ui lsp-ivy flycheck dap-mode counsel company-lsp cargo base16-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
