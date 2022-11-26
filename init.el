(setq user-full-name "Abhishek Anand Amralkar"
      user-mail-address "abhishekamralkar@gmail.com")

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
(package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
(package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(require 'org-tempo)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(setq inhibit-startup-message 1)
(set-buffer-modified-p 1)
(tool-bar-mode 0)
(tooltip-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode 0)
(show-paren-mode 1)
(setq require-final-newline 1)
(setq display-time-24hr-format 1)
(display-time-mode +1)
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)
;; disable the annoying bell ring
(setq ring-bell-function 'ignore)
;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
    scroll-conservatively 100000
    scroll-preserve-screen-position 1)

(setq frame-title-format
    '((:eval (if (buffer-file-name)
                 (abbreviate-file-name (buffer-file-name))
               "%b"))))

 (line-number-mode t)
 (column-number-mode t)
 (size-indication-mode t)
;; Keep all backup and auto-save files in one directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;; UTF-8 please
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; Turn off the blinking cursor
(blink-cursor-mode -1)
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
               term-mode-hook
               shell-mode-hook
               treemacs-mode-hook
               eshell-mode-hook
               vterm-mode-hook
               compilation-mode-hook))
 (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;;(use-package zenburn-theme
;;  :ensure t
;;  :config (load-theme 'zenburn t))
(use-package doom-themes
  :init (load-theme 'doom-material-dark t))
;;(use-package doom-themes
;;  :init (load-theme 'leuven t))

(font-family-list)
 (add-to-list 'default-frame-alist
       (cond
          ((string-equal system-type "darwin")    '(font . "Fira Code-14"))
          ((string-equal system-type "gnu/linux") '(font . "Fira Code-12"))))

(use-package ac-emoji
  :ensure t)

(set-fontset-font
t 'symbol
(font-spec :family "Symbola") nil 'prepend)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(winner-mode t)

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package general
  :ensure t)

(use-package ace-window
  :ensure t
  :init
   (progn
    (setq aw-scope 'global) ;; was frame
    (global-set-key (kbd "C-x O") 'other-frame)
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
    '(aw-leading-char-face
    ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

(use-package fzf :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
   :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
       :map minibuffer-local-map
       ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package swiper
  :ensure try
  :bind (("C-s" . swiper)
       ("C-r" . swiper)
       ("C-c C-r" . ivy-resume)
       ("M-x" . counsel-M-x)
       ("C-x C-f" . counsel-find-file))
  :config
(progn
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
))

(defun lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :after lsp)

(use-package dap-mode
 ;; Uncomment the config below if you want all UI panes to be hidden by default!
 ;; :custom
 ;; (lsp-enable-dap-auto-configure nil)
 ;; :config
 ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
 ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed

 ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
        ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
        (company-minimum-prefix-length 1)
        (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit))

(use-package forge
  :ensure t
  :after magit)

(setq exec-path (append exec-path '("/usr/local/go/bin/go")))
(setq exec-path (append exec-path '("/usr/bin/gopls")))

(defun lsp-go-install-save-hooks ()
   (add-hook 'before-save-hook #'lsp-format-buffer t t)
   (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode 
  :ensure t
  :config
  (add-hook 'go-mode-hook #'lsp)
  (require 'dap-dlv-go)

  (add-hook 'before-save-hook 'gofmt-before-save) ; run gofmt on each save
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook #'lsp-deferred))

(use-package go-eldoc
  :ensure t
  :config
  (go-eldoc-setup))

(use-package exec-path-from-shell
  :ensure t)

(use-package go-guru
  :ensure t
  :config
  ;; Search entire workspace
  (customize-set-variable 'go-guru-scope "...")
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(use-package company-go
  :ensure t
  :config
  (add-hook 'go-mode-hook (lambda ()
                         (set (make-local-variable 'company-backends)
                              '(company-go))
                         (company-mode))))

(use-package gotest
  :ensure t
  :bind (:map go-mode-map
           ("C-c C-t p" . go-test-current-project)
           ("C-c C-t f" . go-test-current-file)
           ("C-c C-t ." . go-test-current-test)
           ("C-c r" . go-run))
  :config
  (setq go-test-verbose t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package yasnippet
  :ensure t
  :init
 (yas-global-mode 1))

(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)
;;(global-electric-pair-mode t)

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(use-package clojure-mode
   :defer t
   :ensure t)

(use-package cider
  :ensure t)

(use-package clj-refactor
  :ensure t
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                (clj-refactor-mode 1)
                                ))
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (setq cljr-warn-on-eval nil)
   :bind ("C-c '" . hydra-cljr-help-menu/body))

(use-package racer
  :ensure t
  :config
  (add-hook 'racer-mode-hook #'company-mode)
  (setq company-tooltip-align-annotations t)
  (setq racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :config
  (setq compilation-scroll-output t)
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook 'flycheck-mode))

(use-package json-mode
  :ensure t
  :config
  (customize-set-variable 'json-mode-hook
                          #'(lambda ()
                              (setq tab-width 2))))

(use-package docker
  :ensure t
  :bind (("C-c d c" . docker-containers)
         ("C-c d i" . docker-images)))

(use-package dockerfile-mode
  :ensure t)

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package terraform-mode
   :ensure t)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
