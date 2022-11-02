(setq user-full-name "Abhishek Anand Amralkar"
      user-mail-address "abhishekamralkar@gmail.com")

(defun org-babel-tangle-config() 
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/emacs.org"))
    (let ((org-config-babel-evaluate nil))
      (org-babel-tangle))))

 (add-hook 'org-mode-hook
            (lambda ()
               (add-hook 'after-save-hook #'org-babel-tangle-config)))

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
(column-number-mode 0)
(show-paren-mode 1)
(setq require-final-newline 1)
(setq display-time-24hr-format 1)
(display-time-mode +1)
(setq redisplay-dont-pause 1
    scroll-margin 1
    scroll-step 1
    scroll-conservatively 10000
    scroll-preserve-screen-position 1)

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
;;(use-package sml/setup
;;   :ensure t)
;;(setq sml/theme 'respectful)
;;(setq sml/shorten-directory t)
;;(setq sml/shorten-modes t)
;;(use-package nyan-mode
;;  :ensure t)
;;(setq nyan-wavy-trail nil)
;;(setq nyan-animate-nyancat t)

;;(use-package zenburn-theme
;;:ensure t
;;:config (load-theme 'zenburn t))
(use-package doom-themes
  :init (load-theme 'doom-palenight t))
;;(use-package doom-themes
;;  :init (load-theme 'leuven t))

(font-family-list)
(add-to-list 'default-frame-alist
      (cond
         ((string-equal system-type "darwin")    '(font . "Fira Code-14"))
         ((string-equal system-type "gnu/linux") '(font . "Fira Code-12"))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

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

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

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
  :ensure t)

(use-package forge
  :ensure t
  :after magit)

;;(use-package lsp-mode
 ;; :ensure t
 ;; :config
 ;; (add-hook 'before-save-hook 'gofmt-before-save)
 ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
 ;; (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode 
  :ensure t
  :config
  (add-hook 'go-mode-hook #'lsp)
  (require 'dap-dlv-go)

  (add-hook 'before-save-hook 'gofmt-before-save) ; run gofmt on each save
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook #'lsp-deferred))

(use-package exec-path-from-shell
  :ensure t)

(defun set-exec-path-from-shell-PATH ()
(let ((path-from-shell (replace-regexp-in-string
                        "[ \t\n]*$"
                        ""
                        (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
  (setenv "PATH" path-from-shell)
  (setq eshell-path-env path-from-shell) ; for eshell users
  (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))
(setenv "GOPATH" "~/golang/src/github.com/abhishekamralkar/")

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

(use-package cider
   :ensure t
   :config
     (add-hook 'cider-repl-mode-hook #'company-mode)
     (add-hook 'cider-mode-hook #'company-mode)
     (add-hook 'cider-mode-hook #'eldoc-mode)

     (setq cider-repl-use-pretty-printing t)
     (setq cider-repl-display-help-banner nil)


   :bind (("M-r" . cider-namespace-refresh)
          ("C-c r" . cider-repl-reset)
          ("C-c ." . cider-reset-test-run-tests)))

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

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
