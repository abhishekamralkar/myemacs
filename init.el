(setq user-full-name "Abhishek Anand Amralkar"
  user-mail-address "abhishekamralkar@gmail.com")

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
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(setq gc-cons-threshold (* 50 1000 1000))

(setq inhibit-startup-message t)   ;no startup screen
(tool-bar-mode -1)                 ;no toolbar
(menu-bar-mode -1)                 ;no menubar
(scroll-bar-mode -1)               ;no scrollbar
(setq ring-bell-function 'ignore)  ;no ringing bells
(defalias 'yes-or-no-p 'y-or-n-p)
(setq line-number-mode t)
(setq column-number-mode t)
(global-display-line-numbers-mode)
(global-hl-line-mode 1)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq make-backup-files nil)
(setq auto-save-default nil)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package treemacs
  :ensure t
  :bind ("C-c t" . treemacs)
  :custom
  (treemacs-is-never-other-window t)
  :hook
  (treemacs-mode . treemacs-project-follow-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (column-number-mode 1)
  :custom (
  (doom-modeline-height 15)
  (doom-modeline-window-width-limit nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-env-python-executable "python")
  (doom-modeline-env-go-executable "go")))

(use-package all-the-icons
  :ensure t)

(use-package nerd-icons
    :ensure t)

(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook))

(setq dashboard-banner-logo-title "Welcome to AAA Emacs ")
(setq dashboard-center-content t)
(setq dashboard-show-shortcuts nil)
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-owl :no-confirm))

(set-face-attribute 'default nil :family "FiraCode Nerd Font" :height 130)
(set-face-attribute 'italic nil :family "Hack")

(use-package ac-emoji
  :ensure t)

(use-package ligature
  :config
  (ligature-set-ligatures 't '("www"))
  ;; `variable-pitch' face
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(setq warning-minimum-level :emergency)

(setq auto-save-default t)
;; Set the interval between auto-saves based on time (in seconds)
(setq auto-save-timeout 10)  ;; Save every 20 seconds of idle time

(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;;(add-hook 'org-mode-hook
;;          (lambda ()
;;            (add-hook 'before-save-hook 'org-babel-tangle nil t)))

(show-paren-mode 1)

(use-package rainbow-delimiters
   :ensure t
   :init
   (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package yasnippet
   :ensure t
   :config
     (use-package yasnippet-snippets
       :ensure t)
     (yas-reload-all))

(use-package magit
     :ensure t
     :bind ("C-x g" . magit))

(use-package sqlite3
  :ensure t)

  (use-package forge
     :ensure t
     :after magit)

(use-package projectile
   :ensure t
   :init
     (projectile-mode 1))

(use-package general
   :ensure t)

(use-package fzf
  :bind
    ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package electric
  :ensure t
  :config
  (electric-pair-mode 1))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t))

(use-package helm
  :ensure t
  :bind
  ("C-x C-f" . 'helm-find-files)
  ("C-x C-b" . 'helm-buffers-list)
  ("M-x" . 'helm-M-x)
  :config
  (defun daedreth/helm-hide-minibuffer ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook 'daedreth/helm-hide-minibuffer)
  (setq helm-autoresize-max-height 0
        helm-autoresize-min-height 40
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-split-window-in-side-p nil
        helm-move-to-line-cycle-in-source nil
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8 
        helm-echo-input-in-header-line t)
  :init
  (helm-mode 1))

(helm-autoresize-mode 1)
(define-key helm-find-files-map (kbd "C-b") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "C-f") 'helm-execute-persistent-action)

(use-package eglot
  :ensure t
  :defer t
  :hook
  (go-mode . eglot-ensure)
  (python-mode . eglot-ensure))

;; Tree-sitter for enhanced syntax highlighting
(use-package tree-sitter
  :hook ((python-mode . tree-sitter-mode)
           (python-mode . tree-sitter-hl-mode)
           (go-mode . tree-sitter-mode)
           (go-mode . tree-sitter-hl-mode)
           (rust-mode . tree-sitter-mode)
           (rust-mode . tree-sitter-hl-mode)))

    (use-package tree-sitter-langs
      :ensure t
      :after tree-sitter)

(use-package lsp-pyright
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.venvs/")
  :config
  ;; (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(use-package blacken
  :init
  (setq-default blacken-fast-unsafe t)
  (setq-default blacken-line-length 80))

(use-package python-mode
  :hook
  (python-mode . pyvenv-mode)
  (python-mode . flycheck-mode)
  (python-mode . company-mode)
  (python-mode . blacken-mode)
  (python-mode . yas-minor-mode)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  :config)

(use-package go-mode
    :ensure t)

(setq exec-path (append exec-path '("/usr/local/go/bin/go")))

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

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

(add-hook 'shell-mode-hook 'yas-minor-mode)
(add-hook 'shell-mode-hook 'flycheck-mode)
(add-hook 'shell-mode-hook 'company-mode)

(defun shell-mode-company-init ()
  (setq-local company-backends '((company-shell
                                  company-shell-env
                                  company-etags
                                  company-dabbrev-code))))

(use-package company-shell
  :ensure t
  :config
    (require 'company)
    (add-hook 'shell-mode-hook 'shell-mode-company-init))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :ensure t
  :init
    (require 'company)
    (slime-setup '(slime-fancy slime-company)))

(use-package rust-mode
  :ensure t
  :hook
  (rust-mode . eglot-ensure)
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(setq org-ellipsis " ")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-export-with-smart-quotes t)
(setq org-src-window-setup 'current-window)
(add-hook 'org-mode-hook 'org-indent-mode)

(add-hook 'org-mode-hook
            (lambda ()
               (visual-line-mode 1)))

(use-package diminish
    :ensure t
    :init
    (diminish 'which-key-mode)
    (diminish 'linum-relative-mode)
    (diminish 'hungry-delete-mode)
    (diminish 'visual-line-mode)
    (diminish 'subword-mode)
    (diminish 'beacon-mode)
    (diminish 'irony-mode)
    (diminish 'page-break-lines-mode)
    (diminish 'auto-revert-mode)
    (diminish 'rainbow-delimiters-mode)
    (diminish 'rainbow-mode)
    (diminish 'yas-minor-mode)
    (diminish 'flycheck-mode)
    (diminish 'helm-mode))

(use-package json-mode
   :ensure t
   :config
   (customize-set-variable 'json-mode-hook
                             (lambda ()
                                 (setq tab-width 2))))

(use-package yaml-mode
     :ensure t)

(use-package docker
     :ensure t
     :bind (("C-c d c" . docker-containers)
            ("C-c d i" . docker-images)))

(use-package dockerfile-mode
    :ensure t)

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package k8s-mode
  :ensure t
  :hook (k8s-mode . yas-minor-mode))

(use-package terraform-mode
    :ensure t)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))
