(setq gc-cons-threshold (* 50 1000 1000))

(defun myemacs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'myemacs/display-startup-time)

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

(use-package doom-themes
  :defer t
  :init (load-theme 'doom-one t))

(set-frame-font "Iosevka-16" nil t)

(use-package ac-emoji
  :ensure t)

(use-package ligature
  :load-path "~/.emacs.d/ligatures/"
  :config
  (ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                                      "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
                                      "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
                                      "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
                                      "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
                                      "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
                                      ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
                                      "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
                                      "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
                                      "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
                                      "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
  (global-ligature-mode t))

(setq inhibit-startup-message t)

(tool-bar-mode -1)

(menu-bar-mode -1)

(scroll-bar-mode -1)

(setq ring-bell-function 'ignore)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq make-backup-files nil)
(setq auto-save-default nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook))

;; Set the title
(setq dashboard-banner-logo-title "Welcome to AAA Emacs ")

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package all-the-icons
  :ensure t)

(use-package nerd-icons
    :ensure t)

(setq line-number-mode t)
(setq column-number-mode t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(global-display-line-numbers-mode)
(global-hl-line-mode 1)

(use-package electric
  :ensure t
  :config
  (electric-pair-mode 1))

;; Enable company-mode globally with additional settings
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

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(use-package projectile
  :ensure t
  :init
  (projectile-mode 1))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(setq warning-minimum-level :emergency)

(setq auto-save-visited-file-name t)

(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(show-paren-mode 1)

(use-package rainbow-delimiters
   :ensure t
   :init
   (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package flycheck
   :ensure t)

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

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook 
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-deferred)
  :custom
  (lsp-diagnostics-provider :capf)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(project file symbols))
  (lsp-lens-enable nil)
  (lsp-disabled-clients '((python-mode . pyls)))
  :init
  (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
  :config)

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

(setq exec-path (append exec-path '("/usr/local/go/bin/go")))

(use-package eglot
      :ensure t
      :config
      (add-to-list 'eglot-server-programs '(go-mode . ("/usr/local/bin/gopls")))
      :hook ((go-mode . eglot-ensure)))

(setq gofmt-command "goimports") ; or "gofmt" for default
(add-hook 'before-save-hook 'gofmt-before-save)

;; Optional: Linter
(require 'flymake)
(defun go-flymake-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "golint" (list local-file))))
(add-to-list 'flymake-allowed-file-name-masks
             '("\\.go\\'" go-flymake-init))

(use-package go-eldoc
:ensure t
:config
(go-eldoc-setup))

(use-package go-guru
:ensure t
:config
(customize-set-variable 'go-guru-scope "...")
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(use-package gotest
:ensure t
:bind (:map go-mode-map
                ("C-c C-t p" . go-test-current-project)
                ("C-c C-t f" . go-test-current-file)
                ("C-c C-t ." . go-test-current-test)
                ("C-c r" . go-run))
:config
    (setq go-test-verbose t))

(use-package dap-mode
  :ensure t
  :hook ((go-mode . dap-mode)
         (go-mode . dap-ui-mode))
  :config
  (require 'dap-go))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cargo-mode rust-mode fzf telephone-line yasnippet-snippets which-key use-package terraform-mode sqlite3 slime-company rainbow-delimiters pyvenv python-mode projectile org-bullets lsp-pyright kubernetes k8s-mode json-mode ivy-rich helm gotest go-guru go-eldoc general forge flycheck exec-path-from-shell eglot doom-themes doom-modeline dockerfile-mode docker dired-single dired-sidebar dired-open dired-hide-dotfiles diminish dashboard dap-mode counsel company-shell company-go clj-refactor blacken beacon auto-package-update all-the-icons-dired ac-emoji)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
