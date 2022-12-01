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

(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook #'org-babel-tangle
                          :append :local)))

(use-package zerodark-theme
  :ensure t
  :init
    (load-theme 'zerodark t))

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

(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(when window-system (add-hook 'prog-mode-hook 'hl-line-mode))

(when window-system
      (use-package pretty-mode
      :ensure t
      :config
      (global-pretty-mode t)))

(setq make-backup-files nil)
(setq auto-save-default nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package async
  :ensure t
  :init (dired-async-mode 1))

(use-package command-log-mode
  :commands command-log-mode)

(use-package projectile
  :ensure t
  :init
    (projectile-mode 1))

(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
    (setq spaceline-buffer-encoding-abbrev-p nil)
    (setq spaceline-line-column-p nil)
    (setq spaceline-line-p nil)
    (setq powerline-default-separator (quote arrow))
    (spaceline-spacemacs-theme))

(setq powerline-default-separator nil)

(setq line-number-mode t)
(setq column-number-mode t)

(setq display-time-24hr-format t)
(setq display-time-format "%H:%M - %d %B %Y")
(display-time-mode 1)

(use-package fancy-battery
  :ensure t
  :config
    (setq fancy-battery-show-percentage t)
    (setq battery-update-interval 15)
    (if window-system
      (fancy-battery-mode)
      (display-battery-mode)))

(use-package ivy
  :ensure t)
(setq scroll-conservatively 100)

(use-package which-key
  :ensure t
  :config
    (which-key-mode))

(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper))

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

(require 'helm-config)    
(helm-autoresize-mode 1)
(define-key helm-find-files-map (kbd "C-b") 'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "C-f") 'helm-execute-persistent-action)

(setq electric-pair-pairs '(
                           (?\{ . ?\})
                           (?\( . ?\))
                           (?\[ . ?\])
                           (?\" . ?\")
                           ))   
(electric-pair-mode t)

(use-package beacon
  :ensure t
  :config
    (beacon-mode 1))

(show-paren-mode 1)

(use-package rainbow-delimiters
  :ensure t
  :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package yasnippet
  :ensure t
  :config
    (use-package yasnippet-snippets
      :ensure t)
    (yas-reload-all))

(use-package flycheck
  :ensure t)

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

(defun config-edit ()
  (interactive)
  (find-file "~/.emacs.d/emacs.org"))
(global-set-key (kbd "C-c e") 'config-edit)

(use-package general
  :ensure t)

(defun config-reload ()
  "Reloads ~/.emacs.d/emacs.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/emacs.org")))
(global-set-key (kbd "C-c r") 'config-reload)

(use-package dap-mode
   :commands dap-debug
   :config
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

(use-package magit
  :ensure t
  :bind ("C-x g" . magit))
  
(use-package forge
  :ensure t
  :after magit)

(setq exec-path (append exec-path '("/usr/local/go/bin/go")))
(setq exec-path (append exec-path '("/home/aaa/Code/golang/bin/gopls")))

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

(defun set-exec-path-from-shell-PATH ()
     (let ((path-from-shell (replace-regexp-in-string
                    "[ \t\n]*$"
                       ""
                       (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
   (setenv "PATH" path-from-shell)
   (setq eshell-path-env path-from-shell) ; for eshell users
   (setq exec-path (split-string path-from-shell path-separator))))
 
  (when window-system (set-exec-path-from-shell-PATH))
  (setenv "GOPATH" "/home/aaa/golang/src/github.com/abhishekamralkar/")

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
	    '(lambda ()
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
