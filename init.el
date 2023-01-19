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

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(font-family-list)
(add-to-list 'default-frame-alist
     (cond
         ((string-equal system-type "darwin")    '(font . "Fira Code-12"))
         ((string-equal system-type "gnu/linux") '(font . "Fira Code-10"))))

(use-package ac-emoji
  :ensure t)

(set-fontset-font
  t 'symbol
(font-spec :family "Symbola") nil 'prepend)

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
(setq dashboard-banner-logo-title "Welcome to AAA Emacs Dashboard")

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
