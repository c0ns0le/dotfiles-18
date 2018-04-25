(require 'package)
(package-initialize)

(defvar is-windows (eq system-type 'windows-nt))

(if is-windows
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; my default configs
(when is-windows
  (set-frame-font "Hack 11" nil t))

(when (not is-windows)
  (set-frame-font "Hack 12" nil t))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(electric-pair-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
;;(desktop-save-mode t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq frame-title-format "emacs"
      inhibit-splash-screen t
      inhibit-startup-screen t
      custom-file "~/.emacs.d/custom.el"
      make-backup-files nil
      auto-save-default nil)


(load custom-file)

(defun open-eshell-below (new)
  (interactive)
  (let ((w (split-window-below -10)))
    (select-window w)
    (let ((eshell-buffer (eshell new)))
      (switch-to-buffer eshell-buffer))))

(global-set-key (kbd "C-x m") (lambda () (interactive) (open-eshell-below nil)))
(global-set-key (kbd "C-x M") (lambda () (interactive) (open-eshell-below t)))

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;;(use-package challenger-deep-theme :ensure t)
;;(use-package monokai-theme :ensure t)
;;(use-package material-theme :ensure t :config (load-theme 'material 'no-confirm))
;;(use-package arjen-grey-theme :ensure t)
;;(use-package nord-theme :ensure t :config (load-theme 'nord 'no-confirm))
;;(use-package doneburn-theme :ensure t :config (load-theme 'doneburn 'no-confirm))
;;(use-package base16-theme
;;  :ensure t
;;  :config
;;  (load-theme 'base16-eighties t))
(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-use-less-bold t)
  (load-theme 'solarized-light 'no-confirm)

  ;; get rid of bolds
  (set-face-attribute 'font-lock-constant-face nil :weight 'normal)

  ;; custom faces for modes
  (defface import-module-face
    '((t (:foreground "#dc322f")))
    "Face for the import, from, and as keywords in module import"
    :group 'typescript-color-faces)
  
  (font-lock-add-keywords
   'typescript-mode
   '(("import" . 'import-module-face)
     ("from" . 'import-module-face))))

(use-package zenburn-theme
  :ensure t
  :config
  (set-face-attribute 'fringe nil :background nil)
  (set-face-attribute 'linum nil :foreground "#90907e")
  (set-face-attribute 'font-lock-keyword-face nil :weight 'normal)
  (set-face-attribute 'font-lock-builtin-face nil :weight 'normal))

(use-package company
  :ensure t
  :diminish
  :config
  (progn
    ;; align annotations to the right
    (setq company-tooltip-align-annotations t)
    (global-company-mode t)))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

(use-package diminish :ensure t)
(use-package delight :ensure t)

(use-package tide
  :ensure t
  :hook (typescript-mode . setup-tide-mode)
  :preface
  (progn
    (defun setup-typescript-keybindings ()
      (define-key typescript-mode-map [f12] 'tide-goto-reference)
      (define-key typescript-mode-map [f11] 'tide-references)
      (define-key typescript-mode-map [f2] 'tide-rename-symbol))
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (flycheck-mode t)
      (eldoc-mode t)
      (company-mode t)
      (tide-hl-identifier-mode nil)
      (setq flycheck-idle-change-delay 2)
      (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
      (setup-typescript-keybindings))))

(use-package editorconfig
  :ensure t
  :diminish
  :config
  (editorconfig-mode))

(use-package add-node-modules-path
  :hook typescript-mode
  :ensure t)

(use-package prettier-js
  :ensure t
  :config
  (add-hook 'typescript-mode-hook 'prettier-js-mode))

(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode t))

(use-package ido
  :init
  (setq ido-enable-flex-matching t
	ido-everywhere t
	ido-create-new-buffer 'always)
  :config
  (ido-mode t))

(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; https://www.reddit.com/r/emacs/comments/51lqn9/helm_or_ivy/
(use-package ivy
  :disabled t
  :ensure t
  :diminish
  :config
  (ivy-mode t)
  ;; maybe don't use virtual buffers
  (setq ivy-count-format ""
	ivy-display-style nil
	ivy-minibuffer-faces nil)

  ;; add fuzzy matching
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done))

(use-package projectile
  :ensure t
  :config
  (projectile-mode))
  ;;(setq projectile-completion-system 'ivy))

(use-package engine-mode
  :ensure t
  :config
  (engine-mode t)
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine appsscript
    "https://developers.google.com/s/results/?q=%s&p=/apps-script/"
    :keybinding "a")
  (defengine github
    "https://github.com/search?utf8=✓&q=%s&type="
    :keybinding "g"))

(use-package magit
  :ensure t
  :bind ("C-c g" . magit))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defun compilation-finished (buffer msg)
  (remove-hook 'compilation-finish-functions 'compilation-finished)
  (if (string-match "^finished" msg)
      (progn
	(delete-windows-on buffer)
	(tooltip-show "\n Compiled successfully! :)"))
    (tooltip-show "\n Compile failed.. :("))
  (setq current-frame (car (car (cdr (current-frame-configuration)))))
  (select-frame-set-input-focus current-frame)
  (call-process "echo ^G"))

(defun deploy-addon ()
  (interactive)
  (compile "npm run deploy-addon:local" t)
  (add-hook 'compilation-finish-functions 'compilation-finished))

(global-set-key (kbd "C-c C-t r") 'deploy-addon)
