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

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(electric-pair-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

(setq frame-title-format "emacs"
      inhibit-splash-screen t
      inhibit-startup-screen t
      custom-file "~/.emacs.d/custom.el"
      make-backup-files nil
      auto-save-default nil)


(load custom-file)

;;(use-package challenger-deep-theme :ensure t)
;;(use-package monokai-theme :ensure t)
;;(use-package material-theme :ensure t :config (load-theme 'material 'no-confirm))
;;(use-package arjen-grey-theme :ensure t)
(use-package nord-theme :ensure t)

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
      (tide-hl-identifier-mode t)
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
