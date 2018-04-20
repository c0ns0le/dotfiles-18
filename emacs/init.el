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
      custom-file "~/.emacs.d/custom.el")


(load custom-file)


;;(use-package challenger-deep-theme :ensure t)
(use-package monokai-theme
  :ensure t)

(use-package company
  :ensure t
  :config
  (progn
    ;; align annotations to the right
    (setq company-tooltip-align-annotations t)
    (global-company-mode t)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package tide
  :ensure t
  :hook (typescript-mode . setup-tide-mode)
  :preface
  (progn
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (flycheck-mode t)
      (eldoc-mode t)
      (company-mode t)
      (tide-hl-identifier-mode t)
      (setq flycheck-idle-change-delay 2)
      (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change)))))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode))

(use-package add-node-modules-path
  :hook typescript-mode
  :ensure t)

(use-package prettier-js
  :hook typescript-mode
  :ensure t)
