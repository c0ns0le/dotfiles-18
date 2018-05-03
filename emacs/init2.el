
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(org-babel-load-file "~/.emacs.d/dotemacs.org")

;; my default configs



(defun open-eshell-below (new)
  (interactive)
  (let ((w (split-window-below -10)))
    (select-window w)
    (let ((eshell-buffer (eshell new)))
      (switch-to-buffer eshell-buffer))))

(global-set-key (kbd "C-x m") (lambda () (interactive) (open-eshell-below nil)))
(global-set-key (kbd "C-x M") (lambda () (interactive) (open-eshell-below t)))

(global-set-key (kbd "C-c v") 'eval-buffer)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

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

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

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

(use-package ido
  :disabled t
  :init
  (setq ido-enable-flex-matching t
	ido-everywhere t
	ido-create-new-buffer 'always)
  :config
  (ido-mode t))

(use-package smex
  :disabled t
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; https://www.reddit.com/r/emacs/comments/51lqn9/helm_or_ivy/
(use-package ivy
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
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

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
    :keybinding "g")
  (defengine melpa
    "http://melpa.org/#/?q=%s"
    :keybinding "m"))

(use-package magit
  :ensure t
  :bind ("C-c g" . magit))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package less-css-mode
  :ensure t)

(use-package elm-mode
  :ensure t)

(use-package npm-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package org
  :disabled t
  :ensure t
  :pin org
  :config
  (setq org-todo-keywords
	'((sequence "TODO" "IN PROGRESS" "DONE")))
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (setq org-confirm-babel-evaluate nil
	org-src-fontify-natively t
	org-src-tab-acts-natively t)
  (use-package ob-python)
  (use-package ob-css)
  (use-package ob-js)
  (use-package ob-css)
  (use-package ob-sql)
  (use-package ob-typescript
    :ensure t)
  (use-package ob-sh
    :config
    (defadvice org-babel-sh-execute (around set-shell activate)
      "Add header argument :shcmd that determines which shell to call."
      (let* ((org-babel-sh-command (or (cdr (assoc :shcmd params)) org-babel-sh-command)))
	ad-do-it))))

;; org mode



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
