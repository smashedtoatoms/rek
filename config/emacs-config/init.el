;; init.el --- das emacs config

;;; Commentary:

;;; Code:

;; Run GC when emacs is idle, or at 1GB, whichever happens first.
(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 2 t (lambda () (garbage-collect)))

;; Set up use-package
(require 'package)
(add-to-list 'package-archives
  '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Packages:

;; Exec path from shell (emacs does weird things with the path, this helps)
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-shell-name "/bin/bash")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("PATH"))))

;; Theme
(use-package zenburn-theme
  :ensure t
  :init
  (load-theme 'zenburn t))

;; Doom Modeline (make the modeline pretty)
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-lsp t))

;; Ace Window (switch windows quickly)
(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window))

;; Silver Searcher (search quickly)
(use-package ag
  :ensure t)

;; Browse Kill Ring (browse your kill ring quickly)
(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings))

;; Projectile (one emacs, many projects, somehow sane)
(use-package projectile
  :ensure t
  :bind
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/app/"))
  (setq projectile-globally-ignored-directories '("~/target")))

;; Company Mode (autocomplete)
(use-package company
  :ensure t
  :demand t
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-show-numbers t))

;; Counsel Projectile (project aware counsel)
(use-package counsel-projectile
  :ensure t)

;; Counsel (make finding things in emacs generally easier)
(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  :config
  (counsel-projectile-mode 1))

;; Ivy (more code completion assistance)
(use-package ivy
  :ensure t
  :bind
  ("C-s" . swiper)
  ("C-c C-r" . ivy-resume)
  :init
  (ivy-mode t)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

;; Swiper (more autocomplete)
(use-package swiper
  :ensure t)

;; Expand Region (make it easy to select words)
(use-package expand-region
  :ensure t
  :bind
  ("C-c n" . er/expand-region))

;; NeoTree (because I am a toddler who still needs tree view sometimes)
(use-package neotree
  :ensure t
  :bind ("C-x t" . neotree-toggle)
  :config
  (setq neo-autorefresh nil)
  (setq neo-window-fixed-size nil)
  (eval-after-load "neotree"
    '(add-to-list 'window-size-change-functions
     (lambda (frame)
       (let ((neo-window (neo-global--get-window)))
         (unless (null neo-window)
           (setq neo-window-width (window-width neo-window))))))))

;; Org Mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Magit Setup (maybe the whole reason to use emacs)
(use-package magit
  :ensure t
  :bind
  ("C-c g" . magit-status))

;; Git Gutter (again, I am a toddler)
(use-package git-gutter
  :ensure t
  :bind
  ("C-0" . git-gutter:next-hunk)
  ("C-9" . git-gutter:previous-hunk)
  :init
  (global-git-gutter-mode t)
  :config
  (setq git-gutter:modified-sign "✔")
  (setq git-gutter:added-sign "✔")
  (setq git-gutter:deleted-sign "✘"))

;; Flycheck (linting)
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; Show flycheck errors in popups
(use-package flycheck-pos-tip
  :ensure t
  :init
  (flycheck-pos-tip-mode))

;; Add shellcheck checks for shell scripting;
(add-hook 'sh-mode-hook 'flycheck-mode)

;; SmartParens (manage my parenthesis for me)
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  :bind
  ("C-c j" . sp-forward-slurp-sexp)
  ("C-c h" . sp-forward-barf-sexp)
  ("C-c k" . sp-unwrap-sexp)
  :hook
  ((typescript-mode js2-mode) . smartparens-mode)
  ((cider-repl-mode
    emacs-lisp-mode
    eval-expression-minibuffer-setup
    ielm-mode
    lisp-mode
    lisp-interaction-mode) . smartparens-strict-mode))
(autoload 'smartparens-strict-mode "smartparens"
  "Turn on pseudo-structural editing of Lisp code." t)

;; Rainbow Delimiters (make delimiters easy to see)
(use-package rainbow-delimiters
  :ensure t
  :init
  (rainbow-delimiters-mode-enable)
  :config
  (setq show-paren-mode 1))

;; Add node modules to path (node doesn't behave in emacs, this helps)
(use-package add-node-modules-path
  :ensure t
  :config
  (add-hook 'typescript-mode-hook 'add-node-modules-path)
  (add-hook 'javascript-mode-hook 'add-node-modules-path)
  (add-hook 'web-mode-hook 'add-node-modules-path))

;; Language Server (see readme to configure eslint)
(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-enable-file-watchers nil) ;; let's not index everything in existence and ruin the fans in my computer.
  (setq lsp-enable-on-type-formatting t)
  (setq lsp-before-save-edits t)
  (setq lsp-eslint-server-command
    `("node",
      (expand-file-name (car (last (file-expand-wildcards "~/.emacs.d/vscode-eslint/server/out/eslintServer.js"))))
      "--stdio")) ;; for whatever reason, tilde expansion breaks here, expand-file-name fixes it.
  :commands
  (lsp lsp-deferred))

;; Language Server UI Niceness
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil))

;; Prettier
(use-package prettier-js
  :ensure t)

;; Typescript Mode
(use-package typescript-mode
  :ensure t
  :bind
  ("C-x C-e" . ts-send-last-sexp)
  ("C-c C-b" . ts-send-buffer)
  :config
  (add-hook 'typescript-mode-hook 'lsp)
  (add-hook 'typescript-mode-hook 'smartparens-mode)
  (add-hook 'typescript-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'typescript-mode-hook 'add-node-modules-path)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'jest-test-mode)
  (setq typescript-indent-level 2)
  (setq typescript-fmt-on-save t)
  (setq typescript-fmt-tool 'prettier)
  (setq typescript-backend 'lsp))

;; Typescript Repl (make sure you have tsun installed, and start with 'run-ts')
(use-package ts-comint
  :ensure t)

;; Typescript/Javascript Test runner
(use-package jest-test-mode
  :ensure t
  :defer t
  :commands jest-test-mode)

;; Javascript Mode
(use-package js2-mode
  :ensure t
  :bind
  ("C-c e" . js-comint-send-last-sexp)
  ("C-c l" . js-comint-send-buffer)
  :config
  (add-hook 'js2-mode-hook 'lsp)
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'js2-mode-hook 'smartparens-mode)
  (add-hook 'js2-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook 'add-node-modules-path))

;; Javascript Repl
(use-package js-comint
  :ensure t)

;; Javascript refactoring tools
(use-package js2-refactor
  :ensure t)

;; Javascript xref (jump to definition)
(use-package xref-js2
  :ensure t)

;; Json Mode
(use-package json-mode
  :ensure t)

;; Web Mode
(use-package web-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'lsp)
  (add-hook 'web-mode-hook 'smartparens-mode)
  (add-hook 'web-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (setq typescript-indent-level 2)
  (setq typescript-fmt-on-save t)
  (setq typescript-fmt-tool 'prettier)
  (setq typescript-backend 'lsp))

;; Yaml Mode
(use-package yaml-mode
  :ensure t)

;; Markdown Mode
(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook 'smartparens-mode)
  (add-hook 'markdown-mode-hook 'rainbow-delimiters-mode))

;; Sly (CommonLisp setup)
(use-package sly
  :ensure t
  :init
  (load (expand-file-name "~/.roswell/helper.el"))
  :config
  (add-hook 'lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'subword-mode)
  (setq inferior-lisp-program "ros -Q run"))

;; Global Config:

;; Irritating random indention fix
(setq-default electric-indent-inhibit t)
(setq js-indent-level 2)
(setq c-basic-offset 2)

;; Javascript js2-mode default instead of js-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Move all customization information into its own file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :no-error)

;; Prompt before killing emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;; Make window adjust per pixel instead of per character
(setq frame-resize-pixelwise t)

;; Use a transparent menu bar.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Assumes a dark colorscheme.
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Removes file name from titlebar.
(setq frame-title-format nil)

;; Turn off GUI stuff from 1997.
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))
  (menu-bar-mode -1)

;; Highlight current line.
(global-hl-line-mode 1)

;; Add line number to status bar.
(setq line-number-mode t)

;; Add column number to status bar.
(setq column-number-mode t)

;; global line numbers.
(global-display-line-numbers-mode)

;; Go straight to scratch buffer on startup.
(setq inhibit-startup-message t)

;; Always select the help buffer on open.
(setq help-window-select t)

;; Set fill column to 80 characters.
(setq-default fill-column 80)

;; Make sure that text files are correctly formatted.
(setq require-final-newline t)

;; Remove trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Changes all yes/no questions to y/n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable backup files.
(setq make-backup-files nil)

;; put emacs auto-saves into a directory dedicated to auto-saves
(unless (file-exists-p "~/.emacs.d/auto-save") (make-directory "~/.emacs.d/auto-save"))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-save/" t)))

;; Disable lock files.
(setq create-lockfiles nil)

;; Disable the bell.
(setq ring-bell-function 'ignore)

;; Don't use hard tabs; however, if tabs are there, make them 2 characters
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Configure kill-ring to integrate with copy/paste.
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t)

;; Shows a list of buffers with ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Use better binding for goto-line
(global-set-key (kbd "M-g") 'goto-line)

;; Delete selected if I start typing
(delete-selection-mode 1)

;; Enable mouse in terminal
(xterm-mouse-mode 1)

;; Use "home row" (e.g., a, s , d, f) to jump between windows.
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; Custom functions
(global-set-key (kbd "C-c w") 'toggle-truncate-lines)

;; Dired disable ls on mac
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;;; init.el ends here
