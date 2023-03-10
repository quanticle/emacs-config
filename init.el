;; -*- lexical-binding: t -*-

;; Global settings

;;; I never use emacs at the command line, and in GUI emacs C-z minimizes,
;;; which is an unpleasant surprise when you miss C-x and hit C-z by mistake
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;;; Ubind C-t (transpose characters). I have almost never intentionally used
;;; function, but I hit C-t accidentally all the time when reaching for C-r
;;; (reverse i-search)

(global-unset-key (kbd  "C-t"))

;;; I shouldn't have to set this, but Emacs is archaic and will use some random
;;; ASCII or Windows-1252 codepage if this isn't set
(prefer-coding-system 'utf-8)

;;; Set the font
(set-face-attribute 'default nil :font "Cascadia Code PL-12")
;;; This needs to be set to make inline code in org-mode not look awful
(set-face-attribute 'fixed-pitch nil :family 'unspecified)

;;; Make scrolling smooth
(setq scroll-conservatively 9999)

;;; Disable annoyances
(setq ring-bell-function 'ignore)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;;; Set the initial mode to text mode, as that's more useful than emacs-lisp
;;; mode
(setq inhibit-startup-message t)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "")

;;; Spaces > Tabs, but allow this to be overridden e.g. for Go
(setq-default indent-tabs-mode nil)

;;; If a block of text is selected, typing should overwrite the selected text,
;;; just like in other editors and word processors
(delete-selection-mode 1)

;;; Always show matching parens
(show-paren-mode 1)

;;; Show column number as well as line number in the mode line
(column-number-mode 1)

;;; Disable backup files, auto-save and lock files, since emacs rarely crashes
;;; these days, and everything I do is version controlled anyhow
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;;; Another thing that should be enabled by default. It's not 1999 any more.
;;; Disk I/O is cheap.
(global-auto-revert-mode 1)

;;; If we have multiple buffers open with the same name, use the directory to
;;; disambiguate
(setq uniquify-buffer-name-style 'forward)

;;; Make dired a bit nicer by allowing dired to kill the associated buffer when
;;; a file is deleted
(require 'dired-x)
(setq dired-clean-up-buffers-too t)

;;; Computers have lots of RAM these days, so emacs should be more relaxed about
;;; garbage collection. This sets the GC to run when either 1. 8GB of lisp 
;;; objects have been allocated since the last GC or 2. emacs has been idle for
;;; five minutes.
(setq gc-cons-threshold 8589934592)
(run-with-idle-timer 300 t 'garbage-collect)

;;; Don't show the warnings buffer unless there's an actual error
(setq display-warning-minimum-level :error)

;; Initialize the package manager
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnu-tls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)
(package-refresh-contents)
(if (not (package-installed-p 'use-package))
    (package-install 'use-package))

;; Packages
(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-count-format "(%d / %d) ")
  (setq enable-recursive-minibuffers t))

(use-package projectile
  :ensure t
  :after (ivy)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-enable-caching t)
  (projectile-mode 1))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :config
  (counsel-projectile-mode 1))

(use-package org
  :ensure t
  :bind (:map org-mode-map
         ("C-c >" . org-metaright)
         ("C-c <" . org-metaleft)
         ("C-c v" . org-metadown)
         ("C-c M-v". org-metaup))
  :config
  (setq org-startup-truncated nil)
  (setq org-startup-indented nil)
  (setq org-startup-folded nil)
  (setq org-startup-export-with-toc nil)
  (setq org-adapt-indentation nil)
  (setq org-yank-folded-subtrees nil)
  (add-hook 'org-mode-hook
            (lambda ()
              (electric-indent-mode -1))))

(use-package magit
  :ensure t
  :config
  (setq vc-handled-backends nil))

(use-package cider
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'cider-mode))

(use-package company
  :ensure t
  :after (cider)
  :config
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'emacs-lisp-mode-hook #'company-mode))

(use-package paredit
  :ensure t
  :after (cider)
  :config
  (add-hook 'cider-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :config
  (add-hook 'js-mode-hook #'lsp))

(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))
