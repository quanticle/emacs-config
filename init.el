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
(setq warning-minimum-level :error)


;; Set Javascript indentation to two spaces
(setq js-indent-level 2)

;; Enable emacs to handle encrypted files
(require 'epa-file)
(epa-file-enable)

;; I don't like list-directory, so let's just use dired for both C-x C-d and
;; C-x d
(global-set-key (kbd "C-x C-d") 'dired)

;; I hit C-x C-c accidentally far too often for my liking, so add a confirmation
;; for that

(global-set-key (kbd "C-x C-c") (lambda ()
                                  (interactive)
                                  (if (y-or-n-p "Really quit?")
                                      (save-buffers-kill-terminal))))

;; Just save everything without asking
(global-set-key (kbd "C-x s") (lambda () (interactive) (save-some-buffers t)))

;; I don't double-space after periods
(setq sentence-end-double-space nil)

;; Command to allow me to rename frames
(defun rename-frame ()
  "Command to allow the user to rename frames. Prompts the user for a new frame
name. If the user passes 'default as the new frame name, this command resets the
name to the default value specified by FRAME-TITLE-FORMAT."
  (interactive)
  (let ((current-frame (selected-frame))
        (new-frame-name (read-string "New frame name: ")))
    (if (string-equal new-frame-name "'default")
        (modify-frame-parameters current-frame (list (cons 'name nil)))
      (modify-frame-parameters current-frame (list (cons 'name new-frame-name))))))

;; Enable upcase-region and downcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Don't include text properties when yanking
(setq yank-excluded-properties t)

;; Add org-mode source blocks, example blocks and LaTeX fragment delimiters to
;; the list of excluded regions for ispell, in order to limit the number of
;; false positives

(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC")) ; Code blocks
(add-to-list 'ispell-skip-region-alist '("[[:space:](]=[[:graph:]]" . "[[:graph:]]=[[:space:],.)]")) ; Code fragments
(add-to-list 'ispell-skip-region-alist '("https?" . "[[:space:]]")) ; URLs
(add-to-list 'ispell-skip-region-alist '("\\\\(" . "\\\\)")) ; LaTeX fragments
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE")) ; Example blocks
(add-to-list 'ispell-skip-region-alist '("#\\+OPTIONS" . "\n")) ; Options config
(add-to-list 'ispell-skip-region-alist '("#\\+HTML_HEAD" . "\n")) ; HTML config
(add-to-list 'ispell-skip-region-alist '("-\\*-" . "-\\*-")) ; Emacs per-file options

;; Map <f5> to quick-revert-buffer so that it works like <f5> in a browser

(global-set-key (kbd "<f5>") 'revert-buffer-quick)

;; Disable emacs version control
(setq vc-handled-backends nil)

;; Make emacs always attempt to display the calendar in a buffer that's at the
;; bottom of the frame and which spans the entire frame
(add-to-list 'display-buffer-alist
             '("\\*Calendar\\*"
               (display-buffer-at-bottom display-buffer-pop-up-window)))


;; Initialize the package manager
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnu-tls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

;; Disable show-paren-mode in org-mode buffers, because org-mode buffers will 
;; often use > or < as less than or greater than, but show-paren treats them as
;; parentheses, and then show spurious errors when they don't match

(setq show-paren-predicate '(and (not (derived-mode . special-mode))
                                 (not (derived-mode . org-mode))))



;; Packages

(use-package request
  :ensure t)

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

(use-package sly
  :ensure t
  :custom-face
  (sly-mrepl-output-face ((t (:foreground "SeaGreen")))))

(use-package cider
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'cider-mode))

(use-package clj-refactor
  :ensure t
  :after (cider)
  :hook (clojure-mode . (lambda ()
                          (clj-refactor-mode 1)
                          (cljr-add-keybindings-with-prefix "C-c C-m"))))

(use-package company
  :ensure t
  :hook ((cider-mode . company-mode)
         (emacs-lisp-mode . company-mode)
         (sly-mode . company-mode)))

(use-package org
  :ensure t
  :bind (:map org-mode-map
         ("C-c ," . org-insert-structure-template)
         ("<insert>" . org-insert-structure-template)
         ("C-c C-'" . org-edit-special)
         ("C-c C-k" . nil))
        (:map org-src-mode-map
         ("C-c C-'" . org-edit-src-exit))
  :config
  (setq org-startup-truncated nil)
  (setq org-startup-indented nil)
  (setq org-startup-folded nil)
  (setq org-startup-export-with-toc nil)
  (setq org-adapt-indentation nil)
  (setq org-yank-folded-subtrees nil)
  (setq org-export-with-toc nil)
  (setq org-image-actual-width '(512))
  (setq org-blank-before-new-entry '((heading . auto) (plain-list-item . nil)))
  (setq org-fontify-todo-headline t)
  (setq org-fontify-done-headline t)
  (setq-default org-clock-mode-line-total 'current)
  :hook (org-mode . (lambda ()
                      (electric-indent-mode -1)
                      (make-local-variable 'search-invisible)
                      (setq search-invisible nil))))

(use-package paredit
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode))

(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :config
  (add-hook 'js-mode-hook #'lsp))

(use-package csv-mode
  :ensure t)

(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook 'lsp-deferred))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package elpher
  :ensure t)

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t)
  (set-face-attribute 'org-todo nil :box 'unspecified)
  (set-face-attribute 'org-done nil :box 'unspecified)
  (set-face-attribute 'org-headline-done nil :foreground "#d4d4d4" :strike-through t)
  (set-face-attribute 'org-headline-todo nil :foreground "#d4d4d4")
  (set-face-attribute 'org-document-title nil :height 'unspecified)
  (set-face-attribute 'org-level-1 nil :height 'unspecified))

(server-start)
