(require 'package)
(setq package-archives `(("org"   . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(windmove-default-keybindings 'meta)

(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq-default indent-tabs-mode nil)

(setq auto-save-default nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(auto-save-visited-mode 1)
(recentf-mode 1)
(show-paren-mode 1)
(column-number-mode 1)
(electric-pair-mode 1)

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'toggle-truncate-lines)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defvar --backup-directory (concat user-emacs-directory "backups"))
(unless (file-exists-p --backup-directory)
  (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory))
      make-backup-files t ; backup of a file the first time it is saved.
      backup-by-copying t ; don't clobber symlinks
      version-control t   ; version numbers for backup files
      delete-old-versions t)

;;; esc always quits
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'keyboard-escape-quit)

(eval-when-compile
  (require 'use-package))

(use-package org
  :init
  (setq org-directory "~/Org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files (list org-directory))

  (setq org-capture-templates
        `(("t" "Todo" entry
           (file+headline "" "Tasks")
           "* TODO %?\n %i")

          ("g" "Game add" table-line
           (file "games.org")
           "|%?||%^{Status:|Play|Replay|Playing|Finished|Ongoing}||")))

  :hook (org-mode . org-indent-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)))

(use-package lispy
  :hook (emacs-lisp-mode . lispy-mode))

(use-package ivy
  :ensure t
  :demand
  :init
  (setq ivy-use-virtual-buffers t
		ivy-count-format "%d/%d ")
  :config
  (ivy-mode t))

(use-package which-key
  :ensure t
  :demand
  :init
  (setq which-key-show-early-on-C-h t)
  :config
  (which-key-mode t))

(use-package undo-tree
  :ensure t
  :demand
  :bind (("C-z" . undo-tree-undo)
		 ("C-S-z" . undo-tree-redo))
  :config
  (global-undo-tree-mode t))

(use-package company
  :init
  (setq company-idle-delay 0
        company-minimum-prefix-length 1)
  :hook (prog-mode . company-mode))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package heaven-and-hell
  :init
  (setq heaven-and-hell-themes '((light . gruvbox-light-soft)
                                 (dark . material))
        heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind ("<f6>" . heaven-and-hell-toggle-theme))

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode t)
  :hook (projectile-after-switch-project . treemacs))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-enable-snippet nil
        lsp-keymap-prefix "C-c l")

  :hook (lsp-mode . lsp-enable-which-key-integration))

(use-package lsp-ui
  :after lsp-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)))

(use-package typescript-mode
  :hook (typescript-mode . lsp))

(use-package js
  :hook (js-mode . lsp))

(use-package go-mode
  :hook ((go-mode . (lambda ()
                      (add-hook 'before-save-hook 'lsp-format-buffer t t)
                      (add-hook 'before-save-hook 'lsp-organize-imports t t)))
         (go-mode . lsp)))

(use-package lua-mode
  :hook (lua-mode . lsp))

(use-package ruby-mode
  :hook (ruby-mode . lsp))

(use-package company-lsp
  :after (company lsp-mode)
  :config
  (add-to-list 'company-lsp-filter-candidates '(lsp-emmy-lua . t))
  (add-to-list 'company-backends 'company-lsp))

(use-package jq-mode
  :mode "\\.jq$")

(use-package magit
  :bind (:map magit-file-mode-map
              ("C-c g" . magit-file-dispatch)))

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package treemacs
  :bind (("C-x t t" . treemacs)))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package lsp-java
  :hook (java-mode . lsp))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#ebdbb2" "#9d0006" "#79740e" "#b57614" "#076678" "#8f3f71" "#427b58" "#3c3836"])
 '(custom-enabled-themes (quote (gruvbox-light-soft)))
 '(custom-safe-themes
   (quote
    ("d4f8fcc20d4b44bf5796196dbeabec42078c2ddb16dcb6ec145a1c610e0842f3" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "123a8dabd1a0eff6e0c48a03dc6fb2c5e03ebc7062ba531543dfbce587e86f2a" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" default)))
 '(org-babel-load-languages (quote ((js . t))))
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
    (heaven-and-hell material-theme org diff-hl undo-tree treemacs-projectile lsp-ivy lsp-java shell-toggle tide typescript-mode jq-mode lsp-treemacs treemacs lua-mode company-lsp lsp-mode lsp-ui doom-modeline yaml-mode evil-org evil projectile gruvbox-theme magit lispy company flycheck which-key use-package ivy counsel)))
 '(pdf-view-midnight-colors (quote ("#282828" . "#f2e5bc")))
 '(safe-local-variable-values (quote ((read-only-mode . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(if (daemonp)
    (add-hook 'after-make-frame-functions
			  (lambda (frame)
				(select-frame frame)
				(load-theme (car custom-enabled-themes) t))))
