(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

(windmove-default-keybindings 'meta)

(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq c-basic-offset tab-width)
(setq-default electric-indent-inhibit t)
(setq-default indent-tabs-mode t)
(setq backward-delete-char-untabify-method 'nil)

(setq auto-save-default nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(auto-save-visited-mode 1)
(recentf-mode 1)
(show-paren-mode 1)

(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(eval-when-compile
  (require 'use-package))

(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files   t ; backup of a file the first time it is saved.
      backup-by-copying   t ; don't clobber symlinks
      version-control     t ; version numbers for backup files
      delete-old-versions t)

;;; esc always quits
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'keyboard-escape-quit)

(use-package lispy
  :hook (emacs-lisp-mode . lispy-mode))

(use-package which-key
  :ensure t
  :init
  (setq which-key-show-early-on-C-h t)
  :config
  (which-key-mode))

(use-package ivy
  :ensure t
  :demand
  :init
  (setq ivy-use-virtual-buffers t
		ivy-count-format "%d/%d ")
  :config
  (ivy-mode))

(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  :config
  (global-company-mode))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package company
  :hook (prog-mode . company-mode))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode))

(use-package undo-tree
  :demand
  :bind (("C-z" . 'undo-tree-undo)
		 ("C-S-z" . 'undo-tree-redo))
  :config
  (global-undo-tree-mode))

(use-package org
  :hook ((org-mode . flyspell-mode)
		 (org-mode . org-indent-mode)))
  
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((go-mode lua-mode) . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(add-hook 'go-mode-hook
		  (lambda ()
			(add-hook 'before-save-hook #'lsp-format-buffer t t)
			(add-hook 'before-save-hook #'lsp-organize-imports t t)))

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :config
  (push 'company-lsp company-backend)
  (push '(lsp-emmy-lua . t) company-lsp-filter-candidates))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(counsel-mode nil)
 '(custom-enabled-themes (quote (gruvbox-light-soft)))
 '(custom-safe-themes
   (quote
	("4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "123a8dabd1a0eff6e0c48a03dc6fb2c5e03ebc7062ba531543dfbce587e86f2a" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" default)))
 '(org-babel-load-languages (quote ((js . t))))
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
	(treemacs lua-mode company-lsp lsp-mode lsp-ui doom-modeline yaml-mode evil-org evil projectile gruvbox-theme org magit lispy company flycheck which-key use-package ivy counsel)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#282828")))
 '(show-paren-mode t)
 '(which-key-mode t))
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

