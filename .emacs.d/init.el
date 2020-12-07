(require 'package)
(setq package-archives `(("org"   . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(windmove-default-keybindings 'meta)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq-default indent-tabs-mode nil)
(setq lazy-highlight-initial-delay 0)
(setq auto-save-default nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(auto-save-visited-mode 1)
(recentf-mode 1)
(show-paren-mode 1)
(column-number-mode 1)
(electric-pair-mode 1)
(global-display-line-numbers-mode 1)

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'toggle-truncate-lines)

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
(global-set-key (kbd "C-x K") 'kill-current-buffer)
(global-set-key (kbd "C-x R") 'counsel-recentf)

(global-set-key (kbd "<mouse-8>") 'previous-buffer)
(global-set-key (kbd "<mouse-9>") 'next-buffer)

(eval-when-compile
  (require 'use-package))

(use-package browse-url
  :bind ("C-c u" . browse-url))

(use-package org
  :init
  (setq org-directory "~/Org"
        org-default-notes-file (concat org-directory "/misc.org")
        org-agenda-files (list org-directory))

  (require 'org-capture)
  (setq org-capture-templates
        `(("t" "Todo" entry
           (file+headline "" "Tasks")
           "* TODO %?\n")

          ("g" "Game add" table-line
           (file+headline "" "Games")
           "|%?||%^{Status:|Play|Replay|Playing|Finished|Ongoing}||")

          ("m" "Movie add" entry
           (file+headline "" "Movies")
           "* TODO %?"
           :prepend t)))

  :hook (org-mode . org-indent-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)))

(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode))

(use-package lispy
  :hook (emacs-lisp-mode . lispy-mode))

(use-package crux
  :bind ([remap move-beginning-of-line] . crux-move-beginning-of-line))

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
  :bind
  ("C-S-z" . undo-tree-redo)
  ("C-z" . undo-tree-undo)
  :config
  (global-undo-tree-mode 1))

(use-package company
  :init
  (setq company-idle-delay 0
        company-minimum-prefix-length 1)
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ([escape] . company-abort)))

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
  (setq projectile-completion-system 'ivy
        projectile-switch-project-action 'projectile-commander)
  :bind-keymap
  ("C-p" . projectile-command-map)
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-enable-snippet nil
        lsp-keymap-prefix "C-c l")
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-completion-provider :capf))

(use-package lsp-ui
  :after lsp-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)))

(use-package lsp-ivy
  :after lsp-mode
  :bind (:map lsp-command-map
              ("g s" . lsp-ivy-workspace-symbol)))

(use-package typescript-mode
  :hook (typescript-mode . lsp-deferred))

(use-package js
  :hook (js-mode . lsp-deferred)
  :bind ([remap js-find-symbol] . lsp-ui-peek-find-definitions))

(use-package go-mode
  :hook ((go-mode . lsp-deferred)
         (go-mode . (lambda ()
                      (add-hook 'before-save-hook 'lsp-format-buffer t t)
                      (add-hook 'before-save-hook 'lsp-organize-imports t t)))))

(use-package lua-mode
  :hook (lua-mode . lsp-deferred))

(use-package ruby-mode
  :hook (ruby-mode . lsp-deferred))

(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :init
  (setq lsp-java-vmargs
        '("-noverify"
          "-Xmx1G"
          "-XX:+UseG1GC"
          "-XX:+UseStringDeduplication")))

(use-package jq-mode
  :mode "\\.jq$")

(use-package magit
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package git-gutter
  :config
  (global-git-gutter-mode 1))

(use-package treemacs
  :bind ("C-x t t" . treemacs))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package yafolding
  :hook (prog-mode . yafolding-mode)
  :bind (:map yafolding-mode-map
              ("C-M-S-<return>" . yafolding-toggle-all)
              ("C-M-<return>" . yafolding-toggle-element)))

(use-package discover
  :disabled
  :config
  (global-discover-mode 1))

(use-package evil
  :init
  (setq evil-toggle-key "M-z"
        evil-default-state 'emacs)
  :config
  (evil-set-initial-state 'emacs-lisp-mode 'emacs)
  (evil-mode 1))

(use-package evil-magit
  :after (evil magit)
  :init
  (setq evil-magit-state 'motion))

(use-package evil-fringe-mark
  :config
  (global-evil-fringe-mark-mode 1))

(use-package newcomment
  :bind ("C-;" . comment-or-uncomment-region))

(use-package ace-jump-mode
  :bind ("C-c <SPC>" . ace-jump-mode))

(use-package zig-mode
  :requires lsp
  :hook (zig-mode . lsp-deferred)
  :init
  (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "~/Packages/zls/zig-cache/bin/zls")
    :major-modes '(zig-mode)
    :server-id 'zls)))

(use-package yaml-mode
  :hook (yaml-mode . toggle-truncate-lines))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-scroll-output t)
 '(custom-enabled-themes '(material))
 '(custom-safe-themes
   '("edb73be436e0643727f15ebee8ad107e899ea60a3a70020dfa68ae00b0349a87" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "a390bea70629258d80f41a42098bafcc636cd5f29f2449f00a86c1dabf68358d" "9b7f37885eec6ef0441bae9c5ea4a1dd2484eef4342aa3f88d1691722f769fba" "13880fa28757754bc40c85b05689c801ddaa877f2fe65abf1779f37776281ef1" "cba5ebfabc6456e4bbd68e0394d176161e1db063c6ca24c23b9828af0bdd7411" "aa6638f0cd2ba2c68be03220ea73495116dc6f0b625405ede34087c1babb71ae" "347f47b3da854bce47e95497bf2df2e313d1cf934adc88af8393a0e3d1b5133e" "0ff8590332dd254c88bfff22a7fbbdd2cc465b4985bc6959d87da1c9163933f0" "d1af5ef9b24d25f50f00d455bd51c1d586ede1949c5d2863bef763c60ddf703a" "afd761c9b0f52ac19764b99d7a4d871fc329f7392dfc6cd29710e8209c691477" "edf1f9e74600cac84368d8c1ae2158db85217c3a02e3b1470545462a64cea016" "c7eb06356fd16a1f552cfc40d900fe7326ae17ae7578f0ef5ba1edd4fdd09e58" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "5b7c31eb904d50c470ce264318f41b3bbc85545e4359e6b7d48ee88a892b1915" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "d4f8fcc20d4b44bf5796196dbeabec42078c2ddb16dcb6ec145a1c610e0842f3" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "123a8dabd1a0eff6e0c48a03dc6fb2c5e03ebc7062ba531543dfbce587e86f2a" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" default))
 '(package-selected-packages
   '(crux groovy-mode gradle-mode docker-compose-mode lsp-mode lsp-ui json-mode counsel-jq csv-mode sql-indent ace-jump-mode jinja2-mode fish-mode projectile-ripgrep ripgrep nord-theme white-sand-theme colorless-themes lab-themes greymatters-theme autumn-light-theme parchment-theme mood-one-theme evil-fringe-mark evil-magit treemacs-magit treemacs-evil emacs-amazon-libs discover yafolding atom-dark-theme ample-zen-theme ample-theme heaven-and-hell material-theme org diff-hl undo-tree treemacs-projectile lsp-ivy shell-toggle tide typescript-mode jq-mode treemacs lua-mode doom-modeline yaml-mode evil-org evil projectile gruvbox-theme magit lispy company flycheck which-key use-package ivy counsel))
 '(pdf-view-midnight-colors '("#282828" . "#f2e5bc"))
 '(smithy-indent-basic 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
