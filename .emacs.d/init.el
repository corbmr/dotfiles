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
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(windmove-default-keybindings 'meta)
(show-paren-mode 1)

(recentf-mode)

(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq c-basic-offset tab-width)
(setq-default electric-indent-inhibit t)
(setq-default indent-tabs-mode t)
(setq backward-delete-char-untabify-method 'nil)

(setq make-backup-files nil)
(setq auto-save-default nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(auto-save-visited-mode)

(global-set-key [escape] 'keyboard-escape-quit)

(eval-when-compile
  (require 'use-package))

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
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (global-company-mode))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package undo-tree
  :demand
  :bind (("C-z" . 'undo-tree-undo)
		 ("C-S-z" . 'undo-tree-redo))
  :config
  (global-undo-tree-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :config
  (push 'company-lsp company-backend))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (darkokai)))
 '(custom-safe-themes
   (quote
	("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "37ba833442e0c5155a46df21446cadbe623440ccb6bbd61382eb869a2b9e9bf9" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" "5f1bd7f67dc1598977e69c6a0aed3c926f49581fdf395a6246f9bc1df86cb030" "41c8c11f649ba2832347fe16fe85cf66dafe5213ff4d659182e25378f9cfc183" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" default)))
 '(ensime-sem-high-faces
   (quote
	((var :foreground "#9876aa" :underline
		  (:style wave :color "yellow"))
	 (val :foreground "#9876aa")
	 (varField :slant italic)
	 (valField :foreground "#9876aa" :slant italic)
	 (functionCall :foreground "#a9b7c6")
	 (implicitConversion :underline
						 (:color "#808080"))
	 (implicitParams :underline
					 (:color "#808080"))
	 (operator :foreground "#cc7832")
	 (param :foreground "#a9b7c6")
	 (class :foreground "#4e807d")
	 (trait :foreground "#4e807d" :slant italic)
	 (object :foreground "#6897bb" :slant italic)
	 (package :foreground "#cc7832")
	 (deprecated :strike-through "#a9b7c6"))))
 '(fci-rule-color "#14151E")
 '(hl-todo-keyword-faces
   (quote
	(("TODO" . "#dc752f")
	 ("NEXT" . "#dc752f")
	 ("THEM" . "#2d9574")
	 ("PROG" . "#4f97d7")
	 ("OKAY" . "#4f97d7")
	 ("DONT" . "#f2241f")
	 ("FAIL" . "#f2241f")
	 ("DONE" . "#86dc2f")
	 ("NOTE" . "#b1951d")
	 ("KLUDGE" . "#b1951d")
	 ("HACK" . "#b1951d")
	 ("TEMP" . "#b1951d")
	 ("FIXME" . "#dc752f")
	 ("XXX+" . "#dc752f")
	 ("\\?\\?\\?+" . "#dc752f"))))
 '(package-selected-packages
   (quote
	(lsp-mode company-go go-mode company-lsp company-ghc flycheck-haskell lispy counsel haskell-mode use-package ini-mode company-racer cargo magit projectile rust-mode neotree toml-mode evil-smartparens smartparens rainbow-delimiters spaceline monokai-theme darkokai-theme yaml-mode dracula-theme mood-line doom-modeline spacemacs-theme powerline powerline-evil afternoon-theme company evil which-key helm)))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e")))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
	((20 . "#d54e53")
	 (40 . "goldenrod")
	 (60 . "#e7c547")
	 (80 . "DarkOliveGreen3")
	 (100 . "#70c0b1")
	 (120 . "DeepSkyBlue1")
	 (140 . "#c397d8")
	 (160 . "#d54e53")
	 (180 . "goldenrod")
	 (200 . "#e7c547")
	 (220 . "DarkOliveGreen3")
	 (240 . "#70c0b1")
	 (260 . "DeepSkyBlue1")
	 (280 . "#c397d8")
	 (300 . "#d54e53")
	 (320 . "goldenrod")
	 (340 . "#e7c547")
	 (360 . "DarkOliveGreen3"))))
 '(vc-annotate-very-old-color nil)
 '(which-key-show-early-on-C-h t))
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

