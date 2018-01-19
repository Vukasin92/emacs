;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(menu-bar-mode 0)
(tool-bar-mode 0)
(global-linum-mode 1)
(global-hl-line-mode +1)
(line-number-mode t)
(column-number-mode t)

(setq-default indent-tabs-mode nil)

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package-20171226.1104/")
  (require 'use-package))

(use-package whitespace
  :init
  (dolist
      (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'whitespace-mode)
    (add-hook 'before-save-hook 'whitespace-cleanup)
  )
  :config
  (setq whitespace-line-column 150)
  (setq whitespace-style '(whitespace face tabs empty trailing)))
;; limit line length

  (use-package angular-mode
    :config
    (setq auto-mode-alist (append '(("\\.ts$" . angular-mode))
                                  auto-mode-alist)))

(use-package projectile
  :init
  (add-hook 'prog-mode-hook 'projectile-mode)
  :bind
  ("C-; f" . projectile-find-file)
  ("C-; ." . projectile-pt))

;; Omnisharp is slowing me down - stop it
;;(use-package omnisharp
  ;;  :config
  ;;  (setq omnisharp-server-executable-path "E:\\omnisharp-win-x64\\OmniSharp.exe")
  ;;  (add-hook 'csharp-mode-hook 'omnisharp-mode)
;;  )

(use-package paredit
  :diminish paredit-mode
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'json-mode-hook 'enable-paredit-mode))

(use-package neotree
  :init
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-autorefresh t)
  :config
  (defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
  :bind
  ("C-c C-q" . neotree-toggle)
  ("C-c r")
  ("C-q" . neotree-show)
  ("C-c C-w" . copy-file-name-to-clipboard)
  (:map
   neotree-mode-map
   ("C-c C-w" . neotree-copy-filepath-to-yank-ring)))

(use-package recentf
  :config
  (setq recentf-max-menu-items 50))

(use-package ido
  :init (ido-mode 1)
  :requires recentf
  :config
  (defun ido-recentf-open ()
    "Use `ido-completing-read' to find a recent file."
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting")))
  (global-set-key (kbd "C-x C-r") 'ido-recentf-open)
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-use-virtual-buffers t)
  :bind
  ("C-x C-r" . ido-recentf-open))
(use-package ido-completing-read+
  :requires ido)
(use-package ido-occur
  :requires ido
  :bind
  ("C-s" . ido-occur))
(use-package flx-ido
  :init (flx-ido-mode 1)
  :requires ido)
(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex)
  :requires ido)

(use-package avy
  :config
  (bind-keys*
   ("C-c C-c" . avy-goto-char)
   ("C-c C-s" . avy-goto-word-1)))
(use-package sql)

(use-package bm
  :bind
  ("C-; g" . bm-toggle-cycle-all-buffers)
  ("C-; t" . bm-toggle)
  ("C-; s" . bm-next)
  ("C-; r" . bm-previous))

(use-package ace-window
  :requires hydra
  :init
  (defhydra hydra-window ()
    "Window management"
    ("v" split-window-vertically "Split vertical")
    ("h" split-window-horizontally "Split horizontal")
    ("b" windmove-left "Move to buffer on the left")
    ("f" windmove-right "Move to buffer on the right")
    ("p" windmove-down "Move to buffer on the bottom")
    ("n" windmove-up "Move to buffer on the top")
    ("F" ace-delete-other-windows "Move window")
    ("s" ace-swap-window "Swap window")
    ("d" ace-delete-window "Delete window"))
  :bind
  ("C-x s" . ace-select-window)
  ("C-<tab>" . other-window)
  ("C-M-o" . hydra-window/body)
  )
(use-package monokai-theme
  :config
  (load-theme 'monokai t))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)
;; disable the annoying bell ring
(setq ring-bell-function 'ignore)
(setq font-lock-maximum-decoration 2)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("e:/org/list.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
