;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(menu-bar-mode 0)
(tool-bar-mode 0)
(global-hl-line-mode +1)

(setq-default indent-tabs-mode nil)

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package-20171226.1104/")
  (require 'use-package))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'whitespace-mode))
  (add-hook 'before-save-hook 'whitespace-cleanup)
  :config
  (setq whitespace-line-column 150) ;; limit line length
)

(use-package projectile
  :init
  (add-hook 'prog-mode-hook 'projectile-mode)
  :bind
  ("C-; f" . projectile-find-file)
  ("C-; ." . projectile-pt))

(use-package omnisharp
  :config
  (setq omnisharp-server-executable-path "E:\\omnisharp-win-x64\\OmniSharp.exe")
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

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
  :bind
  ("C-c C-q" . neotree-toggle)
  ("C-c r")
  ("C-q" . neotree-show)
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
  :bind
  ("M-g c" . avy-goto-char)
  ("M-g w" . avy-goto-word-1))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8ed752276957903a270c797c4ab52931199806ccd9f0c3bb77f6f4b9e71b9272" default)))
 '(org-agenda-files (quote ("e:/org/waflandwidb.org" "e:/org/list.org")))
 '(package-selected-packages
   (quote
    (ido-occur projectile-variable pt bm ## ace-window hydra neotree flx-ido ido-completing-read+ paredit smex tabbar use-package omnisharp helm-projectile projectile projectile-codesearch google-maps google-this google-translate multiple-cursors sql-indent yasnippet tfs sr-speedbar spotify monokai-theme helm-youtube helm-xref helm-spotify-plus helm-spotify helm-rtags helm-gtags helm-flyspell helm-flymake helm-flycheck helm-firefox helm-etags-plus helm-cscope helm-codesearch gxref ggtags flymake-cppcheck flycheck-elm f3 etags-table elm-mode dash-functional csharp-mode company ac-etags)))
 '(projectile-mode t nil (projectile))
 '(recentf-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
