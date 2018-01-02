;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq-default indent-tabs-mode nil)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package-20171226.1104/")
  (require 'use-package))

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

(use-package tabbar
  :init (tabbar-mode 1))
(use-package ido
  :init (ido-mode 1)
  :config
  (setq ido-enable-flex-matching t
        ido-everywhere t))
(use-package ido-completing-read+
  :requires ido)
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
(use-package doremi)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (doremi ace-window neotree sr-speedbar tabbar yasnippet which-key use-package tao-theme smex rainbow-delimiters paredit paradox ido-vertical-mode ido-ubiquitous helm-gtags helm-ag google-this ggtags flycheck flx-ido cuda-mode auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

