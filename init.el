;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; CONFIG - REGION
;; BEGIN
;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)
;; disable the annoying bell ring
(setq ring-bell-function 'ignore)
(setq font-lock-maximum-decoration 2)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode -1)
(global-hl-line-mode +1)
(setq size-indication-mode t)
(line-number-mode t)
(column-number-mode t)
(setq inhibit-startup-screen t)
;; replace yes/no questions with y/n
(fset 'yes-or-no-p 'y-or-n-p)
(fringe-mode '(1 . 1))
;; delete the previous selection when overrides it with a new insertion.
(delete-selection-mode)
;; the blinking cursor is pretty annoying, so disable it.
(blink-cursor-mode -1)
 ;; make sure that UTF-8 is used everywhere.
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)
(setq-default indent-tabs-mode nil)
;; disable auto save and backups
(setq auto-save-default nil
      auto-save-list-file-prefix nil
      make-backup-files nil)
(setq show-paren-delay 0)
(show-paren-mode t)

(defvar avax-temporal-directory (concat user-emacs-directory "tmp/"))
(unless (file-exists-p avax-temporal-directory)
  (make-directory avax-temporal-directory))
;; END
;; CONFIG REGION


;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package-20171226.1104/")
  (require 'use-package))

;; PACKAGE - REGION
;; BEGIN

(use-package saveplace
  :ensure t
  :config
  (progn
    (setq save-place-file (concat avax-temporal-directory "saveplace.el"))
    (setq-default save-place t)))

(use-package async
  :defer t
  :ensure t
  :config
  (setq async-bytecomp-package-mode t))


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
  :requires counsel-projectile
  :init
  (add-hook 'prog-mode-hook 'projectile-mode)
  :config
  (setq projectile-cache-file (concat avax-temporal-directory "projectile.cache"))
  (setq projectile-known-projects-file (concat avax-temporal-directory "projectile-bookmarks.eld"))
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-mode t)
  (bind-keys*
   ("C-c p p" . counsel-projectile-switch-project))
  :bind (("C-; f" . counsel-projectile-find-file)
         ("C-; ." . projectile-pt)))

(use-package tfs
  :config
  (setq tfs/tf-exe "c:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\common7\\ide\\tf.exe"))

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

(use-package company
  :init
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 1)
  (global-company-mode 1)
  :bind (:map company-active-map
              ("C-s" . company-select-next)
              ("C-r" . company-select-previous)
              ("C-c s" . company-search-candidates)
              ("<tab>" . company-complete-selection)))

(use-package counsel-spotify
  :config
  (setq counsel-spotify-client-id "92d14dda838042468eb437c177b18c2b")
  (setq counsel-spotify-client-secret "6f879ce2ebf44d139c3dbf03fce8bc9d"))

(use-package recentf
  :config
  (setq recentf-max-menu-items 50))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind
  (:map ivy-minibuffer-map
   ("C-o" . hydra-ivy/body))
  :config
  (setq ivy-use-virtual-buffers t)
  ;; swiper regular search
  ;; rest fuzzy match
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t      . ivy--regex-fuzzy)))
  (ivy-mode 1)
  (bind-key "C-c C-r" 'ivy-resume))


(use-package ivy-youtube
  :config
  (setq ivy-youtube-key "AIzaSyBgQ5JaS0wB9FpoNbHwzF9UgpAOWHUxZek"))

(use-package swiper
  :bind
  ("C-s" . swiper))

(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-r" . counsel-recentf)
  ("C-x C-f" . counsel-find-file)
  ("C-c f" . counsel-describe-function)
  ("C-c v" . counsel-describe-variable)
  ("C-c k" . counsel-pt))

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
  ("C-M-o" . hydra-window/body))

(fset 'revertmacro
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([15 15 83 101 108 101 99 116 45 83 113 108 65 122 117 114 101 67 108 117 115 116 101 114 32 19 76 97 115 116 75 110 111 119 110 67 108 117 115 116 101 114 58 return 6 67108896 134217830 134217830 134217847 134217788 5 25 return 83 101 108 101 99 backspace backspace backspace backspace backspace 36 100 98 32 61 32 71 101 116 48 backspace 45 68 97 116 97 98 97 115 101 32 45 76 111 103 105 99 97 108 83 101 101 114 backspace backspace 114 118 101 114 78 97 109 101 32 19 76 111 103 105 99 97 108 83 101 114 118 101 114 14 return 134217830 6 6 67108896 19 68 97 116 97 98 97 115 101 return 134217826 2 134217847 18 45 76 111 103 105 99 97 108 5 25 19 68 97 116 97 98 97 115 101 78 97 109 101 58 return 6 67108896 134217734 134217847 134217788 14 5 32 45 68 97 116 97 98 97 115 101 78 97 109 101 32 25 return 36 115 99 104 101 109 97 61 19 34 34 2 83 99 104 101 backspace backspace backspace backspace backspace delete 83 99 104 101 109 97 return 19 83 99 104 101 109 97 34 58 34 91 backspace return 6 67108896 134217830 134217847 19 36 115 99 104 101 109 97 61 return 25 39 134217826 39 5 return 36 116 97 98 108 101 32 backspace 61 39 39 19 84 97 98 108 101 34 58 34 return 6 67108896 134217734 134217847 19 36 116 97 98 108 101 61 39 return 25 19 73 110 100 101 120 78 97 109 101 34 58 34 return 67108896 134217734 134217847 19 36 116 97 98 108 101 return 5 return 36 105 110 100 101 120 61 39 39 2 25 5] 0 "%d")) arg)))


(use-package monokai-theme
  :config
  (load-theme 'monokai t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("8ed752276957903a270c797c4ab52931199806ccd9f0c3bb77f6f4b9e71b9272" "77c65d672b375c1e07383a9a22c9f9fc1dec34c8774fe8e5b21e76dca06d3b09" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "3c06231f8aa4ad2ebc07d70ade7a1d310cc2adab02251c77a1882787e30f8394" "4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "a25d273ab67ca3d0e8dad603d0a17a8817814a2c1879b0384a14d22d275fb19e" default)))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(org-agenda-files (quote ("e:/org/list.org")))
 '(package-selected-packages
   (quote
    (counsel-spotify ivy-youtube company ivy-hydra which-key use-package smex projectile paredit org-bullets org neotree monokai-theme kanban hydra ggtags counsel bm angular-mode ace-window)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;NOTES
;; DESIRED PACKAGES - QUICK RUN
