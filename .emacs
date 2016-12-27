(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(defun my/add-to-hooks (func hooks)
  (mapc (lambda (hook)
          (add-hook hook func))
        hooks))

;;;; standard-settings
(delete-selection-mode 1)
(menu-bar-mode 0)
(show-paren-mode 1)
(tool-bar-mode 0)
(global-subword-mode 1) ; Iterate over camelCasedWords
(electric-pair-mode)
(setq vc-follow-symlinks t) ; follow symlinks without asking

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(setq backup-dir "~/emacs-backups")
(setq backup-directory-alist `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,backup-dir t)))

(setq inhibit-startup-message nil)
(setq inhibit-startup-message t)
;; fix scrolling
(setq-default redisplay-dont-pause t
              scroll-margin 1
              scroll-step 1
              scroll-conservatively 10000
              scroll-preserve-screen-position 1
              scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;; Use spaces by default
(setq-default indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; better keybinds for pane splits
(bind-keys*
 ("C-x -" . split-window-below)
 ("C-x |" . split-window-right))

;; save buffer on biffer switch
;; http://stackoverflow.com/questions/1413837/emacs-auto-save-on-switch-buffer
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-frame (before other-frame-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice ace-window (before other-frame-now activate)
  (when buffer-file-name (save-buffer)))

(diminish 'subword-mode)
(diminish 'auto-revert-mode)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;; core
(use-package swiper
  :ensure t
  :bind* ("C-s" . swiper))

(use-package flx
  :ensure t)

(use-package smex
  :ensure t)

(use-package ivy
  :init
  (ivy-mode)
  (setq ivy-display-style 'fancy)
  (setq ivy-initial-inputs-alist nil)
  :bind (:map ivy-minibuffer-map
              ("C-j" . ivy-immediate-done))
  :diminish ivy-mode)

(use-package counsel
  :ensure t
  :bind*
  (("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   ("C-c g" . counsel-ag)
   ("C-h b" . counsel-descbinds)
   ("C-h f" . counsel-describe-function)))

(use-package ibuffer
  :ensure t
  :bind* ("C-x C-b" . ibuffer))

(use-package dired+
  :ensure t
  :defer t
  :init
  (if (executable-find "gls")
      (progn
        (setq insert-directory-program "gls")
        (setq dired-listing-switches "-lFaGh1v --group-directories-first")))
  (diredp-toggle-find-file-reuse-dir 1)
  (add-to-list 'dired-omit-extensions ".DS_Store")
  (customize-set-variable 'diredp-hide-details-initially-flag nil))

(use-package undo-tree
  :ensure t
  :defer t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind* ("M-/" . undo-tree-visualize))

(use-package expand-region
  :ensure t
  :defer t
  :bind*
  (("M-=" . er/expand-region)
   ("M--" . er/contract-region)))

(use-package avy
  :ensure t
  :bind* ("M-l" . avy-goto-word-1))

(use-package visual-regexp-steroids
  :ensure t
  :defer t) ;; TODO: add better kbd

(use-package projectile
  :ensure t
  :defer t
  :init
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

(use-package simpleclip
  :ensure t
  :bind*
  (("C-x C-w" . simpleclip-copy)
   ("C-x C-y" . simpleclip-paste)))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind) ;; TODO: add better kbds

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :config (global-company-mode)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)))

(use-package magit
  :ensure t
  :defer t
  :bind*
  (("<f11>" . magit-log-all)
   ("<f12>" . magit-status)))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init (global-git-gutter-mode t))

(use-package ws-butler
  :ensure t
  :diminish (ws-butler-mode)
  :init
  (ws-butler-global-mode))

;; MODES

;; web
(use-package web-mode
  :ensure t
  :config
  (setq-default indent-tabs-mode nil)
  (let ((default-offset 2))
    (setq web-mode-markup-indent-offset default-offset)
    (setq web-mode-css-indent-offset default-offset)
    (setq web-mode-code-indent-offset default-offset)
    (setq web-mode-indent-style default-offset)
    (setq web-mode-script-padding default-offset)
    (setq web-mode-style-padding default-offset)
    (setq web-mode-block-padding default-offset)
    )
  :mode ("\\.html?\\'"
         "\\.vue?\\'"))

(use-package emmet-mode
  :ensure t
  :defer t
  :init (my/add-to-hooks 'emmet-mode '(web-mode-hook
                                       html-mode-hook
                                       vue-mode-hook
                                       js-mode-hook)))

;; js
(use-package js
  :config
  (setq js-indent-level 2))


;; elisp
(use-package paredit
  :ensure t
  :config (add-hook 'paredit-mode 'emacs-lisp-mode))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'rainbow-delimiters-mode 'emacs-lisp-mode))

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :config (add-hook 'highlight-parentheses-mode 'emacs-lisp-mode))

;; LOOKS
;; theme
(use-package arjen-grey-theme
  :ensure t
  :config
  (load-theme 'arjen-grey t))

(defun my/set-font-height (height)
  (set-face-attribute 'default nil :font "Source Code Pro" :height height))

(setq my/default-font-height 145)
(setq my/live-coding-font-height 210)

(my/set-font-height my/default-font-height)
(defun my/toggle-live-coding ()
  (interactive)
  (let ((default-height my/default-font-height)
        (live-coding-height my/live-coding-font-height)
        (current-height (face-attribute 'default :height)))
    (if (>= current-height live-coding-height)
        (my/set-font-height default-height)
      (my/set-font-height live-coding-height))))

;; modeline
(defun my/shorten-dir (dir-str)
  "Given a directory keep the only the last two items"
  (let ((dirs (reverse (split-string dir-str "/"))))
    (cond ((and (equal (car dirs) "")
                (equal (cadr dirs) ""))
           "/")
          ((and (equal (car dirs) "")
                (or (equal (cadr dirs) user-login-name)
                    (equal (cadr dirs) "~")))
           "~")
          (t (cadr dirs)))))


(setq mode-line-format
      (setq default-mode-line-format
            (list '("-"
                    (:eval (if (buffer-modified-p) "M" "-"))
                    (:eval (if buffer-read-only    "R" "-"))
                    " "
                    mode-line-buffer-identification
                    " "
                    mode-line-position
                    "\tat:"
                    (:eval (my/shorten-dir default-directory))
                    " "
                    vc-mode
                    " "
                    mode-line-modes))))
