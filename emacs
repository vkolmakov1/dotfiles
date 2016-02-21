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

;;;; standard-settings

(delete-selection-mode 1)
(menu-bar-mode 0)
(show-paren-mode 1)
(global-subword-mode 1) ; Iterate over camelCasedWords
(electric-pair-mode)

(setq backup-directory-alist `(("." . "~/emacs-backups")))
(toggle-truncate-lines 0)
(setq inhibit-startup-message nil)
(setq inhibit-startup-message t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(diminish 'subword-mode)

;;;; core

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package ivy
  :init (progn
          (ivy-mode)
          (setq ivy-display-style 'fancy)
          (setq ivy-re-builders-alist
                '((t . ivy--regex-fuzzy)))
          (setq ivy-initial-inputs-alist nil)
          )
  :diminish ivy-mode)

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-c g" . counsel-git-grep)
         ("C-h b" . counsel-descbinds)
         ("C-h f" . counsel-describe-function)
         ))

(use-package flx
  :ensure t)

;;;; org-mode
(use-package org
  :mode (("\\.org$" . org-mode))
  :init (global-set-key (kbd "C-c a") 'org-agenda)
  :config (progn
            (setq org-src-fontify-natively t)
            (setq org-agenda-files (list "~/org/work.org"
                                         "~/org/school.org"
                                         "~/org/home.org"))
            ))


;;;; company-c-python

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :init (progn
          (global-company-mode)
          (define-key company-active-map (kbd "C-n") 'company-select-next)
          (define-key company-active-map (kbd "C-p") 'company-select-previous)
          (setq company-minimum-prefix-length 2)))

(use-package company-c-headers
  :ensure t
  :defer t
  :init (add-to-list 'company-backends 'company-c-headers))

(use-package company-jedi
  :ensure t
  :defer t
  :init (add-to-list 'company-backends 'company-jedi))

;;;; general-utils

(use-package dired+
  :ensure t
  :defer t
  :init (diredp-toggle-find-file-reuse-dir 1))

(use-package smooth-scrolling
  :ensure t)

(use-package undo-tree
  :ensure t
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :bind ("M-/" . undo-tree-visualize))

(use-package comment-dwim-2
  :ensure t
  :defer t
  :bind ("M-;" . comment-dwim-2))

(use-package expand-region
  :ensure t
  :defer t
  :bind (("M-=" . er/expand-region)
         ("M--" . er/contract-region)))

;; (use-package multiple-cursors
;;   :ensure t
;;   :defer t
;;   :bind (("C-]" . mc/mark-next-like-this)
;;          ("C-<" . mc/mark-previous-like-this)))
;; Find better binds for it

(use-package ace-jump-mode
  :ensure t
  :defer t
  :bind ("C-c SPC" . ace-jump-char-mode))

(use-package visual-regexp-steroids
  :ensure t
  :defer t
  :bind ("C-c r" . vr/select-query-replace))

(use-package projectile
  :ensure t
  :defer t
  :init (progn
          (projectile-global-mode)
          (setq projectile-completion-system 'ivy)))

(use-package ace-window
  :ensure t
  :defer t
  :bind ("C-x o" . ace-window))

;;; web-stuff

(use-package web-mode
  :ensure t
  :defer t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
          (setq web-mode-enable-auto-pairing t)
          (setq web-mode-enable-current-element-highlight t)
          (setq-default indent-tabs-mode nil)
          (setq web-mode-markup-indent-offset 4)
          (setq web-mode-css-indent-offset 4)
          (setq web-mode-code-indent-offset 4)))

(use-package emmet-mode
  :ensure t
  :defer t
  :init (add-hook 'web-mode-hook 'emmet-mode))

(use-package js2-mode
  :ensure t
  :defer t

  :init (progn
          (add-hook 'js-mode-hook 'js2-mode)
          (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))))

;;;; lisp
(add-hook 'lisp-mode-hook (lambda ()
                            (setq-local show-paren-style 'expression)))
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq-local show-paren-style 'expression)))

(use-package slime
  :ensure t
  :defer t
  :init (progn
          (add-hook 'slime-repl-mode-hook
                    (lambda ()
                      (define-key slime-repl-mode-map (kbd "DEL") nil)))
          (load (expand-file-name "~/quicklisp/slime-helper.el"))
          (slime-setup '(slime-fancy))
          (setq inferior-lisp-program "sbcl")))

;;;; looks
(use-package smart-mode-line
  :ensure t
  :init (progn
          (setq sml/no-confirm-load-theme t)
          (setq sml/theme 'respectful)
          (setq sml/shorten-modes t)
          (setq sml/shorten-directory t)
          (sml/setup)))

(use-package tangotango-theme
  :ensure t
  :init (load-theme 'tangotango t))

(use-package golden-ratio
  :ensure t
  :bind ("M-o" . golden-ratio))
(put 'upcase-region 'disabled nil)
