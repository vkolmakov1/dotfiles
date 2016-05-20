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
(tool-bar-mode 0)
(global-subword-mode 1) ; Iterate over camelCasedWords
(electric-pair-mode)
(setq vc-follow-symlinks t) ; follow symlinks without asking

(setq backup-dir "~/emacs-backups")
(setq backup-directory-alist `(("." . ,backup-dir)))

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

;; don't fold lines, and use spaces by default
(setq-default truncate-lines t
              indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'focus-out-hook 'garbage-collect)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; switch between panes
(setq windmove-wrap-around t) ; wrap around on pane-switching
(bind-keys*
 ("C-x <right>" . windmove-right)
 ("C-x <left>" . windmove-left)
 ("C-x <up>" . windmove-up)
 ("C-x <down>" . windmove-down))

;; better keybinds for pane splits
(bind-keys*
 ("C-x -" . split-window-below)
 ("C-x |" . split-window-right))

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

;; ask before closing emacs
(global-set-key (kbd "C-x C-c")
                (lambda ()
                  (interactive)
                  (cond ((y-or-n-p "Close this session?")
                         (save-buffers-kill-terminal)))))

;; create a command to clear kill-ring
(defun my/clear-kill-ring ()
  "Clears out kill-ring contents"
  (interactive)
  (setq kill-ring nil))

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
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  :bind (:map ivy-minibuffer-map
              ("C-j" . ivy-immediate-done))
  :diminish ivy-mode)

(use-package counsel
  :ensure t
  :bind* (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-c g" . counsel-git-grep)
         ("C-h b" . counsel-descbinds)
         ("C-h f" . counsel-describe-function)
         ("C-c k" . counsel-ag)))

;;;; org-mode
(use-package org
  :mode (("\\.org$" . org-mode))
  :init (global-set-key (kbd "C-c a") 'org-agenda)
  :config
  (org-indent-mode 1)
  (setq org-src-fontify-natively t)
  (setq org-agenda-files (list "~/org/agenda/work.org"
                               "~/org/agenda/school.org"
                               "~/org/agenda/personal.org")))

;;;; general-utils
(use-package auto-complete
  :ensure t
  :init
  (ac-config-default)
  (setq ac-auto-start 2)
  :bind
  (:map ac-complete-mode-map
        ("TAB" . nil)
        ("C-n" . ac-next)
        ("C-p" . ac-previous)))

(use-package magit
  :ensure t
  :defer t
  :bind* (("<f11>" . magit-log-all)
         ("<f12>" . magit-status)))

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
  (customize-set-variable 'diredp-hide-details-initially-flag nil)
  (add-hook 'dired-mode-hook 'auto-revert-mode))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :mode ("\\.yasnippet\\'" . snippet-mode)
  :init
  (setq yas-snippet-dirs
        '("~/dotfiles/snippets"))
  (yas-global-mode)
  :config (yas-reload-all))

(use-package undo-tree
  :ensure t
  :defer t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind* ("M-/" . undo-tree-visualize))

(use-package comment-dwim-2
  :ensure t
  :defer t
  :bind* ("M-;" . comment-dwim-2))

(use-package region-bindings-mode
  :ensure t
  :init
  (region-bindings-mode)
  (region-bindings-mode-enable)
  :bind
  (:map region-bindings-mode-map
        ("u" . upcase-region)
        ("l" . downcase-region)))

(use-package expand-region
  :ensure t
  :defer t
  :bind* (("M-=" . er/expand-region)
         ("M--" . er/contract-region)))

(use-package avy
  :ensure t
  :bind* ("M-a" . avy-goto-word-1))

(use-package visual-regexp-steroids
  :ensure t
  :defer t
  :bind
  (:map region-bindings-mode-map
        ("r" . vr/select-query-replace)))

(use-package projectile
  :ensure t
  :defer t
  :init
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

(use-package ace-window
  :ensure t
  :defer t
  :bind* ("C-x o" . ace-window))

(use-package simpleclip
  :ensure t
  :bind* (("C-x C-w" . simpleclip-copy)
         ("C-x C-y" . simpleclip-paste)))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind
  (:map region-bindings-mode-map
        ("a" . mc/mark-all-like-this)
        ("n" . mc/mark-next-like-this)
        ("p" . mc/mark-previous-like-this)
        ("P" . mc/unmark-previous-like-this)
        ("N" . mc/unmark-next-like-this)))

(use-package drag-stuff
  :ensure t
  :init (drag-stuff-global-mode 1)
  :bind
  (:map region-bindings-mode-map
        ("<up>" . drag-stuff-up)
        ("<down>" . drag-stuff-down)
        ("<left>" . drag-stuff-left)
        ("<right>" . drag-stuff-right)))
;; mostly for javascript
;;;; borrowed from http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (file-executable-p eslint)
      (setq-local flycheck-javascript-eslint-executable eslint))))

;; first, do $ npm install -g eslint babel-eslint
;; then configure ~/.eslintrc
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (add-hook 'js-mode-hook #'my/use-eslint-from-node-modules)
  (add-hook 'js-mode-hook 'flycheck-mode)
  :config
  (setq-default flycheck-temp-prefix ".")
  (setq-default flycheck-disabled-checkers
                             (append flycheck-disabled-checkers
                                     '(javascript-jshint))))

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize)))

;;;; python
;; sudo pip install elpy rope jedi
(use-package elpy
  :ensure t
  :init (add-hook 'python-mode-hook 'elpy-mode))

;;; web-stuff
(use-package web-mode
  :ensure t
  :defer t
  :mode "\\.html?\\'"
  :config
  (setq web-mode-enable-current-element-highlight t)
  (setq-default indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))

(use-package emmet-mode
  :ensure t
  :defer t
  :init (add-hook 'web-mode-hook 'emmet-mode))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

;;;; javascript
(use-package js2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js-mode-hook 'js2-minor-mode))

(use-package nodejs-repl
  :ensure t)

(use-package restclient
  :ensure t
  :config (add-hook 'restclient-mode-hook '(lambda () (local-set-key (kbd "C-c C-c") 'restclient-http-send-current-stay-in-window)))
  :bind
  (:map restclient-mode-map
        ("C-c C-c" . restclient-http-send-current-stay-in-window) ; this doesn't work for some reason :(
        ("M-p" . restclient-jump-prev)
        ("M-n" . restclient-jump-next)))

(use-package css-mode
  :ensure t
  :mode "\\.css\\'")

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'")

;;;; lisp
(use-package paredit
  :ensure t
  :bind
  (:map paredit-mode-map
        ("M-{" . paredit-forward-barf-sexp)
        ("M-}" . paredit-forward-slurp-sexp)))

(use-package rainbow-delimiters
  :ensure t)

(use-package slime
  :ensure t
  :defer t
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl")
  (setq slime-protocol-version 'ignore)
  (define-key slime-repl-mode-map (kbd "DEL") nil)
  (define-key slime-repl-mode-map (kbd "M-RET") 'slime-repl-newline-and-indent)

  (defun my/lisp-hook ()
    (rainbow-delimiters-mode 1)
    (paredit-mode 1)
    (eldoc-mode 1)
    (setq eldoc-idle-delay 0.3)
    (diminish 'eldoc-mode))

  (add-hook 'slime-repl-mode-hook 'my/lisp-hook)
  (add-hook 'lisp-mode-hook 'my/lisp-hook))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (diminish 'eldoc-mode)
                                  (paredit-mode 1)
                                  (setq eldoc-idle-delay 0.3)
                                  (eldoc-mode 1)))

;;;; looks
(defmacro my/set-theme (tname)
  "Install and set theme using use-package"
  (let ((theme-name tname)
        (package-name (funcall (lambda ()
                                 (intern (concat
                                          (symbol-name tname)
                                          "-"
                                          (symbol-name 'theme)))))))
    `(use-package ,package-name
       :ensure t
       :config (load-theme ',theme-name t))))

(my/set-theme ample)

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

(use-package golden-ratio
  :ensure t
  :bind ("M-o" . golden-ratio))

(custom-set-variables
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-errors nil)
 '(js2-mode-show-warnings nil)
 '(js2-mode-show-strict-warnings nil)
 '(js2-mode-show-warn-or-err nil)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults))))
(put 'narrow-to-region 'disabled nil)
