;; -*- mode: emacs-lisp -*-

(defconst my/default-font-height 120)
(defconst my/live-coding-font-height 200)
(defconst my/font-name "Fira Code")

(defun my/switch-to-tabs ()
  (interactive)
  (progn
    (kill-local-variable 'indent-tabs-mode)
    (setq indent-tabs-mode t)
    (setq-default indent-tabs-mode t)))

(defun my/switch-to-spaces ()
  (interactive)
  (progn
    (kill-local-variable 'indent-tabs-mode)
    (setq indent-tabs-mode nil)
    (setq-default indent-tabs-mode nil)))

(defun my/set-font-height (height)
  (set-face-attribute 'default nil :font my/font-name :height height))

(defun my/toggle-live-coding ()
  (interactive)
  (let ((default-height my/default-font-height)
        (live-coding-height my/live-coding-font-height)
        (current-height (face-attribute 'default :height)))
    (if (>= current-height live-coding-height)
        (my/set-font-height default-height)
      (my/set-font-height live-coding-height))))

;; Setup
(defun my/setup-use-package ()
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-when-compile
    (require 'use-package)))

(defun my/set-general-settings ()
  (delete-selection-mode 1)
  (menu-bar-mode 0)
  (show-paren-mode 1)
  (tool-bar-mode 0)
  ;; Iterate over camelCasedWords
  (global-subword-mode 1) 
  (electric-pair-mode)
  ;; follow symlinks without asking
  (setq vc-follow-symlinks t)
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore)
  ;; set settings for backup files
  (setq backup-dir "~/emacs-backups")
  (setq backup-directory-alist `((".*" . ,backup-dir)))
  (setq auto-save-file-name-transforms `((".*" ,backup-dir t)))
  ;; indentation
  (setq-default default-tab-width 4)
  (my/switch-to-spaces)
  (fset 'yes-or-no-p 'y-or-n-p)

  (setq tramp-default-method "ssh")
  (setq tramp-copy-size-limit nil))

(defun my/set-default-keybindings ()
  (bind-keys*
   ("C-x -" . split-window-below)
   ("C-x |" . split-window-right)
   ("C-S-n" . (lambda ()
                (interactive)
                (ignore-errors (next-line 5))))
   ("C-S-p" . (lambda ()
                (interactive)
                (ignore-errors (previous-line 5))))))


(defun my/add-core-packages ()
  (use-package swiper
    :ensure t
    :bind*
    ("C-s" . swiper))

  (use-package smex
    :ensure t)

  (use-package diminish
    :ensure t)

  (use-package ivy
    :init (ivy-mode)
    :config
    (setq ivy-display-style 'fancy)
    (setq ivy-initial-inputs-alist nil)
    :bind
    (:map ivy-minibuffer-map
          ;; to be able to select options with partially overlapping names
          ("C-j" . ivy-immediate-done))
    :diminish ivy-mode)

  (use-package company
    :ensure t
    :diminish company-mode
    :init (global-company-mode)
    :bind
    (:map company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))

  (use-package counsel
    :ensure t
    :bind*
    ("M-x" . counsel-M-x)
    ("M-y" . counsel-yank-pop)
    ("C-c g" . counsel-rg))

  (use-package undo-tree
    :ensure t
    :diminish undo-tree-mode
    :init
    (global-undo-tree-mode)
    :bind*
    ("M-/" . undo-tree-visualize))

  (use-package multiple-cursors
    :ensure t
    :bind*
    ("M-n" . mc/mark-next-like-this)
    ("M-p" . mc/mark-previous-like-this))

  (use-package git-gutter
    :ensure t
    :diminish git-gutter-mode
    :init
    (global-git-gutter-mode t))

  (use-package magit
    :ensure t
    :bind*
    ("<f11>" . magit-log)
    ("<f12>" . magit-status))

  (use-package ws-butler
    :ensure t
    :diminish ws-butler-mode
    :init
    (ws-butler-global-mode))

  (use-package dumb-jump
    :ensure t
    :config
    (setq dumb-jump-selector 'ivy)
    (setq dumb-jump-force-searcher (cond ((executable-find "rg") 'rg)
                                         ((executable-find "ag") 'ag)
                                         (t nil)))
    :bind*
    ("M-." . dumb-jump-go)
    ("C-." . dumb-jump-back))

  (use-package exec-path-from-shell
    :ensure t
    :init
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))

(defun my/looks ()
  (use-package arjen-grey-theme
    :ensure t
    :init
    (load-theme 'arjen-grey t))

  (defconst fira-code-char-regexp-alist
    '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
      (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
      (36 . ".\\(?:>\\)")
      (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
      (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
      (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
      (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
      (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
      (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
      (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
      (48 . ".\\(?:x[a-zA-Z]\\)")
      (58 . ".\\(?:::\\|[:=]\\)")
      (59 . ".\\(?:;;\\|;\\)")
      (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
      (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
      (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
      (63 . ".\\(?:\\(\\?\\?\\)\\|[=?]\\)")
      (91 . ".\\(?:]\\)")
      (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
      (94 . ".\\(?:=\\)")
      (119 . ".\\(?:ww\\)")
      (123 . ".\\(?:-\\)")
      (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
      (126 . ".\\(?:~[>~]\\|[>=@~-]\\)")))

  (dolist (char-regexp fira-code-char-regexp-alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring])))

  (my/set-font-height my/default-font-height))

(defun my/markdown ()
  (use-package markdown-mode
    :ensure t
    :hook (markdown-mode . flyspell-mode)
    :mode "\\.md?\\'")

  (use-package flyspell
    :defer t
    :config
    (setq ispell-program-name (executable-find "aspell"))))

(defun my/javascript ()
  (use-package rjsx-mode
    :ensure t
    :mode ("\\.js?\\'" . rjsx-mode)
    :config
    ;; delegate syntax error highlighting to flycheck
    (setq js2-mode-show-parse-errors nil)
    (setq js2-mode-show-strict-warnings nil))

  (use-package add-node-modules-path
    :ensure t
    :defer t
    :hook (rjsx-mode . add-node-modules-path))

  (use-package prettier-js
    :ensure t
    :defer t
    :hook (rjsx-mode . prettier-js-mode))

  (use-package flycheck
    :ensure t
    :defer t
    :hook (rjsx-mode . flycheck-mode))

  (use-package emmet-mode
    :ensure t
    :defer t
    :hook (rjsx-mode . emmet-mode))

  (use-package tern
    :ensure t
    :hook (rjsx-mode . tern-mode)
    :config
    (setq tern-command (append tern-command '("--no-port-file"))))

  (use-package company-tern
    :ensure t
    :config
    (add-to-list 'company-backends 'company-tern)))

(defun my/elm ()
  (use-package elm-mode
    :ensure t
    :config
    (add-to-list 'company-backends 'company-elm)
    (setq elm-format-on-save t))

  (use-package flycheck
    :ensure t
    :defer t
    :hook (elm-mode . flycheck-mode))

  (use-package flycheck-elm
    :ensure t
    :defer t
    :hook (elm-mode . flycheck-elm-setup)))

(my/setup-use-package)
(my/set-general-settings)
(my/set-default-keybindings)
(my/add-core-packages)
(my/looks)

(my/markdown)
(my/javascript)
(my/elm)
