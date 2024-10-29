;;; init.el --- init.el

;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78"
     default))
 '(js-indent-level 2) '(js-switch-indent-offset 2)
 '(package-selected-packages
   '(web-mode
     vimrc-mode
     undo-tree
     typescript-mode
     sbt-mode
     rust-mode
     quickrun
     projectile
     prettier-js
     poly-ansible
     plantuml-mode
     php-mode
     package-utils
     osx-clipboard
     origami
     nov
     nixpkgs-fmt
     nix-mode
     nginx-mode
     multi-term
     magit
     lua-mode
     lsp-ui
     lsp-metals
     jsonnet-mode
     haskell-mode
     go-mode
     github-browse-file
     feature-mode
     evil-tabs
     evil-surround
     evil-smartparens
     evil-matchit
     evil-leader
     evil-indent-textobject
     erlang
     elixir-mode
     eglot
     editorconfig
     doom-themes
     dockerfile-mode
     diminish
     csharp-mode
     counsel
     company-web
     company-terraform
     coffee-mode
     async
     all-the-icons
     ag
     ac-cider))
 '(ruby-insert-encoding-magic-comment nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when window-system
  (set-frame-size (selected-frame) 160 63))
(setq visible-bell 1)

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq-default indent-tabs-mode nil)

;; file 保存前に行末尾空白を消す
;; EditorConfig で充分では?
; (defvar delete-trailing-whitespece-before-save t)
; (make-variable-buffer-local 'delete-trailing-whitespece-before-save)
; (advice-add
;  'delete-trailing-whitespace
;  :before-while (lambda () delete-trailing-whitespece-before-save))
; (add-hook 'before-save-hook 'delete-trailing-whitespace)
; (add-hook
;  'markdown-mode-hook
;  '(lambda ()
;     (set
;      (make-local-variable
;       'delete-trailing-whitespece-before-save)
;      nil)))
; (add-hook
;  'yaml-mode-hook
;  '(lambda ()
;     (set
;      (make-local-variable
;       'delete-trailing-whitespece-before-save)
;      nil)))

(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))

; (require 'linum)
; (global-linum-mode 1)
(global-display-line-numbers-mode)

;; package 管理 tool を設定する

(require 'package)
(package-initialize)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             t)
(setq package-archive-priorities
      '(("gnu" . 5) ("melpa" . 0) ("melpa-stable" . 10)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq
 use-package-always-ensure t
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

; https://github.com/radian-software/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent
         'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
; (unless (require 'el-get nil t)
;   (url-retrieve
;    "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
;    (lambda (s)
;      (end-of-buffer)
;      (eval-print-last-sexp))))

;; package を install して設定する

(use-package ag)

(use-package cider :hook clojure-mode)

(use-package
 clojure-mode
 :init
 (add-to-list 'auto-mode-alist '("\\.clje\\'" . clojure-mode))
 (add-hook 'clojure-mode-hook 'eglot-ensure)
 :hook
 ((clojure-mode . smartparens-mode) (clojure-mode . subword-mode)))

(use-package company :hook ((after-init . global-company-mode)))

(use-package company-terraform :init (company-terraform-init))

(use-package
 copilot
 :straight
 (:host
  github
  :repo "copilot-emacs/copilot.el"
  :files ("dist" "*.el"))
 :bind
 (:map
  copilot-completion-map
  ("<tab>" . copilot-accept-completion)
  ("TAB" . copilot-accept-completion))
 :hook (prog-mode . copilot-mode))

(use-package
 csv-mode
 :init (add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode)))

(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook
 ((emacs-lisp-mode . elisp-autofmt-mode)
  (emacs-lisp-mode
   .
   (lambda ()
     (add-hook 'before-save-hook 'elisp-autofmt-buffer nil t)))))

(use-package
 typescript-mode
 :init
 ; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
 :hook eglot-ensure)

(use-package company-web :init (require 'company-web-html))

;; Ivy - a generic completion frontend for Emacs, Swiper - isearch with an overview, and more. Oh, man!
(use-package
 counsel
 :bind
 (("M-x" . counsel-M-x)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> o" . counsel-describe-symbol)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("C-x l" . counsel-locate)
  ("C-S-o" . counsel-rhythmbox)
  (:map minibuffer-local-map ("C-r" . counsel-minibuffer-history))))

; (use-package dap-mode
; :init
;   (dap-mode 1)
;   (dap-ui-mode 1)
;   (dap-tooltip-mode 1)
;   (tooltip-mode 1)
;   (dap-ui-controls-mode 1))

;; 指定したマイナーモードを表示しない(diminish篇) - Qiita https://qiita.com/tadsan/items/c859c5c04724cbda75fc
(defmacro safe-diminish (file mode &optional new-name)
  "https://github.com/larstvei/dot-emacs/blob/master/init.org"
  `(with-eval-after-load ,file
     (diminish ,mode ,new-name)))
(use-package
 diminish
 :config
 (safe-diminish "company" 'company-mode)
 (safe-diminish "copilot" 'copilot-mode)
 (safe-diminish "editorconfig" 'editorconfig-mode)
 (safe-diminish "eldoc" 'eldoc-mode)
 (safe-diminish "flycheck" 'flycheck-mode)
 (safe-diminish "ivy" 'ivy-mode)
 (safe-diminish "osx-clipboard" 'osx-clipboard-mode)
 (safe-diminish "smartparens" 'smartparens-mode)
 (safe-diminish "undo-tree" 'undo-tree-mode))

(use-package dockerfile-mode)

(use-package
 doom-themes
 :config
 (setq
  doom-themes-enable-bold t
  doom-themes-enable-italic t)
 (load-theme 'doom-molokai t))

(use-package editorconfig :config (editorconfig-mode 1))

(use-package eglot)

(use-package elscreen)

(use-package erlang :hook eglot-ensure)

(use-package evil :config (evil-mode 1))

(use-package evil-indent-textobject)

(use-package evil-leader :config (global-evil-leader-mode))

(use-package evil-matchit :config (global-evil-matchit-mode 1))

(use-package
 evil-smartparens
 :init
 ; Slurp Barf · Clojure development with Spacemacs & Cider https://practicalli.github.io/spacemacs/structured-editing/lisp-state-slurp-barf.html
 (evil-leader/set-key
  "kb"
  'sp-forward-barf-sexp
  "kB"
  'sp-backward-barf-sexp
  "ks"
  'sp-forward-slurp-sexp
  "kS"
  'sp-backward-slurp-sexp)
 :hook (smartparens-enabled . evil-smartparens-mode))

(use-package evil-surround :config (global-evil-surround-mode 1))

(use-package evil-tabs :config (global-evil-tabs-mode t))

(use-package feature-mode)

(use-package flycheck :init (global-flycheck-mode))

(use-package github-browse-file)

(use-package
 go-mode
 :hook
 ((go-mode . eglot-ensure)
  (go-mode . company-mode)
  (go-mode
   .
   (lambda ()
     (add-hook 'before-save-hook 'gofmt-before-save nil t)))))

(use-package haskell-mode)

; Ivy - a generic completion frontend for Emacs, Swiper - isearch with an overview, and more. Oh, man!
(use-package
 ivy
 :demand t
 :config
 (ivy-mode 1)
 (setq ivy-use-virtual-buffers t)
 (setq enable-recursive-minibuffers t)
 :bind (("C-c C-r" . ivy-resume) ("<f6>" . ivy-resume)))

(use-package jinja2-mode)

(use-package jsonnet-mode)

(use-package lua-mode)

(use-package magit)

(use-package markdown-mode)

(use-package multi-term :init (setq multi-term-program "/bin/zsh"))

(use-package nginx-mode)

(use-package
 nix-mode
 :init
 (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
 (add-hook 'nix-mode-hook 'eglot-ensure))

(use-package
 nixpkgs-fmt
 :init (add-hook 'nix-mode-hook 'nixpkgs-fmt-on-save-mode))

; EPUB を Emacs 上で讀む
(use-package
 nov
 :init
 (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

; fold - Vim日本語ドキュメント https://vim-jp.org/vimdoc-ja/fold.html
(use-package
 origami
 :config (global-origami-mode t)
 :bind
 (:map
  evil-normal-state-map
  ("za" . origami-toggle-node)
  ("zA" . origami-recursively-toggle-node)
  ("zc" . origami-close-node)
  ("zC" . origami-close-node-recursively)
  ("zo" . origami-open-node)
  ("zO" . origami-open-node-recursively)
  ("zv" . origami-show-node)
  ("zr" . origami-open-all-nodes)
  ("zm" . origami-close-all-nodes)
  ("zR" . origami-reset)))

(use-package osx-clipboard :config (osx-clipboard-mode +1))

; See ~/.emacs-live.el
; (el-get-bundle 'overtone-emacs-live
;  :type http-zip
;  :url "https://github.com/overtone/emacs-live/archive/master.zip"
;  ;; NOTE: git submodule update に失敗する
;  ;; :type github
;  ;; :pkgname "overtone/emacs-live"
;  :build
;  (let*
;      ((username "my")
;       (src-dir
;        (substitute-in-file-name
;         "$HOME/.emacs.d/el-get/overtone-emacs-live/packs/template/user-template-pack/"))
;       (dest-dir
;        (substitute-in-file-name
;         (concat "$HOME/.live-packs/" username "-pack"))))
;    `(("mkdir" "-p" ,(eval dest-dir))
;      ("cp" "-R" ,(eval src-dir) ,(eval dest-dir)))))

(use-package package-utils)

(use-package php-mode)

; (use-package poly-ansible)

;; Posframe is a pop-up tool that must be manually installed for dap-mode
;; (use-package posframe)

(use-package
 prettier-js
 :init (add-hook 'js2-mode-hook 'prettier-js-mode)
 (eval-after-load 'web-mode
   '(progn
      (add-hook 'web-mode-hook #'add-node-modules-path)
      (add-hook 'web-mode-hook #'prettier-js-mode))))

(use-package
 projectile
 :init (projectile-mode +1)
 :bind
 (("C-x C-f" . projectile-find-file)
  (:map
   projectile-mode-map
   ("s-p" . projectile-command-map)
   ("C-c p" . projectile-command-map))))

; (use-package proof-general
;   :streight (:host github :repo "ProofGeneral/PG"))
; (el-get-bundle 'proof-general)
; (el-get-bundle 'proof-general
;  :description "A generic Emacs interface for interactive proof assistants."
;  :type github
;  :pkgname "ProofGeneral/PG"
;  :build
;  `(("make" "clean")
;    ("make" ,(concat "EMACS=" el-get-emacs) "compile"))
;  :info "doc"
;  :autoloads "generic/proof-site.el"
;  :website "http://proofgeneral.inf.ed.ac.uk/")

(use-package
 quickrun
 :config
 (quickrun-add-command
  "clojure/babashka"
  '((:command . "bb") (:exec . "%c %s"))
  :default "clojure"))

(use-package rust-mode :init (setq rust-format-on-save t))

(use-package
 sbt-mode
 :commands sbt-start sbt-command
 :config
 ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
 ;; allows using SPACE when in the minibuffer
 (substitute-key-definition
  'minibuffer-complete-word
  'self-insert-command
  minibuffer-local-completion-map)
 ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
 (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package
 scala-mode
 :init (add-hook 'scala-mode-hook 'eglot-ensure)
 :hook ((scala-mode . eglot-ensure) (scala-mode . company-mode))
 :interpreter ("scala" . scala-mode))

(use-package
 smartparens
 :config
 ;(smartparens-global-mode t)
 (smartparens-strict-mode t)
 :hook emacs-lisp-mode)

; Ivy - a generic completion frontend for Emacs, Swiper - isearch with an overview, and more. Oh, man!
(use-package swiper :bind (("\C-s" . swiper)))

(use-package terraform-mode)

(use-package typescript-mode)

(use-package vimrc-mode)

(use-package web-mode)

(use-package yaml-mode)

;;; init.el ends here
