;;; init.el --- init.el

;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 2) '(js-switch-indent-offset 2)
 '(package-selected-packages
   '(web-mode
     vimrc-mode
     ;use-package
     undo-tree
     typescript-mode
     slim-mode
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
     monokai-theme
     molokai-theme
     magit
     lua-mode
     lsp-ui
     lsp-metals
     jsonnet-mode
     j-mode
     haskell-mode
     groovy-mode
     go-mode
     flycheck-golangci-lint
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
(defvar delete-trailing-whitespece-before-save t)
(make-variable-buffer-local 'delete-trailing-whitespece-before-save)
(advice-add
 'delete-trailing-whitespace
 :before-while (lambda () delete-trailing-whitespece-before-save))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook
 'markdown-mode-hook
 '(lambda ()
    (set
     (make-local-variable
      'delete-trailing-whitespece-before-save)
     nil)))
(add-hook
 'yaml-mode-hook
 '(lambda ()
    (set
     (make-local-variable
      'delete-trailing-whitespece-before-save)
     nil)))

(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))

(require 'linum)
(global-linum-mode 1)

;; package 管理 tool を設定する

(require 'package)
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             t)
(setq package-archive-priorities
      '(("gnu" . 5) ("melpa" . 0) ("melpa-stable" . 10)))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

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

(use-package
 company
 :init (add-hook 'after-init-hook 'global-company-mode))

(use-package company-terraform :init (company-terraform-init))

(el-get-bundle
  'copilot
  :description "An unofficial Copilot plugin for Emacs."
  :type github
  :pkgname "zerolfx/copilot.el"
  :info "readme.md"
  :autoloads "copilot.el"
  :website "https://github.com/zerolfx/copilot.el")

(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode))

(use-package
 typescript-mode
 :init
 ;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
 (add-hook 'typescript-mode-hook 'eglot-ensure))

(use-package company-web :init (require 'company-web-html))

; Ivy - a generic completion frontend for Emacs, Swiper - isearch with an overview, and more. Oh, man!
(use-package
 counsel
 :init
 (global-set-key (kbd "M-x") 'counsel-M-x)
 (global-set-key (kbd "<f1> f") 'counsel-describe-function)
 (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
 (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
 (global-set-key (kbd "<f1> l") 'counsel-find-library)
 (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
 (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
 (global-set-key (kbd "C-c g") 'counsel-git)
 (global-set-key (kbd "C-c j") 'counsel-git-grep)
 (global-set-key (kbd "C-c k") 'counsel-ag)
 (global-set-key (kbd "C-x l") 'counsel-locate)
 (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
 (define-key
  minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;(use-package dap-mode
;  :init
;  (dap-mode 1)
;  (dap-ui-mode 1)
;  (dap-tooltip-mode 1)
;  (tooltip-mode 1)
;  (dap-ui-controls-mode 1))

; 指定したマイナーモードを表示しない(diminish篇) - Qiita https://qiita.com/tadsan/items/c859c5c04724cbda75fc
(defmacro safe-diminish (file mode &optional new-name)
  "https://github.com/larstvei/dot-emacs/blob/master/init.org"
  `(with-eval-after-load ,file
     (diminish ,mode ,new-name)))
(use-package
 diminish
 :init
 (safe-diminish "company" 'company-mode)
 (safe-diminish "editorconfig" 'editorconfig-mode)
 (safe-diminish "eldoc" 'eldoc-mode)
 (safe-diminish "osx-clipboard" 'osx-clipboard-mode)
 (safe-diminish "smartparens" 'smartparens-mode)
 (safe-diminish "undo-tree" 'undo-tree-mode))

(use-package dockerfile-mode)

(use-package
 doom-themes
 :init
 (setq
  doom-themes-enable-bold t
  doom-themes-enable-italic t)
 (load-theme 'doom-molokai t))

(use-package editorconfig :init (editorconfig-mode 1))

(use-package eglot)

(use-package elscreen)

(use-package erlang :init (add-hook 'erlang-mode-hook 'eglot-ensure))

(use-package evil :init (evil-mode 1))

(use-package evil-indent-textobject)

(use-package evil-leader :init (global-evil-leader-mode))

(use-package evil-matchit :init (global-evil-matchit-mode 1))

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

(use-package evil-surround :init (global-evil-surround-mode 1))

(use-package evil-tabs :init (global-evil-tabs-mode t))

(use-package feature-mode)

(use-package flycheck :hook (after-init . global-flycheck-mode))

(use-package
 flycheck-golangci-lint
 :init
 (eval-after-load 'flycheck
   '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))
 (setq flycheck-golangci-lint-fast t))

(use-package
 go-mode
 :init (add-hook 'go-mode-hook 'eglot-ensure)
 (add-hook
  'go-mode-hook
  (lambda () (add-hook 'before-save-hook 'gofmt-before-save nil t))))
;(require 'project)
;(defun project-find-go-module (dir)
;  (when-let ((root (locate-dominating-file dir "go.mod")))
;    (cons 'go-module root)))
;(cl-defmethod project-root ((project (head go-module)))
;  (cdr project))
;(add-hook 'project-find-functions #'project-find-go-module)

;(use-package graphql-mode)

(use-package groovy-mode)

(use-package haskell-mode)

; Ivy - a generic completion frontend for Emacs, Swiper - isearch with an overview, and more. Oh, man!
(use-package
 ivy
 :init
 (ivy-mode 1)
 (setq ivy-use-virtual-buffers t)
 (setq enable-recursive-minibuffers t)
 (global-set-key (kbd "C-c C-r") 'ivy-resume)
 (global-set-key (kbd "<f6>") 'ivy-resume))

;(use-package j-mode)

(use-package jinja2-mode)

(use-package jsonnet-mode)

(use-package lua-mode)

(use-package magit)

(use-package markdown-mode)

;(use-package monokai-theme
;  :init
;  (load-theme 'monokai t))

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
 :init (global-origami-mode t)
 (with-eval-after-load 'origami
   (define-key evil-normal-state-map "za" 'origami-toggle-node)
   (define-key
    evil-normal-state-map "zA" 'origami-recursively-toggle-node)
   (define-key evil-normal-state-map "zc" 'origami-close-node)
   (define-key
    evil-normal-state-map "zC" 'origami-close-node-recursively)
   (define-key evil-normal-state-map "zo" 'origami-open-node)
   (define-key
    evil-normal-state-map "zO" 'origami-open-node-recursively)
   (define-key evil-normal-state-map "zv" 'origami-show-node)))

(use-package osx-clipboard :init (osx-clipboard-mode +1))

; See ~/.emacs-live.el
(el-get-bundle
 'overtone-emacs-live
 :type http-zip
 :url "https://github.com/overtone/emacs-live/archive/master.zip"
 ;; NOTE: git submodule update に失敗する
 ;; :type github
 ;; :pkgname "overtone/emacs-live"
 :build
 (let*
     ((username "my")
      (src-dir
       (substitute-in-file-name
        "$HOME/.emacs.d/el-get/overtone-emacs-live/packs/template/user-template-pack/"))
      (dest-dir
       (substitute-in-file-name
        (concat "$HOME/.live-packs/" username "-pack"))))
   `(("mkdir" "-p" ,(eval dest-dir))
     ("cp" "-R" ,(eval src-dir) ,(eval dest-dir)))))

(use-package package-utils)

(use-package php-mode)

(use-package poly-ansible)

(use-package
 prettier-js
 :init
 (add-hook 'js2-mode-hook 'prettier-js-mode)
 (eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)
       (add-hook 'web-mode-hook #'prettier-js-mode))))

(use-package
 projectile
 :init (global-set-key (kbd "C-x C-f") 'projectile-find-file)
 ;(defun projectile-find-file-when-find-file-not-found ()
 ;  "When find-file-not-found then projectile-find-file."
 ;  (require 'projectile)
 ;  (if (projectile-project-p)
 ;    (do
 ;      (interactive)
 ;      (let* ((project-root (projectile-ensure-project (projectile-project-root)))
 ;              (file-name (substring buffer-file-name (length project-root)))
 ;              (file (projectile-completing-read
 ;                      "Find file: "
 ;                      (projectile-project-files project-root)
 ;                      :initial-input file-name)))
 ;        (when file
 ;          (funcall #'find-file (expand-file-name file project-root))
 ;          (run-hooks 'projectile-find-file-hook)
 ;          t)))))
 ;(add-hook 'find-file-not-found-hooks 'projectile-find-file-when-find-file-not-found)
 )

(el-get-bundle 'proof-general)
(el-get-bundle
 'proof-general
 :description "A generic Emacs interface for interactive proof assistants."
 :type github
 :pkgname "ProofGeneral/PG"
 :build
 `(("make" "clean")
   ("make" ,(concat "EMACS=" el-get-emacs) "compile"))
 :info "doc"
 :autoloads "generic/proof-site.el"
 :website "http://proofgeneral.inf.ed.ac.uk/")

(use-package
 quickrun
 :init
 (quickrun-add-command
  "clojure/babashka"
  '((:command . "bb") (:exec . "%c %s"))
  :default "clojure"))

(use-package rust-mode :init (setq rust-format-on-save t))

(use-package
 sbt-mode
 :init
 (substitute-key-definition
  'minibuffer-complete-word
  'self-insert-command
  minibuffer-local-completion-map)
 ;(setq :program-options '("-Dsbt.supershell=false"))
 )

(use-package
 scala-mode
 :init (add-hook 'scala-mode-hook 'eglot-ensure))

(use-package slim-mode)

(use-package
 smartparens
 :init
 ;(smartparens-global-mode t)
 (smartparens-strict-mode t)
 :hook emacs-lisp-mode)

; Ivy - a generic completion frontend for Emacs, Swiper - isearch with an overview, and more. Oh, man!
(use-package swiper :init (global-set-key "\C-s" 'swiper))

(use-package terraform-mode)

(use-package typescript-mode)

(use-package use-package)

(use-package vimrc-mode)

(use-package web-mode)

(use-package yaml-mode)

;;; init.el ends here
