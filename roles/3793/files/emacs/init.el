;;; init.el --- init.el

;;; Commentary:

;;; Code:

(defvar my/packages
  '(
    ac-cider
    ag
    alchemist
    clojure-mode
    cider
    coffee-mode
    company
    dockerfile-mode
    editorconfig
    elixir-mode
    elscreen
    erlang
    evil
    evil-matchit
    evil-surround
    evil-tabs
    feature-mode
    flycheck
    haskell-mode
    helm
    helm-ag
    j-mode
    markdown-mode
    monokai-theme
    origami
    osx-clipboard
    package-utils
    plantuml-mode
    projectile
    robe
    slime
    slim-mode
    vimrc-mode
    yaml-mode
    ))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(package-selected-packages my/packages)
 '(plantuml-jar-path "/usr/local/Cellar/plantuml/1.2017.14/libexec/plantuml.jar")
 '(ruby-insert-encoding-magic-comment nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq-default indent-tabs-mode nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'linum)
(global-linum-mode 1)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
    (url-retrieve-synchronously "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(el-get-bundle 'crystal-mode)
; (el-get-bundle 'proof-general)
(el-get-bundle 'proof-general
       :description "A generic Emacs interface for interactive proof assistants."
       :type github
       :pkgname "ProofGeneral/PG"
       :build `(("make" "clean")
                ("make" ,(concat "EMACS=" el-get-emacs) "compile"))
       :info "doc"
       :autoloads "generic/proof-site.el"
       :website "http://proofgeneral.inf.ed.ac.uk/")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(setq package-archive-priorities
      '(("gnu" . 5)
        ("melpa" . 0)
        ("melpa-stable" . 10)))
(package-initialize)
; (package-refresh-contents)
(dolist (package my/packages)
  (unless (package-installed-p package) (package-install package)))

;; ac-cider
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

;; ag

;; alchemist

;; clojure-mode

;; cider

;; coffee-mode

;; company
(add-hook 'after-init-hook 'global-company-mode)

;; dockerfile-mode

;; editorconfig
(editorconfig-mode 1)

;; elixir-mode

;; elscreen

;; erlang

;; evil
(evil-mode 1)

;; evil-matchit
(global-evil-matchit-mode 1)

;; evil-surround
(global-evil-surround-mode 1)

;; evil-tabs
(global-evil-tabs-mode t)

;; feature-mode

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; haskell-mode

;; helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)

;; helm-ag

;; j-mode

;; markdown-mode

;; monokai-theme
(load-theme 'monokai t)

;; origami
(global-origami-mode t)
(with-eval-after-load 'origami
  (define-key evil-normal-state-map "za" 'origami-toggle-node)
  (define-key evil-normal-state-map "zA" 'origami-recursively-toggle-node)
  (define-key evil-normal-state-map "zc" 'origami-close-node)
  (define-key evil-normal-state-map "zC" 'origami-close-node-recursively)
  (define-key evil-normal-state-map "zo" 'origami-open-node)
  (define-key evil-normal-state-map "zO" 'origami-open-node-recursively)
  (define-key evil-normal-state-map "zv" 'origami-show-node))

;; osx-clipboard
(osx-clipboard-mode +1)

;; package-utils

;; plantuml-mode
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;; projectile

;; robe
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

;; slime
(setq inferior-lisp-program "/usr/local/bin/clisp")
(setq slime-contribs '(slime-fancy))

;; slim-mode

;; vimrc-mode

;; yaml-mode

;;; init.el ends here
