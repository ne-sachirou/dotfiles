;;; init.el --- init.el

;;; Commentary:

;;; Code:

(defvar my/packages
  '(
    ac-cider
    ag
    cider
    clojure-mode
    coffee-mode
    company
    company-terraform
    company-web
    counsel
    csharp-mode
    ; dap-mode
    diminish
    docker-tramp
    dockerfile-mode
    doom-themes
    editorconfig
    elixir-mode
    elscreen
    erlang
    evil
    evil-leader
    evil-matchit
    evil-smartparens
    evil-surround
    evil-tabs
    feature-mode
    flycheck
    flycheck-golangci-lint
    go-mode
    groovy-mode
    haskell-mode
    ivy
    j-mode
    jinja2-mode
    lsp-metals
    lsp-mode
    lsp-ui
    lua-mode
    magit
    markdown-mode
    ; monokai-theme
    multi-term
    nginx-mode
    origami
    osx-clipboard
    package-utils
    php-mode
    plantuml-mode
    poly-ansible
    prettier-js
    projectile
    quickrun
    rust-mode
    sbt-mode
    scala-mode
    slim-mode
    smartparens
    swiper
    terraform-mode
    typescript-mode
    vimrc-mode
    web-mode
    yaml-mode
    ))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" default))
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(package-selected-packages
   '(ac-cider ag clojure-mode cider coffee-mode company company-lsp company-terraform company-web csharp-mode docker-tramp dockerfile-mode doom-themes editorconfig elixir-mode elscreen erlang evil evil-leader evil-matchit evil-smartparens evil-surround evil-tabs feature-mode flycheck go-mode groovy-mode haskell-mode j-mode jinja2-mode lsp-mode lsp-ui magit markdown-mode multi-term nginx-mode origami osx-clipboard package-utils plantuml-mode poly-ansible prettier-js projectile quickrun sbt-mode scala-mode slim-mode terraform-mode typescript-mode vimrc-mode yaml-mode web-mode))
 '(plantuml-jar-path
   (let*
       ((exe-path
         (car
          (split-string
           (shell-command-to-string "which plantuml"))))
        (exe-str
         (with-temp-buffer
           (insert-file-contents exe-path)
           (buffer-string))))
     (string-match "[/.A-Za-z0-9]+/plantuml.jar" exe-str)
     (substring exe-str
                (match-beginning 0)
                (match-end 0))))
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

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq-default indent-tabs-mode nil)

(defvar delete-trailing-whitespece-before-save t)
(make-variable-buffer-local 'delete-trailing-whitespece-before-save)
(advice-add 'delete-trailing-whitespace :before-while
  (lambda () delete-trailing-whitespece-before-save))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'markdown-mode-hook
  '(lambda () (set (make-local-variable 'delete-trailing-whitespece-before-save) nil)))
(add-hook 'yaml-mode-hook
  '(lambda () (set (make-local-variable 'delete-trailing-whitespece-before-save) nil)))

(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))

(require 'linum)
(global-linum-mode 1)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
    (url-retrieve-synchronously "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
; (el-get-bundle 'overtone-emacs-live
;   :type http-zip
;   :url "https://github.com/overtone/emacs-live/archive/master.zip"
;   :build (let* ((username "my")
;                 (src-dir (substitute-in-file-name "$HOME/.emacs.d/el-get/overtone-emacs-live/packs/template/user-template-pack/"))
;                 (dest-dir (substitute-in-file-name (concat "$HOME/.live-packs/" username "-pack"))))
;            `(("mkdir" "-p" ,(eval dest-dir))
;              ("cp" "-R" ,(eval src-dir) ,(eval dest-dir)))))
(el-get-bundle 'proof-general)
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
; https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure
; https://emacs.stackexchange.com/questions/51721/failed-to-download-gnu-archive
; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
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

;; cider
(add-hook 'clojure-mode-hook #'cider-mode)

;; clojure-mode
(add-hook 'clojure-mode-hook #'smartparens-mode)
(add-hook 'clojure-mode-hook #'subword-mode)
(add-to-list 'auto-mode-alist '("\\.clje\\'" . clojure-mode))

;; coffee-mode

;; company
(add-hook 'after-init-hook 'global-company-mode)

;; company-terraform
(company-terraform-init)

;; typescript-mode
;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-hook 'typescript-mode-hook #'lsp)

;; company-web
(require 'company-web-html)

;; counsel
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
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; csharp-mode

;; dap-mode
; (dap-mode 1)
; (dap-ui-mode 1)
; (dap-tooltip-mode 1)
; (tooltip-mode 1)
; (dap-ui-controls-mode 1)

;; diminish
; 指定したマイナーモードを表示しない(diminish篇) - Qiita https://qiita.com/tadsan/items/c859c5c04724cbda75fc
(defmacro safe-diminish (file mode &optional new-name)
  "https://github.com/larstvei/dot-emacs/blob/master/init.org"
  `(with-eval-after-load ,file
     (diminish ,mode ,new-name)))
(safe-diminish "company" 'company-mode)
(safe-diminish "editorconfig" 'editorconfig-mode)
(safe-diminish "eldoc" 'eldoc-mode)
(safe-diminish "osx-clipboard" 'osx-clipboard-mode)
(safe-diminish "smartparens" 'smartparens-mode)
(safe-diminish "undo-tree" 'undo-tree-mode)

;; docker-tramp
(require 'docker-tramp-compat)
(set-variable 'docker-tramp-use-names t)

;; dockerfile-mode

;; doom-themes
(setq doom-themes-enable-bold t
  doom-themes-enable-italic t)
(load-theme 'doom-molokai t)

;; editorconfig
(editorconfig-mode 1)

;; elixir-mode
; (add-hook 'elixir-mode-hook #'lsp)
(add-hook 'elixir-mode-hook
  (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
(add-hook 'elixir-format-hook
  (lambda ()
    (if (projectile-project-p)
      (setq elixir-format-arguments
        (list "--dot-formatter"
              (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
      (setq elixir-format-arguments nil))))

;; elscreen

;; erlang
; (add-hook 'erlang-mode-hook #'lsp)

;; evil
(evil-mode 1)

;; evil-leader
(global-evil-leader-mode)

;; evil-matchit
(global-evil-matchit-mode 1)

;; evil-smartparens
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
; Slurp Barf · Clojure development with Spacemacs & Cider https://practicalli.github.io/spacemacs/structured-editing/lisp-state-slurp-barf.html
(evil-leader/set-key
  "kb" 'sp-forward-barf-sexp
  "kB" 'sp-backward-barf-sexp
  "ks" 'sp-forward-slurp-sexp
  "kS" 'sp-backward-slurp-sexp)

;; evil-surround
(global-evil-surround-mode 1)

;; evil-tabs
(global-evil-tabs-mode t)

;; feature-mode

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; flycheck-golangci-lint
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))
(setq flycheck-golangci-lint-fast t)

;; go-mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook
  (lambda () (add-hook 'before-save-hook 'gofmt-before-save nil t)))

;; groovy-mode

;; haskell-mode

;; ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)

;; j-mode

;; jinja2-mode

;; lsp-metals

;; lsp-mode
(setq lsp-enable-snippet nil)
(setq lsp-prefer-capf t)
(setq lsp-prefer-flymake nil)
; (add-hook 'lsp-mode-hook #'dap-mode)
; (add-hook 'lsp-mode-hook #'dap-ui-mode)
(add-hook 'lsp-mode-hook #'lsp-lens-mode)

;; lsp-ui

;; lua-mode

;; magit

;; markdown-mode

;; monokai-theme
; (load-theme 'monokai t)

;; multi-term
(setq multi-term-program "/bin/zsh")

;; nginx-mode

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

;; overtone-emacs-live
; See ~/.emacs-live.el

;; package-utils

;; php-mode

;; plantuml-mode
(add-to-list 'auto-mode-alist '("\\.\\(plant\\)?uml\\'" . plantuml-mode))

;; poly-ansible

;; prettier-js
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(setq prettier-js-args '())

;; projectile
(global-set-key (kbd "C-x C-f") 'projectile-find-file)
; (defun projectile-find-file-when-find-file-not-found ()
;   "When find-file-not-found then projectile-find-file."
;   (require 'projectile)
;   (if (projectile-project-p)
;     (do
;       (interactive)
;       (let* ((project-root (projectile-ensure-project (projectile-project-root)))
;               (file-name (substring buffer-file-name (length project-root)))
;               (file (projectile-completing-read
;                       "Find file: "
;                       (projectile-project-files project-root)
;                       :initial-input file-name)))
;         (when file
;           (funcall #'find-file (expand-file-name file project-root))
;           (run-hooks 'projectile-find-file-hook)
;           t)))))
; (add-hook 'find-file-not-found-hooks 'projectile-find-file-when-find-file-not-found)

;; proof-general

;; quickrun

;; rust-mode
(setq rust-format-on-save t)

;; sbt-mode
(substitute-key-definition
  'minibuffer-complete-word
  'self-insert-command
  minibuffer-local-completion-map)
; (setq :program-options '("-Dsbt.supershell=false"))

;; scala-mode
(add-hook 'scala-mode-hook #'lsp)

;; slim-mode

;; smartparens
; (smartparens-global-mode t)
(smartparens-strict-mode t)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)

;; swiper
(global-set-key "\C-s" 'swiper)

;; terraform-mode

;; vimrc-mode

;; web-mode

;; yaml-mode

;;; init.el ends here
