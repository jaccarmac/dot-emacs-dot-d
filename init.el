;;; -*- lexical-binding: t; -*-

(defvar network-security-level)
(setf network-security-level 'high)
;; bugfix for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(setf gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setf default-directory "~")

(eval-and-compile
  (package-initialize)
  (setf quelpa-update-melpa-p nil)
  (unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents
       "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
      (eval-buffer)
      (quelpa-self-upgrade)))
  (add-to-list 'package-selected-packages 'quelpa))

(eval-when-compile
  (quelpa 'use-package)
  (require 'use-package)

  (quelpa '(quse-package :fetcher github :repo "jaccarmac/quse-package"))
  (require 'quse-package))
(add-to-list 'package-selected-packages 'use-package)
(add-to-list 'package-selected-packages 'quse-package)

(require 'bind-key)

(quse-package better-defaults)

(use-package saveplace :init
  (when (fboundp 'save-place-mode) (save-place-mode)))

(declare-function ido-everywhere "ido")
(ido-everywhere)
(setf ido-auto-merge-work-directories-length -1)

(quse-package ido-completing-read+
  :preface
  (defvar org-completion-use-ido)
  (defvar magit-completing-read-function)
  :init
  (ido-ubiquitous-mode)
  (setf org-completion-use-ido t)
  (setf magit-completing-read-function 'magit-ido-completing-read))

(quse-package ido-yes-or-no :init (ido-yes-or-no-mode))

(quse-package crm-custom :init (crm-custom-mode))

(quse-package smex
  :init (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(setq-default fill-column 79)

(prefer-coding-system 'utf-8-unix)

(setq-default inhibit-splash-screen t)

(quse-package (undo-tree :fetcher git
                         :url "https://gitlab.com/tsc25/undo-tree")
  :init
  (global-undo-tree-mode)
  (setf undo-tree-history-directory-alist
        `((".". ,(concat user-emacs-directory "undo-tree-history")))))

(quse-package powerline :init (powerline-default-theme))

(quse-package nord-theme
              :init
              (if (daemonp)
                  (cl-labels ((load-nord (frame)
                                         (with-selected-frame frame
                                           (load-theme 'nord t))
                                         (remove-hook
                                          'after-make-frame-functions
                                          #'load-nord)))
                    (add-hook 'after-make-frame-functions #'load-nord))
	        (load-theme 'nord t)))

(quse-package rainbow-delimiters)

(push '(font . "JuliaMono 11") default-frame-alist)

(quse-package emojify :config (global-emojify-mode))

(quse-package smartparens
  :init
  (smartparens-global-mode)
  (smartparens-global-strict-mode)
  (require 'smartparens-config)
  (sp-use-smartparens-bindings))

(setf custom-file "~/.emacs.d/custom.el")

(defun upgrade-and-clean-packages ()
  (interactive)
  (quelpa-upgrade-all)
  (package-autoremove)
  (customize-save-variable 'package-selected-packages nil))

(setf delete-by-moving-to-trash t)

(setf dired-dwim-target t)

(quse-package (org :fetcher git
                   :url "https://code.orgmode.org/bzg/org-mode.git"
                   :files ("lisp/*.el"
                           "contrib/lisp/*.el"
                           "doc/dir"
                           "doc/*.texi"))
  :init
  (setq-default major-mode 'org-mode)
  (use-package org-tempo))

(quse-package htmlize)

(quse-package ox-reveal)

(setf org-latex-pdf-process (list "latexmk -f -pdf %f"))

(add-to-list 'org-latex-classes
             '("apa6"
               "\\documentclass[man,12pt]{apa6}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(quse-package ledger-mode
  :init
  (add-to-list 'auto-mode-alist
               '("ledger.dat" . ledger-mode))
  (add-hook 'ledger-mode-hook (lambda ()
                                (setq-local tab-always-indent 'complete)
                                (setq-local completion-cycle-threshold t)
                                (setq-local ledger-complete-in-steps t))))

(quse-package password-store)

(quse-package projectile
  :init
  (projectile-mode)
  (setf projectile-switch-project-action 'projectile-dired)
  (setf projectile-indexing-method 'alien)
  :bind
  (("C-c p" . projectile-command-map)))

(quse-package magit)

(quse-package ssh-agency)

(quse-package editorconfig :config (editorconfig-mode 1))

(quse-package direx)

(quse-package auto-complete
  :init
  (require 'auto-complete-config)
  (ac-config-default))

(quse-package ac-capf)

(quse-package cider
  :init
  (setf cider-repl-tab-command 'indent-for-tab-command)
  (setf cider-jack-in-default 'boot)
  (setf cider-repl-history-file "~/.cider-repl-history")
  (setf cider-repl-display-help-banner nil))

(quse-package ac-cider
  :init
  (add-hook 'cider-mode-hook 'ac-cider-setup)
  (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
  (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
  (add-hook 'cider-repl-mode-hook 'ac-flyspell-workaround)
  (add-hook 'cider-clojure-interaction-mode-hook 'ac-cider-setup)
  (add-hook 'cider-clojure-interaction-mode-hook 'ac-flyspell-workaround)
  (eval-after-load "auto-complete"
    '(progn
       (add-to-list 'ac-modes 'cider-mode)
       (add-to-list 'ac-modes 'cider-repl-mode)
       (add-to-list 'ac-modes 'cider-clojure-interaction-mode))))

(quse-package slime
  :init
  (setf slime-lisp-implementations '((ros ("ros" "run"))
                                     (sbcl ("ros" "run" "-L" "sbcl-bin"))
                                     (ccl ("ros" "run" "-L" "ccl-bin"))
                                     (abcl ("ros" "run" "-L" "abcl-bin"))
                                     (ecl ("ros" "run" "-L" "ecl"))))
  (setf common-lisp-hyperspec-root (getenv "HYPERSPEC_ROOT"))
  (setf slime-contribs '(slime-fancy))
  (slime-setup))

(quse-package ac-slime
  :init
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'slime-repl-mode)))

(quse-package web-mode
  :init
  (setf web-mode-enable-engine-detection t)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'org-src-lang-modes '("html" . web))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'org-src-lang-modes '("css" . web))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'org-src-lang-modes '("js" . web))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode)))

(quse-package tern
  :config
  (setf tern-command '("tern"))
  (add-hook 'web-mode-hook 'tern-mode)
  (add-to-list 'auto-mode-alist '(".tern-project" . web-mode))
  (add-to-list 'web-mode-content-types '("json" . ".tern-project")))
(quse-package tern-auto-complete :config (tern-ac-setup))

(quse-package coffee-mode)

;; (quelpa 'eclim)
;; (add-to-list 'package-selected-packages 'eclim)
;; (use-package eclim :config (global-eclim-mode))
;; (use-package eclimd :config (setf eclimd-wait-for-process nil))
;; (quse-package ac-emacs-eclim :config (ac-emacs-eclim-config))

(quse-package yasnippet)

(quse-package nim-mode
  :init
  (add-hook 'nim-mode-hook 'nimsuggest-mode))

(quse-package markdown-mode)

(quse-package go-mode
  :init
  (when (executable-find "goimports")
    (setf gofmt-command "goimports"))
  (add-hook 'before-save-hook #'gofmt-before-save)
  :bind (:map go-mode-map
              ("M-." . godef-jump)))

(quse-package go-autocomplete)

(quse-package go-guru)

(quse-package (protobuf-mode :fetcher github
                             :repo "google/protobuf"
                             :files ("editors/protobuf-mode.el")))

(quse-package yaml-mode)

(quse-package jedi
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (setf jedi:complete-on-dot t)
  (setf jedi:use-shortcuts t))

(quse-package virtualenvwrapper
  :preface
  (defvar python-environment-directory)
  (defvar venv-location)
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (let ((global-venv-location "~/.virtualenvs"))
    (setf python-environment-directory global-venv-location)
    (setf venv-location global-venv-location)))

(quse-package (hoon-mode :fetcher github :repo "urbit/hoon-mode.el"))

(quse-package (gdscript-mode :fetcher github
                             :repo "jaccarmac/gdscript-mode"
                             :branch "fix-line-endings"))

(quse-package dockerfile-mode :init (add-to-list
                                     'auto-mode-alist
                                     '("Dockerfile\\'" . dockerfile-mode)))

(quse-package docker)
(quse-package docker-tramp)

(quse-package csharp-mode)

(quse-package request-deferred)
(quse-package omnisharp
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

(quse-package lua-mode)

(quse-package edit-server
  :init (setf edit-server-new-frame nil)
  :config (edit-server-start))

(quse-package erlang)

(quse-package rust-mode
  :init (setf rust-format-on-save t))

(quse-package cargo :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; TODO check if this works

(quse-package lsp-mode)

(quse-package lsp-ui :init (add-hook 'rust-mode-hook 'lsp-ui-mode))

(quse-package lsp-rust :init (setf lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))

;; end TODO check

(quse-package toml-mode)

(quse-package mediawiki)

(quse-package (xelb :fetcher github :repo "ch11ng/xelb"))
(quse-package (exwm :fetcher github :repo "ch11ng/exwm")
  :preface
  (declare-function exwm-config-example "exwm")
  :config
  (require 'exwm-config)
  (exwm-config-example))

(quse-package erc-twitch
  :preface
  (declare-function erc-twitch-enable "erc-twitch")
  :config
  (erc-twitch-enable))

(use-package zone
  :init
  (defun lock-screen () ; TODO: pick a random function, show it, zone on that
    ;; https://www.reddit.com/r/lisp/comments/dprpt6/is_there_a_way_to_get_every_symbol_that_is_of/f5yb5fi/
    ;;
    ;; CL code:
    ;; (let (list)
    ;;   (do-all-symbols (x)
    ;;     (when (and (fboundp x)
    ;;           (not (macro-function x))
    ;;           (not (special-operator-p x)))
    ;;       (push x list)))
    ;;   list)
    "Lock screen using (zone) and xtrlock
 calls M-x zone on all frames and runs xtrlock"
    (interactive)
    (save-excursion
      ;; (shell-command "xtrlock &")
      (set-process-sentinel
       (start-process "xtrlock" nil "xtrlock")
       '(lambda (process event)
          (zone-leave-me-alone)))
      (zone-when-idle 1)))
  :commands (zone-leave-me-alone zone-when-idle))

(quse-package mingus)

(quse-package (mingus-header-mode :repo "atheriel/mingus-header-mode" :fetcher github)
  :init (add-hook 'mingus-playlist-hooks 'mingus-header-mode))

(quse-package elm-mode :init (setf elm-format-on-save t))

(quse-package intero
  :init
  (add-hook 'haskell-mode-hook 'intero-mode)
  (setf haskell-stylish-on-save t))

(quse-package fsharp-mode)

;; I'm building this in Guix at the moment. The environment I'm using looks
;; like this.
;;
;; guix environment --ad-hoc emacs gcc-toolchain zlib glib gobject-introspection cairo libpng poppler -- emacs
;;
;; Then (pdf-tools-install nil t) from inside Emacs.
(quse-package pdf-tools)

(savehist-mode)

(quse-package golden-ratio
  :init
  (golden-ratio-mode)
  (define-advice select-window
      (:after (_window &optional _no-record) golden-ratio-resize-window)
    (golden-ratio)
    nil))

(quse-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x 1" . switch-window-then-maximize)
         ("C-x 2" . switch-window-then-split-below)
         ("C-x 3" . switch-window-then-split-right)
         ("C-x 0" . switch-window-then-delete)))

(quse-package zig-mode)

(quse-package ox-pandoc)

(quse-package (bug-hunter :fetcher github :repo "Malabarba/elisp-bug-hunter"))

(quse-package (inform7-mode :fetcher github :repo "jaccarmac/inform7-mode" :branch "extension-files"))

(quse-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h F" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)))

(quelpa 'fuel)
(add-to-list 'package-selected-packages 'fuel)
(use-package fuel-mode
  :init (setf fuel-factor-root-dir (getenv "FACTOR_ROOT")))

(quse-package (fast-scroll :fetcher github :repo "ahungry/fast-scroll")
  :config
  (fast-scroll-config)
  (fast-scroll-advice-scroll-functions))

(customize-save-variable 'package-selected-packages package-selected-packages)
