;;; -*- lexical-binding: t; -*-

(setf default-directory "~")

(package-initialize)
(setf quelpa-update-melpa-p nil)
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))
(add-to-list 'package-selected-packages 'quelpa)

(quelpa 'use-package)
(add-to-list 'package-selected-packages 'use-package)
(require 'use-package)

(quelpa '(quse-package :fetcher github :repo "jaccarmac/quse-package"))
(add-to-list 'package-selected-packages 'quse-package)
(require 'quse-package)

(quse-package better-defaults)

(use-package saveplace :init
  (when (fboundp 'save-place-mode) (save-place-mode)))

(ido-everywhere)

(quse-package ido-completing-read+
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

(quse-package undo-tree :init (global-undo-tree-mode))

(quse-package powerline :init (powerline-default-theme))

(quse-package material-theme :config (load-theme 'material t))

(quse-package rainbow-delimiters)

(setf default-frame-alist '((font . "Go Mono 10")))

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
  (quelpa-upgrade)
  (package-autoremove)
  (customize-save-variable 'package-selected-packages nil))

(setf delete-by-moving-to-trash t)

(setf dired-dwim-target t)

(quse-package (org :fetcher git
                   :url "git://orgmode.org/org-mode.git"
                   :files ("lisp/*.el"
                           "contrib/lisp/*.el"
                           "doc/dir"
                           "doc/*.texi"))
              :init
              (setq-default major-mode 'org-mode))

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
              :init (add-to-list 'auto-mode-alist
                                 '("ledger/.*\\.dat\\'" . ledger-mode)))

(quse-package password-store)

(quse-package projectile
              :init
              (projectile-global-mode)
              (setf projectile-switch-project-action 'projectile-dired)
              (setf projectile-indexing-method 'alien))

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
              (setf cider-default-repl-command "boot")
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
              (setf inferior-lisp-program "ros run")
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
              (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode)))

(quse-package tern
              :config
              (setf tern-command '("tern"))
              (add-hook 'web-mode-hook 'tern-mode)
              (add-to-list 'auto-mode-alist '(".tern-project" . web-mode))
              (add-to-list 'web-mode-content-types '("json" . ".tern-project")))
(quse-package tern-auto-complete :config (tern-ac-setup))

(quse-package coffee-mode)

(quelpa 'eclim)
(add-to-list 'package-selected-packages 'eclim)
(use-package eclim :config (global-eclim-mode))
(use-package eclimd :config (setf eclimd-wait-for-process nil))
(quse-package ac-emacs-eclim :config (ac-emacs-eclim-config))

(quse-package yasnippet)

(quse-package nim-mode
              :init
              (add-hook 'nim-mode-hook 'nimsuggest-mode)
              (add-hook 'nim-mode-hook 'ac-capf-setup))

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

(let ((global-venv-location "~/.virtualenvs"))
  (setf python-environment-directory global-venv-location)
  (setf venv-location global-venv-location))

(quse-package jedi
              :init
              (add-hook 'python-mode-hook 'jedi:setup)
              (setf jedi:complete-on-dot t)
              (setf jedi:use-shortcuts t))

(quse-package virtualenvwrapper
              :init
              (venv-initialize-interactive-shells)
              (venv-initialize-eshell))

(quse-package (hoon-mode :fetcher github
                         :repo "urbit/urbit"
                         :files ("extras/hoon-mode.el/hoon-mode.el")))

(quse-package (gdscript-mode :fetcher github
                             :repo "jaccarmac/gdscript-mode"
                             :branch "fix-line-endings"))

(quse-package dockerfile-mode :init (add-to-list
                                     'auto-mode-alist
                                     '("Dockerfile\\'" . dockerfile-mode)))

(quse-package docker :init (docker-global-mode))
(quse-package docker-tramp)

(quse-package csharp-mode)

(quse-package request-deferred)
(quse-package omnisharp
              :config
              (add-hook 'csharp-mode-hook 'omnisharp-mode)
              (setf omnisharp-use-http t))

(quse-package lua-mode)

(customize-save-variable 'package-selected-packages package-selected-packages)

(quse-package edit-server
              :init (setf edit-server-new-frame nil)
              :config (edit-server-start))

(quse-package erlang)

(quse-package rust-mode)

(quse-package cargo :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

(quse-package toml-mode)

(quse-package mediawiki)

(quse-package (xelb :fetcher github :repo "ch11ng/xelb"))
(quse-package (exwm :fetcher github :repo "ch11ng/exwm")
              :config
              (require 'exwm-config)
              (exwm-config-default))

(quse-package erc-twitch
              :config
              (erc-twitch-enable))

(use-package zone
  :init
  (defun lock-screen () ; TODO: pick a random function, show it, zone on that
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
