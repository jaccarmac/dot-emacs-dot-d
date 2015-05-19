(package-initialize)
(setq quelpa-update-melpa-p nil)
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(quelpa 'use-package)
(require 'use-package)

(quelpa '(quse-package :fetcher github :repo "jaccarmac/quse-package"))
(require 'quse-package)

(quse-package better-defaults)

(setq-default fill-column 79)

(setq-default inhibit-splash-screen t)

(quse-package undo-tree
              :init (global-undo-tree-mode))

(quse-package powerline
              :init (powerline-default-theme))

(quse-package moe-theme
              :config (load-theme 'moe-dark t))

(quse-package smartparens
              :init (smartparens-global-mode))

(quse-package (ledger-mode :fetcher github
                           :repo "jaccarmac/ledger"
                           :branch "next"
                           :files ("lisp/*.el"))
              :init (add-to-list 'auto-mode-alist
                                 '("ledger/.*\\.dat\\'" . ledger-mode)))

(quse-package password-store)

(quse-package projectile
              :init (projectile-global-mode))

(quse-package magit
              :init (setq magit-last-seen-setup-instructions "1.4.0"))

(quse-package auto-complete
              :init (progn (require 'auto-complete-config)
                           (ac-config-default)))

(quse-package cider
              :init (progn
                      (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
                      (setq cider-repl-tab-command 'indent-for-tab-command)))

(quse-package ac-cider
              :init (progn (add-hook 'cider-mode-hook 'ac-cider-setup)
                           (add-hook 'cider-repl-mode-hook 'ac-cider-setup)))

(quse-package slime
              :init (progn
                      (setq inferior-lisp-program "sbcl")
                      (setq common-lisp-hyperspec-root
                            (getenv "HYPERSPEC_ROOT"))
                      (setq slime-contribs '(slime-fancy))
                      (slime-setup)))

(quse-package ac-slime
              :init (progn (add-hook 'slime-mode-hook 'set-up-slime-ac)
                           (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
                           (eval-after-load "auto-complete"
                             '(add-to-list 'ac-modes 'slime-repl-mode))))

(quse-package (web-mode :fetcher github :repo "jaccarmac/web-mode")
              :init (progn
                      (setq web-mode-enable-engine-detection t)
                      (add-to-list 'auto-mode-alist
                                   '("\\.html?\\'" . web-mode))
                      (add-to-list 'auto-mode-alist
                                   '("\\.css?\\'" . web-mode))
                      (add-to-list 'auto-mode-alist
                                   '("\\.js?\\'" . web-mode))
                      (add-to-list 'auto-mode-alist
                                   '("\\.php?\\'" . web-mode))))

(quelpa 'emacs-eclim)
(use-package eclim :config (global-eclim-mode))
(use-package ac-emacs-eclim-source :config (ac-emacs-eclim-config))

(quse-package yasnippet)

(quse-package nim-mode)

(quse-package ac-nim :init (eval-after-load 'nim-mode
                             '(add-hook 'nim-mode-hook 'ac-nim-enable)))

(quse-package markdown-mode)

(quse-package go-mode)

(quse-package go-autocomplete)

(quse-package (protobuf-mode :fetcher github
                             :repo "google/protobuf"
                             :files ("editors/protobuf-mode.el")))

(quse-package yaml-mode)
