(progn
  (require 'package)
  ;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  (add-to-list 'custom-theme-load-path "~/env/emacs/themes/")
  (load-theme 'otter t)

   ;;; disable menu/toolbar/scrollbar
  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

 ;;; define minor mode for my keys
  (defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

  (define-minor-mode my-keys-minor-mode
    "minor mode with all my keys"
    t "" 'my-keys-minor-mode-map)
  (my-keys-minor-mode 1)
  (defadvice load (after give-my-keybindings-priority)
    "Try to ensure that my keybindings always have priority."
    (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
        (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
          (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
          (add-to-list 'minor-mode-map-alist mykeys))))
  (ad-activate 'load)

 ;;; base settings
  (setq-default auto-save-default nil)   ;; no autosave
  (setq make-backup-files nil)           ;; no autosave
  (setq create-lockfiles nil)            ;; no lock files (starting with .#)
  (setq-default indent-tabs-mode nil)    ;; use spaces instead of tabs
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq-default auto-compression-mode 0) ;; don't try to decompress files
  (setq-default fill-column 120)         ;; text width for fill-paragraph
  (set-default 'truncate-lines t)        ;; disable word wrap
  (setq echo-keystrokes 0.001)           ;; like vim's showcmd
  (setq vc-follow-symlinks nil)
  (setq require-final-newline nil)
  (setq-default show-trailing-whitespace t)
  (setq set-mark-command-repeat-pop t)
  (setq scroll-conservatively 101)       ;; don't recenter the cursor if it moves offscreen
  (setq scroll-margin 2)                 ;; start scrolling 2 lines away from top/bottom of screen
  (setq visible-cursor nil)              ;; disable cursor blinking
  (setq next-error-highlight 1.0)        ;; delay before highlighting errors

  (add-hook 'compilation-mode-hook (lambda ()
                                     (setq truncate-lines nil)
                                     (setq word-wrap t)))

  (setq-default mode-line-format (list
                                  "      "
                                  '(:eval (when buffer-read-only
                                            (propertize "RO " 'face 'error)))
                                  '(:eval (if (buffer-modified-p)
                                              "%b* "
                                            "%b  "))
                                  'mode-line-modes
                                  ))

  (defun add-minor-modes ()
    (highlight-numbers-mode)
    (display-line-numbers-mode)
    (setq display-line-numbers-width 3)
    (undo-tree-mode)
    ;; Not sure why this is needed, since it should already happen at the end of init.el.
    (modalka-mode)
    )

  (add-hook 'conf-mode-hook 'add-minor-modes)
  (add-hook 'yaml-mode-hook 'add-minor-modes)
  (add-hook 'org-mode-hook 'undo-tree-mode)
  (add-hook 'prog-mode-hook 'add-minor-modes)

  ;; startup settings
  (setq initial-major-mode 'text-mode)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-buffer-menu t)
  (setq initial-scratch-message (concat "# " (replace-regexp-in-string " (.*\n.*" "" (emacs-version)) "\n\n"))

  ;; file types
  (add-to-list 'auto-mode-alist '("\\.yml\\.example$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.clang-format$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.fs$" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vs$" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
  (add-to-list 'auto-mode-alist '("zshrc$" . shell-script-mode))
  (add-to-list 'auto-mode-alist '("\\.gitignore$" . conf-mode))

  ;; unset keys
  (global-unset-key (kbd "C-x h"))
  (global-unset-key (kbd "C-x n"))
  (global-unset-key (kbd "C-x e"))
  (global-unset-key (kbd "C-x i"))
  (global-unset-key (kbd "C-x C-h"))
  (global-unset-key (kbd "C-x C-n"))
  (global-unset-key (kbd "C-x C-e"))
  (global-unset-key (kbd "C-x C-i"))
  (global-unset-key (kbd "M-`"))
  (defun eshell () (interactive) ())

  (load "~/.emacs.d/packages.el")
  (load "~/.emacs.d/private.el")
  (load "~/.emacs.d/keys.el")

  (selected-global-mode)
  (modalka-mode)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rubocop nix-mode clang-format cargo-mode rust-mode typescript-mode lsp-mode glsl-mode zig-mode lua-mode enh-ruby-mode yasnippet terraform-mode whole-line-or-region web-mode org helm-projectile))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
