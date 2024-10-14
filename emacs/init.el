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
  ;; these settings make rust-analyzer start more quickly
  (setq gc-cons-threshold 100000000) ;; 100 MB
  (setq read-process-output-max (* 1024 1024)) ;; 1 MB


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

  ;; These are overridden in mode hooks.
  (setq %next-error-fn 'next-error)
  (setq %previous-error-fn 'previous-error)
  (setq %format-fn nil)
  (setq %show-type-fn nil)
  (setq %compile-build-command "")
  (setq %compile-run-command "")

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
 '(custom-safe-themes
   '("e8dcdd65c2d96149266551a7e95b1b0464cd7e040b224dce0e6ed20b687b20e6" "39c7acb711ece186120ca003784b61ab5ed8ec8a77add8a88c1bb2dd2a607b56" "8f4fe6cf334ef27df9b880887ef44660dd698a85b6a6b36ee8c303cbe08a6c41" "bebe94f725efd9d4d14df9a0311ca248364e5f6478d42536849266aa8e86eaf9" "21a6852be4f517124965f3c3b0d3b8e5aa3a4a1260c00e073037ed93856b1caa" "ba580cb6658791106b7858d8bdf619a40e902ab57377aa54764ad071d66ba66c" "e8e8e7c8f8c3ce30089f7707e7594e6e9e192ed2d101c0613838e549e5cefa3a" "a284fc6af49adeb2dd6461371c89ef7c0eff31ac949e9dcb87c6a2403db07492" "615c779983cdc65e4db49383bdc242f4bb95268431c61a8ff74674a646c15afd" "f8539af00a7f69ed96fd86d117f354f7f9635aacb50aba8e5be717c5bbb2e62a" "f04cbd91f984721a1b59ebbd13f6faf07d317c45dd75087b4837a877cd847e04" "7192c78c89afb75cfa0c25ac4c2109da5be84f99415f18e39f0f850e42283aec" "65c9e389ad8cfbd4b70c3ad57cdadcb507b96c7bcf37e0b8957f6ba8b2e0b1d4" "0e1baf1dc2fc9c87326f9b5ed3539367ce4d47f87f332a3fc29979a80560996f" "7dd1db76c7bfc3d669158800522194ed867c9b94846363b34437992e5ae5efcb" "98d114493a906df6f38a05f06c5a28e40a5f5b0a1c24f7d843f56a6f884febd9" "765d26d8b136577c9375721caa09b496931d84596a4ba37b5b668692a9f3c37e" "b2145ef58e115517ffb25b07665f501505114d68787a5dbb74bf4b134bad177f" "32f7a97d10ddf0214e2596e9da0cedeef98e7cc8af6fbba539fcaa4f62f28ebe" "ba2dc7d097616c79db2eb485e2af44199f2b5ae1cf59aea17d3cc7cc61e76e78" "53be5807b3bfac7a173d5fd73cb81ef5d570a1c2aceca877d90e2f08047dbc60" "8219cb76ffc09014884cd67d824c8afa0c41fe5650c58c3d92422daf38d200f6" "bd77d830839d7557958dc2f80d84aa79bdfe22f8459dec27367e6996b1a052c1" "4433a989e835b26328bffa81b93786b86586c09ebe78e2d26ec10ef3fe5a04a1" "3c8ac7e678ddd7adef7405948ad52374c15dec58354d8cb0f971396233ff558b" "e3f705e9330b916d2ca94c9abe5f0e1fe122a25848d60563cf69acad23b5adb3" "f8a0d91fd9fbc7b8021d533ddaf92058d989b890ea7b2759294676615331dc4a" "aaed5280cff090f56aaf6098693a46733278d9d7c6eb46bd2919793be5318ceb" "878f902f0457da885737e2663c8cdd4cb1ada98b0eed56235aa85c813c6baa65" "155fadfce4ad46aad8467bfcf3a3b38ed3d490d9e60d2cc59c1a19067ce5609b" "47e59bab86fc470edf9bdf09a60dc0e87fe4cea45c629d904d48329bd657f40d" "2acc6d259e4d974bedd7e53e1b7ee734c05dc4fa539fda3381358daa9f083400" "b20dce081d826678b26e12b7375dc9afaaf769c50c0affbc73d915962780eb3d" "4ddabbbb167341bd4ba5780ede91860f6911524fadae6472d8534d699ebe885c" "62f9dd388fe59016d06efa70109b3143953198fb6460c1f5f788fea27e0f67c5" "92c7241df73cd677b89d2e57cb6c72044881432e55b3be043e528a07381a29cc" "614dd13d7c96e07961dc015ba1b12a2533a7d79e88bc7c75001cd3d04e904819" "f0910ac264679fc0fb3f1298143d5ac5d5a77efb4d5216c1b279ce0d83e4ee50" "7b0c979481d3680b8e8784ed4b6dc470671b082cdd832406d0649fa499290965" "337a3cd354865336d7442717f77e0f2fc892beee2bf29f5a20051e870220b3d3" "7cc218c2f0abffa2dfa956757d007c93c4232b88ef8bce7a3c4fb70890425ef8" "ed67f4d25956fed615a67df186e54a5279c17c4e824ac5b8c930cd6b9f47e27b" "7945512d2f963e65310f2e94278ccd893ada2012fd634a32757e0c0812aeb907" "592149c7e25ff449b35ac32e2e2d283260a1989afa24c4cbfd95221a4f2a004d" "c0318e7342bf66f2cd5df5e2cb48d2183e32ae5753f4add4c0b5d3b6a3b9cd30" "766141594641b611fbd15e2a037e37fdd742cc245d3318e50fae956676a78062" "f2d2b99702439f152fb7ad7cce3895ff1d741b5361fe16c9ec65cecfde2745e9" "8bb2522f1eda1db0cbbec2e033b2be7c0c2312bcd1d434778cd7269778360c19" "61325e6577dd795fc0b2427882cd9d474f4c74df013699357bc66990b20ae693" default))
 '(package-selected-packages
   '(scroll-on-jump cargo-mode clang-format company enh-ruby-mode glsl-mode helm-company helm-projectile lsp-mode lua-mode nix-mode rubocop rust-mode typescript-mode web-mode whole-line-or-region zig-mode))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
