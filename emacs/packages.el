;;---- built-in ----;;
(use-package paren :config (show-paren-mode 1))
(use-package uniquify :config (setq uniquify-buffer-name-style 'forward))
(use-package winner :config (winner-mode 1))
(use-package autorevert :config (global-auto-revert-mode))

(use-package xt-mouse
  :config
  (xterm-mouse-mode 1)
  (global-set-key [mouse-1] (lambda () (interactive) ))
  (global-set-key [down-mouse-1] (lambda () (interactive) ))
  (global-set-key [mouse-3] 'mouse-set-point)
  (global-set-key [down-mouse-3] 'mouse-drag-region)
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 3)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 3)))
  )

(use-package isearch
  :diminish
  :bind
  (:map isearch-mode-map
        ("M-e" . 'isearch-repeat-backward)
        ("M-n" . 'isearch-repeat-forward)
        ("M-y" . 'isearch-yank-word-or-char)
        ("M-o" . 'isearch-yank-word-or-char)
        ("M-s" . 'helm-swoop-from-isearch)
        ("M-a" . 'helm-multi-swoop-all-from-isearch)
        ))

(use-package elec-pair
  :diminish
  :config
  (electric-pair-mode 1)
  ;; This disables some of the "smarter" electric pair behavior, which was breaking in ruby buffers based on their
  ;; content (after regexes).
  (setq electric-pair-preserve-balance nil)
  )

(use-package whitespace
  :diminish (global-whitespace-mode . "")
  ;; http://ergoemacs.org/emacs/whitespace-mode.html
  :config
  (setq whitespace-style (quote (face trailing newline tab-mark newline-mark)))

  (setq whitespace-display-mappings
        '((newline-mark 10 [182 10]) ; LINE FEED,
          (tab-mark 9 [8677 9] [92 9]) ; tab
          ))
  (global-whitespace-mode)
  )

(use-package compile
  :config
  (require 'ansi-color)
  (defun gpt%colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'gpt%colorize-compilation-buffer)
  )

;;---- external ----;;
(use-package whole-line-or-region :diminish)
(use-package expand-region :config (setq expand-region-fast-keys-enabled nil))

(use-package projectile
  :diminish
  :config
  (projectile-global-mode)
  ;; This is needed for projectile-globally-ignored-directories to have an effect.
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-globally-ignored-directories
        '("node_modules"
          "app/assets"
          "tmp"
          "vendor"
          "emacs/elpa"
          "vendor/raylib"
          "zig-out/"
          "zig-cache/"
          "zsh/zsh-autosuggestions"
          "zsh/zsh-syntax-highlighting"))
  (setq projectile-globally-ignored-file-suffixes
        '(".png"
          ".gif"
          ".pdf"
          ".opam.locked"))
  )

(use-package helm
  :diminish
  :config
  (helm-mode 1))

(use-package helm-projectile
  :after (helm projectile)
  :bind
  (:map helm-projectile-find-file-map
        ("M-e" . 'helm-previous-line)
        )

  (:map helm-buffer-map
        ("M-e" . 'helm-previous-line)
        ("M-n" . 'helm-next-line)
        ("M-E" . 'previous-history-element)
        ("M-N" . 'next-history-element)
        ("M-g" . 'helm-keyboard-quit)

        ;; control-tab
        ("M-+" . 'helm-execute-persistent-action)
        )

  (:map helm-map
        ("M-e" . 'helm-previous-line)
        ("M-n" . 'helm-next-line)
        ("M-E" . 'previous-history-element)
        ("M-N" . 'next-history-element)
        ("M-g" . 'helm-keyboard-quit)

        ;; control-tab
        ("M-+" . 'helm-execute-persistent-action)
        )

  :config
  (helm-projectile-on)

  ;; make helm always open in split window at the bottom
  (setq helm-split-window-in-side-p t)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4)))
  )

(use-package helm-swoop
  :bind
  (:map helm-swoop-map
        ("M-o" . 'helm-swoop-yank-thing-at-point)
        )

  :config
  (setq helm-swoop-split-direction 'split-window-horizontally)
  (setq helm-move-to-line-cycle-in-source nil)
  )

(use-package undo-tree
  :diminish
  :bind
  (:map undo-tree-visualizer-mode-map
        ("e" . 'undo-tree-visualize-undo)
        ("n" . 'undo-tree-visualize-redo)
        ("h" . 'undo-tree-visualize-switch-branch-left)
        ("i" . 'undo-tree-visualize-switch-branch-right)
        ("q" . 'undo-tree-visualizer-abort)
        ("C-g" . 'undo-tree-visualizer-abort)
        ("C-m" . 'undo-tree-visualizer-quit)
        )
  :config
  (setq undo-tree-auto-save-history nil)
  )

(use-package helm-company
  :diminish)

(use-package company-mode
  :diminish
  :hook (prog-mode . company-mode)

  :init
  (setq company-idle-delay nil)
  )


;;---- deferred ----;;
(use-package abbrev :defer t :diminish)

(use-package cc-mode :defer t
  :config
  (setq-default c-basic-offset 2)
  (add-hook 'c-mode-hook '%start-lsp)
  (add-hook 'cc-mode-hook '%start-lsp)
  (add-hook 'c-mode-hook (lambda ()
                           (setq comment-start "//")
                           (setq comment-end "")
                           ))

  (setq %compile-build-command "make -k")
  (setq %compile-run-command "make -k run")
  (setq %format-fn 'clang-format-buffer)

  :bind
  (:map c++-mode-map
        ;; Ignore the c-specific override that this mode sets.
        ("TAB" . 'indent-for-tab-command)
        ))

(use-package glsl-mode :defer t
  :diminish
  :init
  ;; (modify-syntax-entry ?_ "_")
  (add-hook 'glsl-mode-hook
            (lambda ()
              (modify-syntax-entry ?_ "_")
              ))
  )

(use-package css-mode :defer t
  :config
  (setq-default scss-compile-at-save nil))

(use-package typescript-mode :defer t
  :config
  (setq typescript-indent-level 2)
  )

(use-package lua-mode :defer t
  :config
  (setq lua-indent-level 2)
  )

(use-package web-mode :defer t
  :bind
  (:map my-keys-minor-mode-map
        ("M-SPC M-h" . '%insert-html)
        ("M-SPC M-r" . '%insert-erb)
        ("M-SPC M-e" . '%insert-erb-equals)
        )

  :config
  (setq web-mode-markup-indent-offset 2) ;; html indent
  (setq web-mode-code-indent-offset 2) ;; js indent
  (setq js-indent-level 2) ;; js indent

  (setq-default web-mode-comment-formats (remove '("javascript" . "/*") web-mode-comment-formats))
  (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
  (add-to-list 'web-mode-comment-formats '("jsx" . "//"))

  (defun %insert-html (tag-input)
    (interactive "stag: ")
    (let ((tag (if (string= tag-input "")
                   "div"
                 tag-input)))
      (if (region-active-p)
          (progn
            (%adjust-selection-for-edit)
            (if (> (point) (mark))
                (%flip-selection))
            (insert (concat "<" tag ">"))
            (%flip-selection)
            (insert (concat "</" tag ">"))
            )
        (insert (concat "<" tag ">" "</" tag ">")))
      (%left (+ 3 (length tag)))
      (%add)
      ))

  (defun %insert-erb ()
    (interactive)
    (insert "<%  %>")
    (%left 3)
    (%add))

  (defun %insert-erb-equals ()
    (interactive)
    (insert "<%=  %>")
    (%left 3)
    (%add))
  )

(use-package enh-ruby-mode :defer t
  :init
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist
               '("\\(?:Brewfile\\|Vagrantfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . enh-ruby-mode))
  (remove-hook 'enh-ruby-mode-hook 'erm-define-faces)

  :config
  (setq enh-ruby-deep-indent-paren nil)
  (setq enh-ruby-deep-indent-construct nil)

  (setq enh-ruby-extra-keywords
        '("include_examples" "shared_examples"
          "include_context" "shared_context"
          "describe" "context" "it" "specify"
          "xdescribe" "xcontext" "xit" "xspecify"
          "fdescribe" "fcontext" "fit" "fspecify"
          "before" "after"
          "let" "let!" "let_it_be"
          ))

  (setq %format-fn 'rubocop-autocorrect-current-file)
  )

(use-package rubocop :defer t
  :after enh-ruby-mode
  :config
  (setq rubocop-use-server t)
  )

(use-package lsp-mode :defer t
  :diminish
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-diagnostics-provider :none)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  ;; (setq lsp-completion-provider :none)
  ;; (setq lsp-completion-show-detail nil)
  ;; (setq lsp-completion-show-kind nil)
  (setq lsp-lens-enable nil)
  (setq lsp-enable-symbol-highlighting nil)
  )

(use-package eldoc :defer t
  :diminish)

(use-package rust-mode :defer t
  :config
  (setq rust-format-on-save nil)
  (setq rust-indent-offset 2)
  (add-hook 'rust-mode-hook '%start-lsp)

  (setq %compile-build-command "cargo clippy")
  (setq %compile-run-command "cargo run")
  (setq %compile-clippy-command "cargo clippy --fix --allow-dirty")
  (setq %compile-tests-command "cargo test -- --show-output")
  (setq %format-fn 'rust-format-buffer)
  )

(use-package zig-mode :defer t
  :config
  (setq zig-format-on-save nil)
  (setq lsp-zig-zls-executable "~/.local/zls/zig-out/bin/zls")
  (add-hook 'zig-mode-hook '%start-lsp)

  (setq %compile-build-command "zig build")
  (setq %compile-run-command "zig run")
  (setq %compile-clippy-command "zig build")
  (setq %format-fn 'zig-format-buffer)
  (setq %compile-tests-command "zig build test")
  )

(use-package tuareg :defer t
  :config
  ;; merlin setup w/opam
  (let ((opam-share "/home/ben/.opam/default/share"))
    (when (and opam-share (file-directory-p opam-share))
      ;; Register Merlin
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (require 'ocp-indent)
      (autoload 'merlin-mode "merlin" nil t nil)
      ;; Automatically start it in OCaml buffers
      (add-hook 'tuareg-mode-hook 'merlin-mode t)
      (add-hook 'caml-mode-hook 'merlin-mode t)

      (setq compilation-error-regexp-alist-alist
            (cons '(my-ocaml-error
                    ;; regexp to match the error messages
                    "^Called from \\(.*\\) in file \"\\([^\"]+\\)\", line \\([0-9]+\\),"
                    ;; file group
                    2
                    ;; line group
                    3
                    ;; column group (nil means no specific column)
                    nil
                    ;; severity level
                    2)
                  compilation-error-regexp-alist-alist))

      (add-to-list 'compilation-error-regexp-alist 'my-ocaml-error)

      (add-to-list 'auto-mode-alist
                   '("\\.\\(?:atd\\)\\'" . tuareg-mode))

      ;; Use opam switch to lookup ocamlmerlin binary
      (setq merlin-command 'opam)

      (setq merlin-error-on-single-line t)
      ))

  (defun %dune-promote ()
    (interactive)
    (let ((exit-code (shell-command "dune promote")))
      (if (= 0 exit-code)
          (progn
            (message "promoted")
            (revert-buffer nil t)
            ))))

  (defun %dune-runtest ()
    (interactive)
    (let ((exit-code (shell-command "dune runtest")))
      (if (= 0 exit-code)
          (message "%s" (propertize "tests passed" 'face 'success)))))

  (defun %merlin-locate-in-new-window ()
    (interactive)
    (let ((current-prefix-arg '(16))) ;; emulate C-u
      (call-interactively #'merlin-locate)))

  (require 'ocamlformat)
  (defun ocamlformat--enable-indent () nil)

  (setq compilation-auto-jump-to-first-error t)
  (setq compilation-scroll-output t)
  (setq compilation-always-kill t)

  (setq %next-error-fn 'merlin-error-next)
  (setq %previous-error-fn 'merlin-error-prev)
  (setq %format-fn 'ocamlformat)
  (setq %show-type-fn 'merlin-type-enclosing)

  ;; menhir/ocamllex
  (add-to-list 'auto-mode-alist '("\\.mll\\'" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("\\.mly\\'" . tuareg-mode))
  )

(use-package magit :defer t
  :bind
  (:map magit-mode-map
        ("e" . 'magit-section-backward)
        ("n" . 'magit-section-forward)
        ("Q" . (lambda () (interactive)
                 (call-interactively 'magit-mode-bury-buffer)
                 (%restore-layout)
                 ))
        ("M-RET" . 'magit-diff-visit-worktree-file)
        )

  :config
  ;; This removes the "remote branches" and "tags" from magit-status page.
  (setq magit-refs-sections-hook
        '(magit-insert-error-header
          magit-insert-branch-description
          magit-insert-local-branches
          ;; magit-insert-remote-branches
          ;; magit-insert-tags
          ))

  (setq magit-revision-insert-related-refs nil)

  (magit-auto-revert-mode)

  (transient-append-suffix 'magit-branch "b"
    '("m" "main" (lambda () (interactive) (magit-run-git-async "checkout" "main"))))

  (transient-append-suffix 'magit-branch "b"
    '("l" "last branch" (lambda () (interactive) (magit-run-git-async "checkout" "-"))))

  (transient-append-suffix 'magit-rebase "e"
    '("m" "main" (lambda () (interactive) (magit-run-git-async "rebase" "main"))))

  (transient-append-suffix 'magit-commit "c"
    '("t" "update timestamp" (lambda () (interactive)
                               (magit-run-git-async "commit" "--amend" "--no-edit" "--date" (format-time-string "%Y-%m-%d")))))

  (transient-append-suffix 'magit-commit "c"
    '("n" "no hooks" (lambda () (interactive) (magit-run-git-async "commit" "--no-verify"))))
  )

(use-package org :defer t
  :diminish ('org-indent-mode . "")

  :bind
  (:map modalka-mode-map
        ("m" . 'outline-previous-visible-heading)
        ("." . 'outline-next-visible-heading)
        ("j" . '%org-beginning-of-line)

        :map org-mode-map
        ("C-M-h" . 'org-backward-heading-same-level)
        ("C-M-n" . 'outline-next-visible-heading)
        ("C-M-e" . 'outline-previous-visible-heading)
        ("C-M-i" . 'org-forward-heading-same-level)
        ("C-M-u" . 'outline-up-heading)

        ("C-M-l" . '%org-promote)
        ("C-M-y" . '%org-demote)

        ("C-M-o" . '%org-insert-heading)

        ("C-M-x" . '%org-clear-todo)
        ("C-M-t" . '%org-todo)
        ("C-M-d" . '%org-done)
        ("C-M-s" . '%org-skip)

        ("C-M-c" . '%org-checklist)

        ("M-SPC M-s" . '%org-insert-src-block)
        ("M-SPC /" . '%org-update-all-statistics)
        )

  :hook
  (org-mode . visual-line-mode)
  (org-mode . (lambda ()
                (progn
                  ;; Not sure why these don't work in :bind, even when org is loaded after modalka.
                  (bind-key "C-M-n" 'outline-next-visible-heading my-keys-minor-mode-map)
                  (bind-key "C-M-e" 'outline-previous-visible-heading my-keys-minor-mode-map)
                  )))

  :config
  (setq org-todo-keywords
        '((sequence
           "[_]" ;; todo
           "|"
           "[X]" ;; done
           "[S]" ;; skipped
           )))

  (setq org-startup-indented t)               ;; indent tasks and only show one star
  (setq org-startup-folded 'show2levels)      ;; show top 2 levels of headings at startup
  (setq org-catch-invisible-edits t)          ;; don't allow edits to collapsed parts of a buffer
  (setq org-enforce-todo-dependencies t)      ;; can't finish a task with unfinished children
  (setq org-blank-before-new-entry nil)       ;; no blank line between heading for org-metareturn
  (setq org-hierarchical-todo-statistics nil) ;; count all children for statistics cookies

  (setq org-ellipsis " [...]")
  (setq org-cycle-level-after-item/entry-creation nil)

  (defun %org-update-all-statistics ()
    (interactive)
    (let ((current-prefix-arg 4)) ;; emulate C-u
      (call-interactively 'org-update-statistics-cookies)))

  (defun %org-insert-datetime ()
    (interactive)
    (setq current-prefix-arg '(16))      ; C-u C-u
    (call-interactively 'org-time-stamp))

  (defun %org-insert-date ()
    (interactive)
    (%org-insert-datetime)     ;; <2019-03-17 Sun 13:42>|
    (backward-char)              ;; <2019-03-17 Sun 13:42|>
    (org-delete-backward-char 6) ;; <2019-03-17 Sun|>
    (forward-char))               ;; <2019-03-17 Sun>|

  (defun %org-beginning-of-line ()
    (interactive)
    (beginning-of-line)
    (if (org-at-heading-p)
        (search-forward " ")))

  (defun %org-insert-heading ()
    (interactive)
    (%eol)
    (org-insert-heading-after-current)
    (%add))

  (defmacro %defun-todo-fn (name keyword)
    `(defun ,name ()
       (interactive)
       (org-todo ,keyword)))
  (put '%defun-todo-fn 'lisp-indent-function 'defun)

  (%defun-todo-fn %org-clear-todo "")
  (%defun-todo-fn %org-todo "[_]")
  (%defun-todo-fn %org-done "[X]")
  (%defun-todo-fn %org-skip "[S]")

  (defun %org-checklist ()
    (interactive)
    (org-todo "")
    (%org-beginning-of-line)
    (insert "[/] ")
    )

  (defun %org-insert-src-block ()
    (interactive)
    (org-insert-structure-template "src"))

  (defun %org-promote ()
    (interactive)
    (if (org-at-heading-p)
        (call-interactively 'org-promote-subtree)
      (call-interactively 'org-outdent-item-tree)))

  (defun %org-demote ()
    (interactive)
    (if (org-at-heading-p)
        (call-interactively 'org-demote-subtree)
      (call-interactively 'org-indent-item-tree)))
  )
