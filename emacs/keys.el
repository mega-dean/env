;; -*- lexical-binding: t; -*-

(use-package selected
  :ensure t
  :bind
  (:map selected-keymap
        ("w" . %wipe-selection)
        ("b" . %become-selection)
        ("t" . %trade-selection)
        ("c" . %copy-selection)
        ("d" . %dupe-selection)
        ("v" . %vanish-selection)
        )

  :config
  (%define-prefix-action "w" '%wipe)
  (%define-prefix-action "b" '%become)
  (%define-prefix-action "t" '%trade)
  (%define-prefix-action "c" '%copy)
  (%define-prefix-action "d" '%dupe)
  (%define-prefix-action "V" '%vanish)
  )

(use-package modalka
  :init
  (defun %noop () (interactive) )
  (defalias '%m-x-menu 'helm-M-x)

  (defun %log (&rest msgs)
    (message (apply 'concat msgs)))

  (defun %git ()
    (interactive)
    (%save-layout)
    (magit-status)
    (delete-other-windows)
    (magit-process-buffer)
    (other-window 1))

  (setq %copy-to-clipboard "xclip -selection clipboard && xsel --clipboard --output | xsel --clipboard --input")
  (defun %xclip ()
    (interactive)
    (save-excursion
      (%adjust-selection-for-edit)
      (shell-command-on-region (region-beginning) (region-end) %copy-to-clipboard)
      (deactivate-mark)))

 ;;; ---- modes ---- ;;
  (defun %add () (interactive) (modalka-mode -1))
  (defun %normal () (interactive) (modalka-mode) (%deselect) (%save))
  (defun %normal-without-save () (interactive) (modalka-mode))

  (defun %save ()
    (interactive)
    (let ((editable (and (not buffer-read-only)
                         (buffer-file-name))))
      (if editable
          (save-buffer)
        (%log "not saving..."))))

  (defun %full-save ()
    (interactive)
    (%trim-whitespace)
    (%save)
    (%normal))

 ;;; ---- windows ---- ;;
  (defalias '%recenter 'recenter-top-bottom)
  (defalias '%maximize 'delete-other-windows)
  (defalias '%quit-window 'delete-window)
  (defalias '%window-undo 'winner-undo)
  (defalias '%window-redo 'winner-redo)

  (defalias '%switch-window-left 'windmove-left)
  (defalias '%switch-window-down 'windmove-down)
  (defalias '%switch-window-up 'windmove-up)
  (defalias '%switch-window-right 'windmove-right)

  (defalias '%split-left 'split-window-right)
  (defalias '%split-up 'split-window-below)
  (defun %split-right () (interactive) (%split-left) (%switch-window-right))
  (defun %split-down () (interactive) (%split-up) (%switch-window-down))

  (defun %save-layout ()
    (interactive)
    (window-configuration-to-register ?l))

  (defun %restore-layout ()
    (interactive)
    (jump-to-register ?l))

 ;;; ---- buffers ---- ;;
  (defalias '%open-file 'helm-projectile-find-file)
  (defalias '%buffer-list 'ibuffer)
  (defalias '%go-to-buffer 'switch-to-buffer)
  (defalias '%find-open-buffer 'helm-projectile-switch-to-buffer)
  (defalias '%dired 'dired-jump)

  (defun %go-to-compilation-log ()
    (interactive)
    (%go-to-buffer "*compilation*"))

  (defun %go-to-scratch ()
    (interactive)
    (%go-to-buffer "*scratch*"))

  (defun %checkout-buffer ()
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

  (defun %refresh-syntax-highlighting ()
    (interactive)
    (funcall major-mode))

  (defun %show-filepaths ()
    (interactive)
    (if (projectile-project-root)
        (let ((buffer-path-from-root
               (replace-regexp-in-string (regexp-quote (projectile-project-root)) "" (buffer-file-name) nil 'literal)))
          (%log "rel: " (%filepath-from-root) "\nabs: " (buffer-file-name)))
      (%log (buffer-file-name))
      ))

  (defun %cp-filepath ()
    (interactive)
    (shell-command (concat "echo -n " (%filepath-from-root) " | " %copy-to-clipboard)))

  (defun %filepath-from-root ()
    (replace-regexp-in-string (regexp-quote (projectile-project-root)) "" (buffer-file-name) nil 'literal))

 ;;; ---- cursor movement ---- ;;
  (defalias '%left 'backward-char)
  (defalias '%down 'next-line)
  (defalias '%up 'previous-line)
  (defalias '%right 'forward-char)
  (defalias '%indent 'indent-for-tab-command)
  (defalias '%bw-bow 'backward-word)
  (defalias '%fw-bow 'forward-to-word)
  (defalias '%eow 'forward-word)
  (defalias '%bob 'beginning-of-buffer)
  (defalias '%eob 'end-of-buffer)
  (defalias '%bop 'backward-paragraph)
  (defalias '%eop 'forward-paragraph)
  (defalias '%bol-indent 'back-to-indentation)

  (defun %eol ()
    (interactive)
    (if visual-line-mode
        (end-of-visual-line)
      (end-of-line)))

  (defun %bol ()
    (interactive)
    (if visual-line-mode
        (beginning-of-visual-line)
      (beginning-of-line)))

  (defconst %whitespace "[[:space:]]\\|\n")
  (defconst %non-whitespace "[[:graph:]]")

  (defun %bw-boW ()
    (interactive)
    (if (string-match %whitespace (string (preceding-char)))
        (re-search-backward %non-whitespace))
    (re-search-backward %whitespace)
    (%right))

  (defun %fw-boW ()
    (interactive)
    (re-search-forward %whitespace)
    (if (string-match %whitespace (string (following-char)))
        (progn (re-search-forward %non-whitespace)
               (%left))))

  (defun %eoW ()
    (interactive)
    (if (string-match %whitespace (string (following-char)))
        (re-search-forward %non-whitespace))
    (re-search-forward %whitespace)
    (%left))

  (defun %past (target)
    (interactive "cpast: ")
    (search-forward (string target)))

  (defun %bw-past (target)
    (interactive "cbw past: ")
    (search-backward (string target)))

  (defun %until (target)
    (interactive "cuntil: ")
    (%past target)
    (%left))

  (defun %bw-until (target)
    (interactive "cback until: ")
    (%bw-past target)
    (%right))

  ;; Emacs uses "up"/"down" to refer to the direction that the text in the buffer moves,
  ;; % uses direction that the cursor moves.
  (defun %scroll-up-page ()
    (interactive)
    (let ((window-height (window-body-height))
          (start-line (line-number-at-pos)))
      (scroll-down-line window-height)
      (%up (- window-height (- start-line (line-number-at-pos))))))

  (defun %scroll-down-page ()
    (interactive)
    (let ((window-height (window-body-height))
          (start-line (line-number-at-pos)))
      (scroll-up-line window-height)
      (%down (- window-height (- (line-number-at-pos) start-line)))))

  (defun %line-number-command (s)
    (interactive "s:")
    (if (not (zerop (length s)))
        (progn
          (let ((last-char (substring s -1))
                (line-number (string-to-number (substring s 0 -1))))
            (cond ((string-equal last-char "w")
                   (save-excursion
                     (goto-line line-number)
                     (%wipe)))
                  ((string-equal last-char "c")
                   (save-excursion
                     (goto-line line-number)
                     (%copy)))
                  (t
                   (goto-line (string-to-number s))))))))

 ;;; ---- text editing ---- ;;
  (defalias '%bw-wipe-word 'backward-kill-word)
  (defalias '%wipe-word 'kill-word)
  (defalias '%start-macro 'kmacro-start-macro)
  (defalias '%end-macro 'kmacro-end-macro)
  (defalias '%run-macro 'kmacro-end-and-call-macro)
  (defalias '%trim-whitespace 'delete-trailing-whitespace)
  (defalias '%downcase 'downcase-word)
  (defalias '%uppercase 'upcase-word)
  (defalias '%capitalize 'capitalize-word)
  (defalias '%join-line-above 'delete-indentation)
  (defalias '%justify-text 'fill-paragraph)

  (defun %bs ()
    (interactive)
    (delete-char -1)
    (%add))

  (defun %m-bs ()
    (interactive)
    (call-interactively '%bw-wipe-word)
    (%add))

  (defun %del ()
    (interactive)
    (%vanish)
    (%add))

  (defun %m-del ()
    (interactive)
    (call-interactively '%wipe-word)
    (%add))

  (defun %copy ()
    (interactive)
    (whole-line-or-region-kill-ring-save 1))

  (defun %wipe ()
    (interactive)
    (whole-line-or-region-kill-region 1))

  (defun %vanish ()
    (interactive)
    (if (region-active-p)
        (whole-line-or-region-delete-region 1)
      (delete-char 1)))

  (defun %become ()
    (interactive)
    (%vanish)
    (%paste-fmt))

  (defun %graft (c)
    (interactive "cgraft: ")
    (%vanish)
    (insert c)
    (%left))

  (defun %dupe ()
    (interactive)
    (%copy)
    (%flip-selection)
    (%paste-fmt)
    (%flip-selection))

  (defun %paste-raw ()
    (interactive)
    (if current-prefix-arg (insert " "))
    (yank))

  (defun %paste-fmt ()
    (interactive)
    (%paste-raw)
    (call-interactively 'indent-region))

  (defun %wipe-line ()
    (interactive)
    (%select-target "ol")
    (%wipe))

  (defun %paste-eol ()
    (interactive)
    (%eol)
    (%paste-raw))

  (defun %vanish-eol ()
    (interactive)
    (%select-eol)
    (%vanish))

  (defun %dupe-eol ()
    (interactive)
    (%select-eol)
    (%dupe))

  (defun %wipe-eol ()
    (interactive)
    (when (not (eolp))
      (%select-eol)
      (%wipe)))

  (defun %become-eol ()
    (interactive)
    (%select-eol)
    (%become))

  (defmacro %defun-add (fn original)
    `(defun ,fn ()
       (interactive)
       (,original)
       (%add)))
  (put '%defun-add 'lisp-indent-function 'defun)

  (%defun-add %trade %wipe)
  (%defun-add %trade-eol %wipe-eol)
  (%defun-add %add-eol %eol)
  (%defun-add %add-bol-indent %bol-indent)

  (defun %newline-here ()
    (interactive)
    (newline-and-indent)
    (%add))

  (defun %join-line-below ()
    (interactive)
    (%down)
    (%join-line-above))

  (defun %cycle-spacing ()
    (interactive)
    (cycle-spacing 1 nil 'fast))

  (defun xah%toggle-previous-letter-case ()
    (interactive)
    (let ((case-fold-search nil))
      (cond
       ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
       ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point)))))
      ))

  (defun so%increment-next-number (&optional arg)
    "Increment the number forward from point by 'arg'."
    (interactive "p*")
    (%set-last-function 'so%increment-next-number)
    (save-excursion
      (save-match-data
        (let (inc-by field-width answer)
          (setq inc-by (if arg arg 1))
          (skip-chars-backward "0123456789")
          (when (re-search-forward "[0-9]+" nil t)
            (setq field-width (- (match-end 0) (match-beginning 0)))
            (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
            (when (< answer 0)
              (setq answer (+ (expt 10 field-width) answer)))
            (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                   answer)))))))

  (defun so%decrement-next-number (&optional arg)
    (interactive "p*")
    (so%increment-next-number (if arg (- arg) -1))
    (%set-last-function 'so%decrement-next-number)
    )

 ;;; ---- undo/redo ---- ;;
  (defalias '%undo 'undo-tree-undo)
  (defalias '%redo 'undo-tree-redo)
  (defalias '%undo-tree 'undo-tree-visualize)
  (defun %undo-tree-enable () (interactive) (undo-tree-mode))

 ;;; ---- rect ---- ;;
  (defalias '%rect-wipe 'kill-rectangle)
  (defalias '%rect-trade 'string-rectangle)
  (defalias '%rect-paste 'yank-rectangle)

  (defun %rect-add ()
    (interactive)
    (let ((pcol (current-column))
          (mcol (save-excursion
                  (%flip-selection)
                  (current-column))))
      (if (= pcol mcol)
          (call-interactively '%rect-trade)
        (%log "`|a` should only add text, use `|t` to replace text"))))

 ;;; ---- dragging ---- ;;
  (defun %drag-fw ()
    (interactive)
    (%right)
    (transpose-chars 1)
    (%left))

  (defun %drag-bw ()
    (interactive)
    (transpose-chars 1)
    (%left 2))

  (defun %drag-line-bw ()
    ;; neither save-excursion nor setting a marker was working here, probably because point moves
    (interactive)
    (let ((col (current-column)))
      (transpose-lines 1)
      (%up 2)
      (%right col)))

  (defun %drag-line-fw ()
    (interactive)
    (let ((col (current-column)))
      (%down)
      (transpose-lines 1)
      (%up)
      (%bol)
      (%right col)
      ))

 ;;; ---- comments ---- ;;
  (defun %comment ()
    (interactive)
    (whole-line-or-region-comment-dwim nil))

  (defun %comment-par ()
    (interactive)
    (if (region-active-p)
        (%comment)
      (progn
        (%select-target "ip")
        (%left)
        (%comment))))

  (defun %insert-comment (str)
    (interactive)
    (cond ((eq major-mode 'tuareg-mode)
           (progn
             (insert (concat "(* " str " *)"))
             (%left 3)
             (%add)
             ))
          (t (progn
               (%indent)
               (if (string-equal str "")
                   (insert "x")
                 (insert str))
               (%select)
               (%bw-bow)
               (%comment)
               (%eol)
               (if (string-equal str "")
                   (%bs)))))
    (%add)
    )

  (defun %insert-blank-comment ()
    (interactive)
    (%insert-comment ""))

  (defun %insert-todo-comment ()
    (interactive)
    (%insert-comment "TODO "))

  (defun %insert-fixme-comment ()
    (interactive)
    (%insert-comment "FIXME "))

  (defun %insert-polish-comment ()
    (interactive)
    (%insert-comment "POLISH "))

 ;;; ---- selecting text ---- ;;
  (defalias '%deselect 'deactivate-mark)

  (defun %select ()
    (interactive)
    (setq %selection-line nil)
    (set-marker %mark-anchor (point))
    (set-mark-command nil))

  (defun %select-line ()
    (interactive)
    (%bol)
    (%select)
    (setq %selection-line (line-number-at-pos))
    (%eol)
    (set-marker %mark-anchor-eol (+ 1 (point))))

  (defun %select-target (target)
    (interactive)
    (cond
     ((string= "iw" target)
      (%right)
      (%bw-bow)
      (%select)
      (%eow))

     ((string= "ow" target)
      (%select-target "iw")
      (%right))

     ((string= "oW" target)
      (re-search-backward %whitespace)
      (%right)
      (set-mark-command nil)
      (re-search-forward %whitespace))

     ((string= "iW" target)
      (%select-target "oW")
      (%left))

     ((string= "ip" target)
      (mark-paragraph)
      (%right)
      (%flip-selection))

     ((string= "op" target)
      (%select-target "ip")
      (%right))

     ((string= "il" target)
      (%bol)
      (%select)
      (%eol))

     ((string= "ol" target)
      (%select-target "il")
      (%right))))

  (defun %select-outside (char)
    (search-backward char)
    (er/expand-region 1))

  (defun %select-inside (char)
    (%select-outside char)
    (%right)
    (%flip-selection)
    (%left)
    (%flip-selection))

  (defun %select-eol ()
    (interactive)
    (%select)
    (%eol)
    (%flip-selection))

  (setq %mark-anchor (make-marker))
  (setq %mark-anchor-eol (make-marker))

  (defun %adjust-selection (anchor)
    (let* ((mark- (or (mark) 0))
           (diff (- mark- anchor)))
      (if (or (= 0 diff) (= 1 diff))
          (let ((new-mark (if (<= (point) anchor)
                              (1+ anchor)
                            anchor)))
            (set-mark new-mark)))))

  (defun %adjust-line-selection (anchor anchor-eol)
    (if (< (line-number-at-pos) %selection-line) ;; above selection-line
        (progn
          (set-mark anchor-eol)
          (%bol))
      (progn
        (set-mark anchor)
        (%eol))))

  (defun %adjust-selection-hook ()
    (if (region-active-p)
        (let ((anchor (marker-position %mark-anchor))
              (anchor-eol (marker-position %mark-anchor-eol)))
          (if anchor
              (if %selection-line
                  (%adjust-line-selection anchor anchor-eol)
                (%adjust-selection anchor))))))

  (defun %adjust-selection-for-edit ()
    (if (and (> (point) (mark))
             (not (eobp)))
        (%right)))

  (defalias '%flip-selection 'exchange-point-and-mark)
  (defun %flip-selection-and-adjust ()
    (interactive)
    (if (> (point) (mark))
        (%right))
    (exchange-point-and-mark)
    (if (> (point) (mark))
        (%left))
    )

  (defmacro %defun-selection-fn (name fn)
    `(defun ,name ()
       (interactive)
       (%adjust-selection-for-edit)
       (,fn)))
  (put '%defun-selection-fn 'lisp-indent-function 'defun)

  (%defun-selection-fn %vanish-selection %vanish)
  (%defun-selection-fn %dupe-selection %dupe)
  (%defun-selection-fn %trade-selection %trade)
  (%defun-selection-fn %wipe-selection %wipe)
  (%defun-selection-fn %become-selection %become)

  (defun %copy-selection ()
    (interactive)
    (save-excursion
      (%adjust-selection-for-edit)
      (%copy)))

 ;;; ---- searching ---- ;;
  (defalias '%search 'isearch-forward-regexp)

  (defun %search-project ()
    (interactive)
    (if (region-active-p)
        (%adjust-selection-for-edit))
    (setq deadgrep--search-type 'string)
    (call-interactively 'deadgrep))

  (defun %helm-multi-swoop ()
    (interactive)
    (if (region-active-p)
        (%adjust-selection-for-edit))
    (call-interactively 'helm-multi-swoop-current-mode)
    )

  (defun %regex-search-project ()
    (interactive)
    (setq deadgrep--search-type 'regexp)
    (call-interactively 'deadgrep))

  (defun %search-swoop ()
    (interactive)
    ;; (%mark-set ?M)
    (helm-swoop :$query ""))

 ;;; ---- search and replace ---- ;;
  (defalias '%exchange-regex 'replace-regexp)
  (defalias '%exchange 'replace-string)

  (defun %query-exchange-regex ()
    (interactive)
    (let ((scroll-conservatively 0))
      (call-interactively 'query-replace-regexp)))

  (defun %query-exchange ()
    (interactive)
    (let ((scroll-conservatively 0))
      (call-interactively 'query-replace)))

 ;;; ---- help ---- ;;
  (defalias '%help 'help-command)
  (defalias '%help-search 'describe-symbol)
  (defalias '%help-key 'describe-key)
  (defalias '%help-variable 'describe-variable)
  (defalias '%help-messages 'view-echo-area-messages)
  (defalias '%help-mode 'describe-mode)
  (defalias '%help-all-keys 'describe-bindings)
  (defalias '%help-syntax-highlighting 'describe-face)
  (defalias '%paste-from-history 'helm-show-kill-ring)

  (defalias '%eval-expr 'eval-expression)
  (defalias '%eval-inline 'eval-last-sexp)

  (defun gpt%show-cursor-position ()
    "Display the current column and row of the cursor."
    (interactive)
    (let ((position (format "row %d, column %d"
                            (line-number-at-pos)
                            (current-column))))
      (%log position)))

 ;;; ---- folding ---- ;;
  (defun %fold ()
    (interactive)
    (%bol-indent)
    (set-selective-display (1+ (current-column))))

  (defun %fold-max ()
    (interactive)
    (%bol)
    (set-selective-display (1+ (current-column))))

  (defun %unfold ()
    (interactive)
    (set-selective-display 0))

 ;;; ---- compilation ---- ;;
  (defun %compile (command)
    (interactive)
    (%save-layout)
    (%maximize)
    (compile command))

  (defun %next-error ()
    (interactive)
    (cond ((eq major-mode 'tuareg-mode)
           (merlin-error-next))
          (t
           (next-error))
          ))

  (defun %previous-error ()
    (interactive)
    (cond ((eq major-mode 'tuareg-mode)
           (merlin-error-prev))
          (t
           (previous-error))
          ))

  (defun %run-tests ()
    (interactive)
    (cond ((eq major-mode 'zig-mode)
           (%compile "zig build test"))
          ((eq major-mode 'rust-mode)
           (%compile "cargo test -- --show-output"))
          (t
           (%log "run-tests unknown mode: " (symbol-name major-mode)))))

  (defun %format-file ()
    (interactive)
    (cond ((eq major-mode 'tuareg-mode)
           (ocamlformat))
          ((eq major-mode 'zig-mode)
           (zig-format-buffer))
          ((eq major-mode 'rust-mode)
           (rust-format-buffer))
          ((or (eq major-mode 'c++-mode)
               (eq major-mode 'c-mode))
           (clang-format-buffer))
          ((eq major-mode 'enh-ruby-mode)
           (rubocop-autocorrect-current-file))
          (t
           (save-excursion
             (mark-whole-buffer)
             (call-interactively 'indent-region)
             ))
          )
    (%full-save))

  (defun %clippy ()
    (interactive)
    (cond ((eq major-mode 'rust-mode)
           (compile "cargo clippy"))
          ((eq major-mode 'enh-ruby-mode)
           (rubocop-check-project))
          (t
           (private%compile-build)))
    (%full-save))

  (defun %clippy-fix ()
    (interactive)
    (cond ((eq major-mode 'rust-mode)
           (compile "cargo clippy --fix --allow-dirty"))
          ((eq major-mode 'enh-ruby-mode)
           (rubocop-autocorrect-project))
          (t
           (%log "no clippy-fix for " major-mode)))
    (%full-save))

 ;;; ---- lsp ---- ;;
  (defun %go-to-definition ()
    (interactive)
    (cond ((eq major-mode 'tuareg-mode)
           (merlin-locate))
          (t
           (call-interactively 'xref-find-definitions))
          ))

  (defalias '%autocomplete 'completion-at-point)
  (defun %start-lsp ()
    (interactive)
    (require 'lsp-mode)
    (lsp-mode)
    (%log "started LSP"))

  (defalias '%lsp-action 'lsp-execute-code-action)
  (defalias '%show-type 'lsp-describe-thing-at-point)
  (defun %show-type ()
    (interactive)
    (cond ((eq major-mode 'tuareg-mode)
           (merlin-type-enclosing))
          (t
           (if (bound-and-true-p lsp-mode)
               (lsp-describe-thing-at-point)
             (%log "can't show type")))
          ))

 ;;; ---- keys ---- ;;
  ;; FIXME instead of separate functions, these could also use a cond
  (defun %set-last-function (func)
    (%log "setting last function")
    (setq %last-action-type 'function)
    (setq %last-function func)
    )

  (defun %set-last-target-action (target action)
    (%log "setting last target action")
    (setq %last-action-type 'target)
    (setq %last-target target)
    (setq %last-action action)
    )

  (defun %set-last-delimited-action (prefix-key i-or-o start-char action)
    (%log "setting last delimited action")
    (setq %last-action-type 'delimited-target)
    (setq %last-prefix-key prefix-key)
    (setq %last-i-or-o i-or-o)
    (setq %last-start-char start-char)
    (setq %last-action action)
    )

  (defun %set-last-until-action (until-char action)
    (%log "setting last until action")
    (setq %last-action-type 'until)
    (setq %last-until until-char)
    (setq %last-action action)
    )


  (setq %last-action-type nil)

  (defun %repeat-action ()
    (interactive)
    (cond ((eq %last-action-type 'function)
           (funcall %last-function))
          ((eq %last-action-type 'target)
           (%execute-target-action %last-target %last-action))
          ((eq %last-action-type 'delimited-target)
           (%execute-delimited-action %last-prefix-key %last-i-or-o %last-start-char %last-action))
          ((eq %last-action-type 'until)
           (%execute-until-action %last-until %last-action))
          (t (%log "nothing"))))

  (defun %execute-target-action (target action)
    (save-excursion
      (%select-target target)
      (funcall action))
    )

  (defun %execute-until-action (until-char action)
    (save-excursion
      (set-mark-command nil)
      (funcall '%until until-char)
      (funcall action))
    )

  (defun %execute-delimited-action (prefix-key i-or-o start-char action)
    (let ((run-cmd
           (lambda () (interactive)
             (if (string= i-or-o "i")
                 (%select-inside start-char)
               (%select-outside start-char))
             (funcall action))))
      (if (string= prefix-key "c")
          (save-excursion (funcall run-cmd))
        (funcall run-cmd))))

  (defun %define-prefix-action (prefix-key action)
    (defun %chars (string) (split-string string "" t))

    (defun %define-delimited-targets (i-or-o pair)
      (defun %define-delimited-key (selection-start-char target-key)
        (let* ((key-seq (concat prefix-key i-or-o target-key)))
          (define-key modalka-mode-map key-seq (lambda ()
                                                 (interactive)
                                                 (%set-last-delimited-action prefix-key i-or-o selection-start-char action)
                                                 (%execute-delimited-action prefix-key i-or-o selection-start-char action)
                                                 ))))

      (let ((target-keys (%chars (car pair)))
            (start-char (cdr pair)))
        (mapcar (apply-partially '%define-delimited-key start-char) target-keys)
        ))

    (defun %define-until-key ()
      (let ((key-seq (concat prefix-key "u")))
        (define-key modalka-mode-map key-seq (lambda (until-char)
                                               (interactive "c")
                                               (%set-last-until-action until-char action)
                                               (%execute-until-action until-char action)
                                               ))))

    (defun %define-target-keys (i-or-o targets)
      (defun %define-target-key (target-key)
        (let* ((target (concat i-or-o target-key))
               (key-seq (concat prefix-key target)))
          (define-key modalka-mode-map key-seq (lambda ()
                                                 (interactive)
                                                 (%set-last-target-action target action)
                                                 (%execute-target-action target action)
                                                 ))))
      (mapcar '%define-target-key targets))
    (let ((delimited-targets '(("b()" . "(") ("B{}" . "{") ("[]" . "[") ("S'" . "'") ("s\"" . "\"")))
          (targets (%chars "lpwW")))
      (%define-until-key)
      (%define-target-keys "i" targets)
      (%define-target-keys "o" targets)
      (mapc (apply-partially '%define-delimited-targets "i") delimited-targets)
      (mapc (apply-partially '%define-delimited-targets "o") delimited-targets)))

  ;; remapped in kitty
  ;; "actual key pressed" => "what emacs sees"
  ;; "C-i"                => "M-!"
  ;; "S-RET"              => "M-@"
  ;; "C-TAB"              => "M-+"
  ;; "C-/"                => "C-_"
  ;; "M-<deletechar>"     => "M-d"
  :bind
  (:map my-keys-minor-mode-map
        ("<f8>" . '%normal)
        ("C-<f8>" . '%full-save)
        ("M-+" . '%autocomplete)

        ("C-M-n" . '%next-error)
        ("C-M-e" . '%previous-error)

        ("C-x z" . '%noop)

        ("M-(" . 'winner-undo)
        ("M-)" . 'winner-redo)

        ("M-&" . '%fold) ("M-]" . '%unfold)

        ("M-Q" . '%noop)
        ("M-q M-DEL RET" . 'save-buffers-kill-terminal)
        ("M-q '" . '%find-open-buffer)
        ("M-q o" . '%open-file)
        ("M-q DEL" . '%quit-window)
        ("M-q M-m" . '%maximize)
        ("M-q SPC" . '%dired)

        ("M-SPC M-c" . '%insert-blank-comment)
        ("M-SPC M-t" . '%insert-todo-comment)
        ("M-SPC M-f" . '%insert-fixme-comment)
        ("M-SPC M-p" . '%insert-polish-comment)

        ("M-SPC ?" . '%help)

        ("M-SPC z" . '%bob)
        ("M-SPC -" . '%eob)

        ("M-q H" . '%split-left)
        ("M-q N" . '%split-down)
        ("M-q E" . '%split-up)
        ("M-q I" . '%split-right)

        ("M-q M-h" . '%switch-window-left)
        ("M-q M-n" . '%switch-window-down)
        ("M-q M-e" . '%switch-window-up)
        ("M-q M-i" . '%switch-window-right)

        ("M-w" . '%wipe-line)                 ("M-W" . '%noop)      ("C-w" . '%wipe-eol)
        ("M-b" . '%noop)                      ("M-B" . '%noop)      ("C-b" . '%become-eol)
        ("M-p" . '%paste-fmt)                 ("M-P" . '%paste-raw) ("C-p" . '%paste-eol)
        ("M-f" . '%flip-selection-and-adjust) ("M-F" . '%noop)      ("C-f" . '%noop)

        ("M-a" . '%add)           ("M-A" . '%noop) ("C-a" . '%add-eol)
        ("M-r" . '%repeat-action) ("M-R" . '%noop) ("C-r" . '%noop)
        ("M-s" . '%select)        ("M-S" . '%noop) ("C-s" . '%select-eol)
        ("M-t" . '%noop)          ("M-T" . '%noop) ("C-t" . '%trade-eol)

        ("M-/" . '%help) ("M-?" . '%help)
        ("M-," . '%m-x-menu)

        ("M-x" . '%noop)  ("M-X" . '%noop)
        ("M-c" . '%noop)  ("M-C" . '%noop)
        ("M-d" . '%m-del) ("M-D" . '%noop) ("C-d" . '%dupe-eol)
        ("M-v" . '%noop)  ("M-V" . '%noop) ("C-v" . '%vanish-eol)

        ("M-`" . 'pop-to-mark-command) ("M-~" . 'helm-all-mark-rings)

        ("M-z" . '%bol)
        ("M-l" . '%bw-bow) ("C-l" . '%recenter)
        ("M-u" . '%undo)   ("M-U" . '%undo-tree) ("C-u" . '%undo)
        ("M-y" . '%fw-bow) ("C-y" . '%noop)
        ("M-:" . '%redo)

        ("M-h" . '%left)   ("C-h" . '%drag-bw)
        ("M-n" . '%down)   ("C-n" . '%drag-line-fw)
        ("M-e" . '%up)     ("C-e" . '%drag-line-bw)
        ("M-i" . '%right)  ("M-!" . '%drag-fw)
        ("M-o" . '%eow)    ("C-o" . 'universal-argument)
        ("M-'" . '%search) ("M-\"" . '%search)

        ("M-j" . '%join-line-below) ("M-J" . '%join-line-above)
        ("M-k" . '%noop)            ("M-K" . '%noop)
        ("M-m" . '%bop)             ("M-M" . '%scroll-up-page)
        ("M-DEL" . '%m-bs)
        ("M-." . '%eop)             ("M->" . '%scroll-down-page)
        ("M--" . '%eol)             ("C-_" . '%eol)

        :map modalka-mode-map
        ("RET" . '%newline-here)

        ("!" . '%noop) ("@" . '%noop) ("#" . '%noop) ("$" . '%noop) ("%" . '%noop) ("^" . '%noop) ("&" . '%line-number-command) ("*" . '%noop)
        ("1" . '%noop) ("2" . '%noop) ("3" . '%noop) ("4" . '%noop) ("5" . '%noop) ("6" . '%noop) ("7" . '%noop) ("8" . '%noop) ("9" . '%noop) ("0" . '%noop)

        ("[" . '%fold)             ("]" . '%unfold)
        ("{" . '%helm-multi-swoop) ("}" . '%search-project)
        (";" . '%noop)

        ("q'" . '%find-open-buffer)
        ("qb" . '%buffer-list)
        ("ql" . '%go-to-compilation-log)
        ("qs" . '%go-to-scratch)
        ("qo" . '%open-file)
        ("q DEL" . '%quit-window)
        ("qm" . '%maximize)
        ("qw" . '%restore-layout)
        ("q=" . 'balance-windows)

        ("qH" . '%split-left)
        ("qN" . '%split-down)
        ("qE" . '%split-up)
        ("qI" . '%split-right)

        ("qh" . '%switch-window-left)
        ("qn" . '%switch-window-down)
        ("qe" . '%switch-window-up)
        ("qi" . '%switch-window-right)

        ("q SPC d" . '%dired)
        ("q SPC s" . '%show-filepaths)
        ("q SPC c" . '%cp-filepath)

        ("W" . '%noop)
        ("B" . '%noop)
        ("p" . '%paste-fmt)                 ("P" . '%paste-raw)
        ("f" . '%flip-selection-and-adjust) ("F" . '%noop)

        ("a" . '%add)           ("A" . '%add-bol-indent)
        ("r" . '%repeat-action) ("R" . '%noop)
        ("s" . '%select)        ("S" . '%select-line)
        ("T" . '%noop)
        ("g" . '%graft)         ("G" . 'xah%toggle-previous-letter-case)

        ("," . '%m-x-menu) ("<" . '%noop)
        ("x" . '%exchange) ("X" . '%exchange-regex)
        ("C" . '%noop)
        ("D" . '%noop)
        ("v" . '%vanish)

        ("/" . '%noop)
        ("\\" . '%noop)

        ;; help
        ("? h" . '%help)
        ("? SPC" . 'help-for-help)
        ("? m" . '%help-messages)
        ("? M" . '%help-mode)
        ("? k" . '%help-key)
        ("? '" . '%help-search)
        ("? s" . '%help-syntax-highlighting)
        ("? v" . '%help-variable)
        ("? c" . 'gpt%show-cursor-position)

        ;; modifier
        ("SPC u" . '%undo-tree)
        ("SPC SPC u" . 'undo-tree-mode)
        ("SPC '" . 'helm-multi-swoop-current-mode)
        ("SPC p" . '%paste-from-history)
        ("SPC SPC c" . '%xclip)
        ("SPC x" . '%query-exchange)
        ("SPC X" . '%query-exchange-regex)

        ;; command
        ("SPC j" . '%justify-text)

        ("SPC L" . '%start-lsp)
        ("SPC b" . 'private%compile-build)
        ("SPC e" . 'private%compile-exec)
        ("SPC t" . '%show-type)
        ("SPC T" . '%run-tests)
        ("SPC l" . '%lsp-action)
        ("SPC f" . '%format-file)
        ("SPC c" . '%clippy)
        ("SPC C" . '%clippy-fix)
        ("SPC d" . '%go-to-definition)

        ("SPC =" . 'so%increment-next-number)
        ("SPC -" . 'so%decrement-next-number)

        ("SPC S" . '%refresh-syntax-highlighting)

        ("SPC g" . '%git)
        ("SPC m" . '%start-macro)
        ("SPC M" . '%end-macro)
        ("SPC RET" . '%run-macro)
        ("SPC (" . '%eval-expr) ("SPC )" . '%eval-inline)

        ("=" .'%comment)
        ("+" . '%comment-par)

        ("|a" . '%rect-add)
        ("|t" . '%rect-trade)
        ("|w" . '%rect-wipe)
        ("|p" . '%rect-paste)
        ("j" . '%bol-indent) ("J" . '%join-line-below)

        ("`" . 'pop-global-mark)     ("~" . 'helm-all-mark-rings)
        ("(" . 'pop-to-mark-command) (")" . '%cycle-spacing)

        ("z" . '%bol)    ("Z" . '%bob)
        ("l" . '%bw-bow) ("L" . '%bw-boW)
        ("u" . '%undo)   ("U" . '%undo-tree)
        ("y" . '%fw-bow) ("Y" . '%fw-boW)
        (":" . '%redo)

        ("<deletechar>" . '%del)

        ("h" . '%left)   ("H" . '%noop)
        ("n" . '%down)   ("N" . '%noop)
        ("e" . '%up)     ("E" . '%noop)
        ("i" . '%right)  ("I" . '%noop)
        ("o" . '%eow)    ("O" . '%eoW)
        ("'" . '%search) ("\"" . '%search)

        ("k" . '%redo) ("K" . '%undo-tree)
        ("m" . '%bop)  ("M" . '%scroll-up-page)
        ("DEL" . '%bs)
        ("." . '%eop)  (">" . '%scroll-down-page)
        ("-" . '%eol)  ("_" . '%eob)
        )

  :config
  (add-hook 'text-mode-hook #'modalka-mode)
  (add-hook 'prog-mode-hook #'modalka-mode)

  (defun %set-mode-colors ()
    ;; (rainbow-mode)
    (let* ((colors (cond (modalka-mode '("#bce" "#888" "#141414"))
                         (t            '("#c88" "#a89" "#181114"))))
           (mode-line-active-bg (car colors))
           (line-number-fg (cadr colors))
           (line-number-bg (caddr colors))
           )
      (set-face-background 'mode-line mode-line-active-bg)
      (set-face-foreground 'line-number line-number-fg)
      (set-face-background 'line-number line-number-bg)
      ))

  (define-key global-map (kbd "C-M-x") ctl-x-map)

  (add-hook 'post-command-hook '%adjust-selection-hook)
  (add-hook 'modalka-mode-hook '%set-mode-colors)
  )
