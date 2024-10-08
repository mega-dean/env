;;; Local Variables:
;;; eval: (rainbow-mode)
;;; End:

(deftheme otter
  "originally copied from badger-theme.el")

(setq %bg "#212121")
(setq %bg-darker "#111")
(setq %bg-dark "#181818")
(setq %bg-gray "#222")
(setq %bg-light "#282828")
(setq %bg-lighter "#333")
(setq %bg-strong "#555")
(setq %bg-weak "#333")

(setq %fg "#dde2e7") ; 90
(setq %fg-darker "#ddd")
(setq %fg-dark "#d8d8d8")
(setq %fg-gray "#eee")
(setq %fg-light "#e8e8e8")
(setq %fg-strong "#fff")
(setq %fg-weak "#888")

(setq %black "#1a202c")
(setq %red "#dca8a8")
(setq %green "#9bcead")
(setq %yellow "#ddd2ae")
(setq %blue "#90b9d1")
(setq %magenta "#c3b0e0")
(setq %cyan "#7cd0c5")
(setq %white "#cdd2d7")

;; (setq %black "#1a202c")
;; (setq %red "#fe8181")
;; (setq %green "#97e7b3")
;; (setq %yellow "#ffeac6")
;; (setq %blue "#8ecef5")
;; (setq %magenta "#d6befc")
;; (setq %cyan "#7de7d9")
;; (setq %white "#dde2e7")

(setq %brightblack "#051610")
(setq %brightred "#f76665")
(setq %brightgreen "#64d18d")
(setq %brightyellow "#fedfaa")
(setq %brightblue "#5fb4ef")
(setq %brightmagenta "#b797f7")
(setq %brightcyan "#48d0c4")
(setq %brightwhite "#f2f2f2")

(setq %deepred "#611")
(setq %deepgreen "#242")
(setq %deepyellow "#b97")
(setq %deepblue "#227")
(setq %deepmagenta "#626")
(setq %deepcyan "#055")


(setq %hl-weak "#112233")
(setq %hl "#234")
(setq %hl-strong "#345")
(setq %hl-stronger "#456")

(setq %hl-selection %deepgreen)
(setq %hl-cursor %deepred)

(setq %black0 "#000")
(setq %gray1 "#111")
(setq %gray2 "#222")
(setq %gray3 "#333")
(setq %gray4 "#444")
(setq %gray5 "#555")
(setq %gray6 "#666")
(setq %gray7 "#777") ; 50
(setq %gray8 "#888")
(setq %gray9 "#999")
(setq %graya "#aaa")
(setq %grayb "#bbb")
(setq %grayc "#ccc")
(setq %grayd "#ddd")
(setq %graye "#eee")
(setq %whitef "#fff")

;; (defmacro face%build (name &rest config)
;;   `(defface ,name
;;      '((t ,config))
;;      ""
;;      :group '%core)
;;   )

;; (defmacro face%create (name fg bg)
;;   `(face%build ,name :foreground ,fg :background ,bg))

;; (defmacro face%inherit (parent name &rest overrides)
;;   `(face%build ,name :inherit ,parent ,@overrides))

(defface todo-comment
  '((t :foreground "#222"
       :background "#447"
       :weight bold
       ))
  "Face for subtle todo highlighting"
  :group 'basic-faces )

(defface fixme-comment
  '((t :foreground "#222"
       :background "#252"
       :weight bold
       ))
  "Face for subtle todo highlighting"
  :group 'basic-faces )

;; custom-theme-set-faces uses some weird macro syntax, which is what the ` and , are
(custom-theme-set-faces
 'otter

 `(default ((t (:foreground ,%fg :background ,%bg))))
 ;; `(line-number ((t (:foreground ,%fg-weak :background ,%bg-dark)))) ;; set in modalka-keys.el
 `(line-number-current-line ((t (:inherit line-number :underline t))))
 ;; `(linum ((t (:foreground ,%fg-weak :background ,%bg-dark))))
 `(region ((t (:background ,%hl))))

 `(isearch ((t (:background ,%hl-cursor)))) ;; match at cursor
 `(lazy-highlight ((t (:background ,%hl-stronger)))) ;; other matches

 `(success ((t (:foreground ,%black0 :background ,%brightgreen :weight bold))))
 `(warning ((t (:foreground ,%black0 :background ,%brightyellow :weight bold))))
 ;; `(compilation-warning ((t (:foreground ,%black0 :background ,%brightyellow :weight bold))))
 `(error ((t (:background ,%deepred :weight bold))))
 `(next-error ((t (:background ,%deepred :weight bold))))

 `(whitespace-newline ((t (:foreground ,%bg :background ,%bg))))
 `(whitespace-trailing ((t (:inherit error))))

 ;; `(mode-line ((t (:background ,%green :foreground ,%magenta ))))
 ;; `(mode-line-inactive ((t (:background ,%gray8 :foreground ,%white ))))
 ;; `(mode-line-active ((t (:foreground "#000"))))
 `(mode-line-inactive ((t (:foreground "#000" :background ,%gray4))))
 `(vertical-border ((t (:foreground ,%gray4 :background ,%gray4))))
 `(mode-line-buffer-id ((t (:foreground ,%white :weight bold))))
 `(minibuffer-prompt ((t (:foreground ,%cyan))))

 `(font-lock-comment-delimiter-face ((t (:foreground ,%gray5))))
 `(font-lock-comment-face ((t (:foreground ,%gray5))))
 `(font-lock-doc-face ((t (:foreground ,%gray6))))
 `(font-lock-negation-char-face ((t (:foreground ,%red))))
 `(font-lock-preprocessor-face ((t (:foreground ,%magenta))))
 `(font-lock-builtin-face ((t (:foreground ,%cyan))))
 `(font-lock-variable-name-face ((t (:inherit default))))
 `(font-lock-function-name-face ((t (:foreground ,%red))))
 `(font-lock-string-face ((t (:foreground ,%yellow))))
 `(font-lock-constant-face ((t (:foreground ,%blue))))
 `(font-lock-type-face ((t (:foreground ,%green))))
 `(font-lock-keyword-face ((t (:foreground ,%gray9))))
 `(button ((t (:foreground ,%cyan :underline t))))

 `(tuareg-font-lock-module-face ((t (:foreground ,%cyan))))
 `(tuareg-font-lock-label-face ((t (:inherit default))))
 `(tuareg-font-lock-governing-face ((t (:foreground ,%gray9))))
 `(tuareg-font-lock-operator-face ((t (:foreground ,%gray7))))
 `(tuareg-font-lock-extension-node-face ((t (:foreground ,%brightblue))))
 `(tuareg-font-lock-infix-extension-node-face ((t (:foreground ,%brightblue))))
 `(merlin-type-face ((t (:background ,%deepcyan :foreground ,%white))))
 `(merlin-compilation-error-face ((t (:background ,%deepred))))

 `(rust-string-interpolation ((t (:foreground ,%deepyellow))))
 `(rust-builtin-formatting-macro ((t (:inherit font-lock-keyword-face))))

 `(helm-selection ((t (:background ,%deepcyan :weight bold))))
 `(helm-match ((t (:foreground ,%fg-darker :weight bold :underline t))))
 `(header-line ((t (:foreground "#056" :background "#023"))))
 `(helm-source-header ((t (:foreground ,%fg-weak :background ,%bg-dark))))
 `(helm-candidate-number ((t (:inherit helm-source-header))))
 `(helm-ff-prefix ((t (:inherit helm-source-header))))
 `(helm-ff-file-extension ((t (:inherit helm-ff-file))))
 `(helm-buffer-process ((t (:inherit helm-ff-file))))

 `(helm-swoop-target-line-face ((t (:inherit isearch :weight bold))))
 `(helm-swoop-target-word-face ((t (:inherit lazy-highlight))))

 `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
 `(web-mode-block-delimiter-face ((t (:foreground ,%red))))

 `(deadgrep-filename-face ((t (:foreground ,%brightmagenta :weight bold :underline t))))
 `(deadgrep-match-face ((t (:background ,%deepmagenta))))

 ;; other sections
 `(diff-removed ((t (:foreground ,%red))))
 `(diff-added ((t (:foreground ,%green))))

 ;; current section
 `(diff-refine-removed ((t (:foreground ,%brightred :background ,%hl-weak))))
 `(diff-refine-added ((t (:foreground ,%brightgreen :background ,%hl-weak))))


;;;;; magit
;;;;;; headings and diffs
 `(magit-section-highlight           ((t (:background ,%hl-weak))))
 `(magit-section-heading             ((t (:foreground ,%yellow :weight bold))))
 `(magit-section-heading-selection   ((t (:foreground ,%red :weight bold))))

 `(magit-diff-added ((t (:inherit diff-added))))
 `(magit-diff-added-highlight ((t (:inherit diff-refine-added))))
 `(magit-diff-removed ((t (:inherit diff-removed))))
 `(magit-diff-removed-highlight ((t (:inherit diff-refine-removed))))

 `(magit-diff-file-heading           ((t (:weight bold))))
 `(magit-diff-file-heading-highlight ((t (:background ,%hl-weak :weight bold))))
 ;; `(magit-diff-file-heading-selection ((t (:background ,%green :foreground ,%cyan :weight bold))))
 ;; current section
 `(magit-diff-hunk-heading-highlight ((t (:background ,%hl-weak :foreground ,%brightcyan))))
 `(magit-diff-hunk-heading           ((t (:foreground ,%cyan))))
 ;; `(magit-diff-hunk-heading-selection ((t (:background ,%brightmagenta :foreground ,%cyan))))
 `(magit-diff-lines-heading          ((t (:foreground ,%cyan))))
 `(magit-diff-context-highlight      ((t (:background ,%hl-weak))))

 `(magit-diffstat-added   ((t (:inherit diff-added))))
 `(magit-diffstat-removed ((t (:inherit diff-removed))))
;;;;;; popup
 `(magit-popup-heading             ((t (:foreground ,%yellow  :weight bold))))
 `(magit-popup-key                 ((t (:foreground ,%brightgreen :weight bold))))
 `(magit-popup-argument            ((t (:foreground ,%green   :weight bold))))
 `(magit-popup-disabled-argument   ((t (:foreground ,%grayb    :weight normal))))
 `(magit-popup-option-value        ((t (:foreground ,%blue  :weight bold))))
;;;;;; process
 `(magit-process-ok    ((t (:foreground ,%green  :weight bold))))
 `(magit-process-ng    ((t (:foreground ,%red    :weight bold))))
;;;;;; log
 `(magit-log-author    ((t (:foreground ,%cyan))))
 `(magit-log-date      ((t (:foreground ,%gray8))))
 `(magit-log-graph     ((t (:foreground ,%gray8))))
;;;;;; sequence
 `(magit-sequence-pick ((t (:foreground ,%yellow))))
 `(magit-sequence-stop ((t (:foreground ,%green))))
 `(magit-sequence-part ((t (:foreground ,%brightyellow))))
 `(magit-sequence-head ((t (:foreground ,%blue))))
 `(magit-sequence-drop ((t (:foreground ,%red))))
 `(magit-sequence-done ((t (:foreground ,%gray8))))
 `(magit-sequence-onto ((t (:foreground ,%gray8))))
;;;;;; bisect
 `(magit-bisect-good ((t (:foreground ,%green))))
 `(magit-bisect-skip ((t (:foreground ,%yellow))))
 `(magit-bisect-bad  ((t (:foreground ,%red))))
;;;;;; blame
 `(magit-blame-heading ((t (:background ,%gray8 :foreground ,%blue))))
 `(magit-blame-hash    ((t (:background ,%gray8 :foreground ,%blue))))
 `(magit-blame-name    ((t (:background ,%gray8 :foreground ,%cyan))))
 `(magit-blame-date    ((t (:background ,%gray8 :foreground ,%cyan))))
 `(magit-blame-summary ((t (:background ,%gray8 :foreground ,%blue :weight bold))))
;;;;;; references etc
 `(magit-dimmed         ((t (:foreground ,%gray8))))
 `(magit-hash           ((t (:foreground ,%gray8))))
 `(magit-tag            ((t (:foreground ,%cyan :weight bold))))
 `(magit-branch-remote  ((t (:foreground ,%green  :weight bold))))
 `(magit-branch-local   ((t (:foreground ,%blue   :weight bold))))
 `(magit-branch-current ((t (:foreground ,%blue   :weight bold :box t))))
 `(magit-head           ((t (:foreground ,%blue   :weight bold))))
 `(magit-refname        ((t (:background ,%grayb :foreground ,%white :weight bold))))
 `(magit-refname-stash  ((t (:background ,%grayb :foreground ,%white :weight bold))))
 `(magit-refname-wip    ((t (:background ,%grayb :foreground ,%white :weight bold))))
 `(magit-signature-good      ((t (:foreground ,%green))))
 `(magit-signature-bad       ((t (:foreground ,%red))))
 `(magit-signature-untrusted ((t (:foreground ,%yellow))))
 `(magit-signature-expired   ((t (:foreground ,%cyan))))
 `(magit-signature-revoked   ((t (:foreground ,%magenta))))
 `(magit-cherry-unmatched    ((t (:foreground ,%cyan))))
 `(magit-cherry-equivalent   ((t (:foreground ,%magenta))))
 `(magit-reflog-commit       ((t (:foreground ,%green))))
 `(magit-reflog-amend        ((t (:foreground ,%magenta))))
 `(magit-reflog-merge        ((t (:foreground ,%green))))
 `(magit-reflog-checkout     ((t (:foreground ,%blue))))
 `(magit-reflog-reset        ((t (:foreground ,%red))))
 `(magit-reflog-rebase       ((t (:foreground ,%magenta))))
 `(magit-reflog-cherry-pick  ((t (:foreground ,%green))))
 `(magit-reflog-remote       ((t (:foreground ,%cyan))))
 `(magit-reflog-other ((t (:foreground ,%cyan))))

;;;;; ruby
 `(enh-ruby-op-face ((t (:inherit default))))
 `(enh-ruby-string-delimiter-face ((t (:inherit font-lock-string-face))))

 ;; `(sh-heredoc ((t (:inherit font-lock-string-face))))
 `(sh-heredoc ((t (:foreground ,%deepyellow))))

;;;;; org-mode
 `(outline-1 ((t (:foreground ,%magenta))))
 `(outline-2 ((t (:foreground ,%cyan))))
 `(outline-3 ((t (:foreground ,%green))))
 `(outline-4 ((t (:foreground ,%blue))))
 `(outline-5 ((t (:inherit outline-1))))
 `(outline-6 ((t (:inherit outline-2))))
 `(outline-7 ((t (:inherit outline-3))))
 `(outline-8 ((t (:inherit outline-4))))

 `(org-checkbox ((t (:foreground ,%brightblue :weight bold))))
 `(org-date ((t (:foreground ,%brightmagenta))))
 ;; `(org-headline-done ((t (:foreground ,%zenburn-green+3))))
 ;; `(org-hide ((t (:foreground ,%zenburn-bg-1))))
 `(org-link ((t (:foreground ,%brightcyan))))
 `(org-table ((t (:foreground ,%brightcyan))))
 `(org-tag ((t (:weight bold))))

 `(org-done ((t (:weight bold :foreground ,%brightgreen))))
 `(org-headline-done ((t (:foreground ,%gray7))))
 `(org-todo ((t (:weight bold :foreground ,%brightred))))

 `(org-warning ((t (:foreground ,%red))))
 `(org-code ((t (:background ,%hl-strong))))

 `(org-ellipsis ((t (:foreground ,%grayb))))

 `(org-agenda-dimmed-todo-face ((t (:weight bold))))

 )

(provide-theme 'otter)


;; highlight special words (for comments, but they highlight everywhere)
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(
               ("\\<\\(FIXME\\|POLISH\\)" 1 'fixme-comment prepend)
               ("\\<\\(TODO\\)" 1 'todo-comment prepend)
               )
             )))


(defun color-variable-name-face ()
  "Customize `font-lock-variable-name-face' for specific major modes."
  (face-remap-add-relative 'font-lock-variable-name-face nil :foreground %magenta))

(add-hook 'yaml-mode-hook 'color-variable-name-face)
(add-hook 'web-mode-hook 'color-variable-name-face)
(add-hook 'enh-ruby-mode-hook (lambda ()
                                (color-variable-name-face)
                                (face-remap-add-relative 'font-lock-builtin-face nil :inherit font-lock-keyword-face)
                                ))
