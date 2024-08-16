;;; modalka-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "modalka" "modalka.el" (0 0 0 0))
;;; Generated autoloads from modalka.el

(defvar modalka-excluded-modes nil "\
A list of major modes in which `modalka-mode' should not be activated.

This variable is considered when Modalka is enabled globally via
`modalka-global-mode'.")

(custom-autoload 'modalka-excluded-modes "modalka" t)

(autoload 'modalka-define-key "modalka" "\
Register translation from ACTUAL-KEY to TARGET-KEY.

\(fn ACTUAL-KEY TARGET-KEY)" nil nil)

(autoload 'modalka-define-kbd "modalka" "\
Register translation from ACTUAL-KBD to TARGET-KBD.

The arguments are accepted in the format that is used for saving
keyboard macros (see `edmacro-mode').

\(fn ACTUAL-KBD TARGET-KBD)" nil nil)

(autoload 'modalka-remove-key "modalka" "\
Unregister translation from KEY.

\(fn KEY)" nil nil)

(autoload 'modalka-remove-kbd "modalka" "\
Unregister translation from KBD.

The arguments are accepted in the format that is used for saving
keyboard macros (see `edmacro-mode').

\(fn KBD)" nil nil)

(autoload 'modalka-mode "modalka" "\
Toggle `modalka-mode'.

With a prefix argument ARG, enable `modalka-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'.

This minor mode sets up translation of key bindings according to
a configuration created previously with `modalka-define-key' and
`modalka-define-keys'.

\(fn &optional ARG)" t nil)

(put 'modalka-global-mode 'globalized-minor-mode t)

(defvar modalka-global-mode nil "\
Non-nil if Modalka-Global mode is enabled.
See the `modalka-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `modalka-global-mode'.")

(custom-autoload 'modalka-global-mode "modalka" nil)

(autoload 'modalka-global-mode "modalka" "\
Toggle Modalka mode in all buffers.
With prefix ARG, enable Modalka-Global mode if ARG is positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Modalka mode is enabled in all buffers where `modalka--maybe-activate' would do it.

See `modalka-mode' for more information on Modalka mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "modalka" '("modalka-"))

;;;***

;;;### (autoloads nil nil ("modalka-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; modalka-autoloads.el ends here
