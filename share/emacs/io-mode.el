;; Author: Alexander Schmolck, LShift LDT
(require 'cl)
(require 'whitespace)
(defconst io-font-lock-keywords
  `(("^#.*$" 0 font-lock-comment-face)
    ("^<\\([^\t]+\\) *\t" 1 font-lock-constant-face)
    ("^\\(\\.\\.\\.\\) *\t" 1 font-lock-type-face)
    ("^>?\\([^\t]+\\) *\t" 1 font-lock-variable-name-face)
    ("\t" 0 whitespace-tab t)
    (" +$" 0 whitespace-space-after-tab t)
    )
  )
(defvar io-mode-syntax-table nil
  "Io mode syntax table.")

(unless io-mode-syntax-table
  (setq io-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\#  "<" io-mode-syntax-table) ; punct, not bracket
  )
;; (define-derived-mode io-mode fundamental-mode "IO Test"
;;     "Major mode for writing input/output test files."


(defun io-mode ()
  "A simple mode to edit rabbit streams `.io' input/output test files."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode         'io-mode
        mode-name          "io")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults (list
                            'io-font-lock-keywords ;KEYWORDS
                            nil                    ;KEYWORDS-ONLY
                            nil                    ;CASE-FOLD
                            ;; could tune this for further optimization
                            nil                 ;SYNTAX-ALIST
                            ;; a mere speed-up
                            #'beginning-of-line ;SYNTAX-BEGIN
                            ))
  (setq comment-start "#")
  ;; (setq comment-start-skip "^#")
  (setq comment-end "")
  ;; (whitespace-mode)
  ;; (whitespace-toggle-options 'trailing)
  ;; (whitespace-toggle-options 'space-before-tab)
  ;; (whitespace-toggle-options 'space-after-tab)
  (setq indent-tabs-mode t)

  (local-set-key "\C-i" 'self-insert-command)
  (set-syntax-table io-mode-syntax-table))


;(add-to-list auto-mode-alist '("\\.io$" . io-mode))
(provide 'io-mode)