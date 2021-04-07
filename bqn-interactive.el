;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'bqn-util)
(require 'bqn-syntax)

(defcustom bqn-interactive-cmd-buffer-name "BQN"
  "Name of the buffer which contains the bqn-interactive-cmd session"
  :type 'string
  :group 'bqn)

(defcustom bqn-interactive-cmd "/Users/David/Downloads/BQN/REPL"
  "Name of the executable used for the BQN REPL session"
  :type 'string
  :group 'bqn)

(defcustom bqn-interactive-cmd-args '()
  "Arguments to be passed to the bqn-interactive-cmd on start"
  :type 'string
  :group 'bqn)

(defcustom bqn-interactive-cmd-init-file nil
  "Full path to the file who's contents are sent to the
  bqn-interactive-cmd on start
Should be NIL if there is no file not the empty string"
  :type 'string
  :group 'bqn)

(defvar bqn-interactive-comint-input-filter-function nil
  "BQN mode specific mask for comint input filter function")

(defvar bqn-interactive-comint-output-filter-function nil
  "BQN mode specific mask for comint output filter function")

(defvar bqn-interactive-comint-preoutput-filter-function nil
  "BQN mode specific mask for comint preoutput filter function")

(defun bqn-interactive-create-session ()
  "Starts a comint session wrapped around the bqn-interactive-cmd"
  (setq comint-process-echoes t)
  (apply 'make-comint bqn-interactive-cmd-buffer-name
         bqn-interactive-cmd bqn-interactive-cmd-init-file bqn-interactive-cmd-args)
  (pop-to-buffer (process-buffer (get-process bqn-interactive-cmd-buffer-name)))
  (bqn-interactive-mode)
  (mapc
   (lambda ( comint-hook-sym)
     (let ((local-comint-hook-fn-sym
            (intern
             (replace-regexp-in-string
              "s$" "" (concat "bqn-interactive-" (symbol-name comint-hook-sym))))))
       (when (symbol-value local-comint-hook-fn-sym)
         (add-hook comint-hook-sym (symbol-value local-comint-hook-fn-sym)))))
   '(comint-input-filter-functions
     comint-output-filter-functions
     comint-preoutput-filter-functions)))

(defun bqn-interactive-ensure-session ()
  "Checks for a running bqn-interactive-cmd comint session and either
  returns it or starts a new session and returns that"
  (or (get-process bqn-interactive-cmd-buffer-name)
      (progn
        (bqn-interactive-create-session)
        (get-process bqn-interactive-cmd-buffer-name))))

;;;###autoload
(defun bqn-interactive ()
  "Ensures a running bqn-interactive-cmd session and switches focus to
the containing buffer"
  (interactive)
  (switch-to-buffer-other-window (process-buffer (bqn-interactive-ensure-session)))
  (bqn-interactive-mode))

(defun bqn-interactive-send-string (string)
  "Sends string to BQN console"
  (let ((session (bqn-interactive-ensure-session)))
    (with-current-buffer (process-buffer session)
      (goto-char (point-max))
      (insert-before-markers (concat string "\n")))
    (comint-simple-send (process-buffer session) string)))

(defun bqn-interactive-send-buffer ()
  "Send the entire content of the current buffer to the active
GNU APL interpreter."
  (interactive)
  (bqn-interactive-send-string (buffer-string)))

(defun bqn-interactive-send-region (start end)
  "Send the region to the active GNU APL interpreter."
  (interactive "r")
  (bqn-interactive-send-string (buffer-substring start end)))

(defun bqn-interactive-send-current-line ()
  "Sends current line to the bqn-interactive-cmd session and executes it"
  (interactive)
  (bqn-interactive-send-region (point-at-bol) (point-at-eol)))

(defun bqn--make-interactive-mode-map ()
  (let ((map (bqn--make-base-mode-map bqn-interactive-mode-map-prefix)))
    (define-key map (kbd "TAB") 'completion-at-point)
    ; (define-key map (kbd "C-c C-f") 'bqn-edit-function)
    ; (define-key map (kbd "C-c C-v") 'bqn-edit-variable)
    map))

(defun bqn--set-interactive-mode-map-prefix (symbol new)
  "Recreate the prefix and the keymap."
  (set-default symbol new)
  (setq bqn-interactive-mode-map (bqn--make-interactive-mode-map)))

(defcustom bqn-interactive-mode-map-prefix "H-"
  "The keymap prefix for ‘bqn-interactive-mode-map’ used both to store the new value
using ‘set-create’ and to update ‘bqn-interactive-mode-map’ using
‘bqn--make-interactive-mode-map’. Kill and re-start your interactive APL
buffers to reflect the change."
  :type 'string
  :group 'bqn
  :set 'bqn--set-interactive-mode-map-prefix)

(defvar bqn-interactive-mode-map (bqn--make-interactive-mode-map)
  "The keymap for ‘bqn-interactive-mode'.")

(define-derived-mode bqn-interactive-mode comint-mode "BQN/Comint"
  "Major mode for interacting with BQN."
  :group 'bqn
  (use-local-map bqn-interactive-mode-map)
  (setq-local font-lock-defaults bqn--token-syntax-types)

  (setq comint-prompt-regexp "^\\(      \\)\\|\\(\\[[0-9]+\\] \\)")
  (setq font-lock-defaults '(nil t)))

(defun bqn-open-customise ()
  "Open the customisation editor for the bqn customisation group."
  (interactive)
  (customize-group 'bqn t))

(defun bqn-switch-to-interactive ()
  "Switch to the BQN interaction buffer if it has been started."
  (interactive)
  (let ((buffer (process-buffer (bqn-interactive-ensure-session))))
    (pop-to-buffer buffer)
    (goto-char (point-max))))

(provide 'bqn-interactive)
