;;; -*- lexical-binding: t -*-

(require 'cl)

(cl-defun bqn--trim (regexp string &optional (start t) (end t))
  (if (or start end)
    (let ((res string)
          (reg (cond ((and start end)
                      (concat "\\(\\`" regexp "\\)\\|\\(" regexp "\\'\\)"))
                     ((not end)
                      (concat "\\`" regexp))
                     (t
                      (concat regexp "\\'")))))
      (while (string-match reg res)
        (setq res (replace-match "" t t res)))
      res)
    string))

(cl-defun bqn--trim-spaces (string &optional (start t) (end t))
  (bqn--trim "[ \t]" string start end))

(cl-defun bqn--trim-trailing-newline (string)
  (bqn--trim "[\n\r]" string nil t))

(defun bqn--open-new-buffer (name)
  (let ((buffer (get-buffer name)))
    (when buffer
      (delete-windows-on buffer)
      (kill-buffer buffer))
    (get-buffer-create name)))

(defun bqn--string-match-start (string key)
  (and (>= (length string) (length key))
       (string= (subseq string 0 (length key)) key)))

(defun bqn--kbd (definition)
  (if (functionp #'kbd)
      (kbd definition)
    (eval `(kbd ,definition))))

(cl-defmacro bqn--when-let ((var value) &rest body)
  "Evaluate VALUE, if the result is non-nil bind it to VAR and eval BODY.
\(fn (VAR VALUE) &rest BODY)"
  (declare (indent 1))
  `(let ((,var ,value))
     (when ,var ,@body)))

(defun bqn--move-to-line (line)
  "A version of ‘goto-line’ that does the right thing in narrowed buffers."
  (let ((dest-char (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char (point-min))
                       (forward-line (1- line))
                       (point)))))
    (goto-char dest-char)))

(defun bqn--current-line-number (&optional pos)
  (save-restriction
    (widen)
    (save-excursion
      (when pos
        (goto-char pos))
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun bqn--current-line-string ()
  (let ((start (save-excursion (beginning-of-line) (point)))
        (end (save-excursion (end-of-line) (point))))
    (buffer-substring start end)))

(unless (fboundp 'cl-find)
  (defun cl-find (&rest args)
    (apply #'find args)))

(unless (fboundp 'cl-defun)
  (defmacro cl-defun (&rest args)
    `(defun* ,@args)))

(unless (fboundp 'cl-defmacro)
  (defmacro cl-defmacro (&rest args)
    `(defmacro* ,@args)))

(unless (fboundp 'cl-remove-if-not)
  (defun cl-remove-if-not (&rest args)
    (apply #'remove-if-not args)))

(provide 'bqn-util)
