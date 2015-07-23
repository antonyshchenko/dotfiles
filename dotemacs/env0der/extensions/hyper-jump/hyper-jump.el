(require 'robe)
(require 'helm)
(require 'evil)

(defun hyper-jump (arg)
  (interactive "P")
  (cond ((and (equal 'ruby-mode major-mode) robe-running) (robe-jump arg))
        ((equal 'emacs-lisp-mode major-mode) (elisp-slime-nav-find-elisp-thing-at-point (elisp-slime-nav--read-symbol-at-point)))
        (t (helm-etags-select arg))))

(defun hyper-jump/thing-at-point ()
  (interactive)
  (let ((thing (thing-at-point 'symbol 1)))
    (when (and (equal 'ruby-mode major-mode) (equal ":" (substring thing 0 1)))
      ;; in ruby mode try to find appropriate method for ruby symbol at point
      (setq thing (substring thing 1)))
    thing))

(defun hyper-jump/jump-to-definition-via-tags (arg)
  (interactive "P")
  (let ((thing (hyper-jump/thing-at-point)))
    (when (derived-mode-p 'ruby-mode 'cperl-mode)
      ;; quick fix for searching full qualified class/module names in ruby and perl
      (setq thing (first (reverse (split-string thing "::")))))
    (helm-etags+-select-internal (concat "\\_<" thing "\\_>"))))

(define-key evil-normal-state-map (kbd "C-]") 'hyper-jump)

(provide 'hyper-jump)
