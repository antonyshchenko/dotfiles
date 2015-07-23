(defvar env0der-pre-extensions
  '(
    ;; pre extension env0ders go here
    )
  "List of all extensions to load before the packages.")

(defvar env0der-post-extensions
  '(
    evil-plugins
    hyper-jump
    ;; helm-etags+
    )
  "List of all extensions to load after the packages.")

(defun env0der/init-evil-plugins ())

(defun env0der/init-hyper-jump ()
  (require 'hyper-jump))

;; (defun env0der/init-helm-etags+ ()
;;   (require 'helm-etags+)
;;   )
