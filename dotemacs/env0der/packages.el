(defvar env0der-packages
  '(ace-jump-buffer
    helm-projectile
    evil
    evil-nerd-commenter
    evil-matchit
    color-identifiers-mode
    projectile
    cider
    tabbar
    tabbar-ruler
    smartparens
    web-mode
    company
    ruby-mode
    cperl-mode
    mo-git-blame
    js2-mode
    bm
    helm-bm
    web-mode
    helm
    ctags-update
    nlinum
    ag
    browse-at-remote
    osx-clipboard
    avy
    scratch
    rspec-mode))

(defvar env0der-excluded-packages '()
  "List of packages to exclude.")

(defun env0der/init-ace-jump-buffer ()
  (use-package ace-jump-buffer
    :config
    (progn
      (define-key key-translation-map "\033[12;2~" (kbd "s-b"))
      (global-set-key (kbd "s-b") 'ace-jump-buffer))))

(defun env0der/init-helm-projectile ()
  (use-package helm-projectile
    :config
    (progn
      (define-key key-translation-map "\033[13;2~" (kbd "s-o"))
      (global-set-key (kbd "s-o") 'helm-projectile-find-file))))

(defun env0der/init-evil ()
  (use-package evil
    :config
    (progn
      ;;;; Clipboard bypass in evil mode
      (defmacro without-evil-mode (&rest do-this)
        ;; Check if evil-mode is on, and disable it temporarily
        `(let ((evil-mode-is-on (evil-mode?)))
           (if evil-mode-is-on
               (disable-evil-mode))
           (ignore-errors
             ,@do-this)
           (if evil-mode-is-on
               (enable-evil-mode))))

      (defmacro evil-mode? ()
        "Checks if evil-mode is active. Uses Evil's state to check."
        `evil-state)

      (defmacro disable-evil-mode ()
        "Disable evil-mode with visual cues."
        `(progn
           (evil-mode 0)
           (message "Evil mode disabled")))

      (defmacro enable-evil-mode ()
        "Enable evil-mode with visual cues."
        `(progn
           (evil-mode 1)
           (message "Evil mode enabled")))


      ;; delete: text object
      (evil-define-operator evil-destroy (beg end type register yank-handler)
        "Vim's 's' without clipboard."
        (evil-delete beg end type ?_ yank-handler))

      (evil-define-operator evil-destroy-replace (beg end type register yank-handler)
        (evil-destroy beg end type register yank-handler)
        (evil-paste-before 1 register))

      (defadvice evil-paste-after (around env0der/evil-paste-after-and-indent activate)
        "Paste and indent"
        (evil-with-single-undo
          ad-do-it
          (call-interactively 'indent-region)))

      (defadvice evil-paste-before (around env0der/evil-paste-before-and-indent activate)
        "Paste and indent"
        (evil-with-single-undo
          ad-do-it
          (call-interactively 'indent-region)))

      (defun newline-and-indent-interactive ()
        (interactive)
        (newline-and-indent))

      (define-key evil-normal-state-map "r" 'evil-destroy-replace)

      (define-key evil-insert-state-map (kbd "C-j") 'newline-and-indent-interactive)
      (define-key evil-normal-state-map (kbd "C-j") 'newline-and-indent-interactive)
      (define-key evil-normal-state-map (kbd "<RET>") (lambda ()
                                                      (interactive)
                                                      (evil-insert-newline-below)))

      (define-key key-translation-map "\033[11;2~" (kbd "<S-return>"))
      (define-key evil-normal-state-map [(S-return)] (lambda ()
                                                       (interactive)
                                                       (evil-insert-newline-above))))))

(defun env0der/init-evil-nerd-commenter ()
  (use-package evil-nerd-commenter
    :config
    (progn
      (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
      (define-key evil-visual-state-map "gc" 'evilnc-comment-operator))))

(defun env0der/init-color-identifiers-mode ()
  (use-package color-identifiers-mode
    :init
    (progn
      (global-color-identifiers-mode))))

(defun env0der/init-projectile ()
  (use-package projectile
    :config
    (progn
      (setq projectile-project-root-files (cons ".projectile" projectile-project-root-files))

      (defun env0der/projectile-ag ()
        (interactive)
        (let ((search-term (read-from-minibuffer
                                      (projectile-prepend-project-name "Ag search for: ")
                                      (projectile-symbol-at-point))))
          (ag/search search-term (projectile-project-root))))

      ;; focus ag search results window when search is finished
      (add-hook 'compilation-finish-functions (lambda (buf str)
                                                (let ((buffer-contents (with-current-buffer buf (buffer-string))))
                                                  (if (not (null (string-match "mode: ag;" buffer-contents)))
                                                      (select-window (get-buffer-window buf))))))

      (evil-leader/set-key "/" 'env0der/projectile-ag)

      (defun projectile-ag-with-ignore-files ()
        (interactive)
        (let ((search-term (read-from-minibuffer
                            (projectile-prepend-project-name "Ag search for: ")
                            (projectile-symbol-at-point)))
              (ignore-files (read-from-minibuffer
                             (projectile-prepend-project-name "Ag ignore files: "))))
          (setq tmp ag-arguments)
          (setq ag-arguments (cons (format "--ignore=%s" ignore-files) ag-arguments))
          (ag search-term (projectile-project-root))
          (setq ag-arguments tmp)))
      (global-set-key (kbd "s-G") 'projectile-ag-with-ignore-files)

      ;; always use system find command to get project files
      ;; otherwise deleted files will be still shown until they are staged in git
      ;; (defun projectile-get-ext-command ()
      ;;   projectile-generic-command)
      )))

(defun env0der/init-cider ()
  (use-package cider
    :config
    (progn
      (defun cider-reset-system ()
        (interactive)
        (spacemacs//cider-eval-in-repl-no-focus "(user/reset)"))
      (define-key clojure-mode-map (kbd "s-r") 'cider-reset-system))))

(defun env0der/init-tabbar-ruler ()
  (use-package tabbar-ruler
    :init
    (setq tabbar-ruler-global-tabbar t)
    (setq tabbar-ruler-global-ruler nil)
    (setq tabbar-ruler-popup-menu nil)
    (setq tabbar-ruler-popup-toolbar nil)
    (setq tabbar-ruler-popup-scrollbar nil)
    (setq tabbar-ruler-movement-timer-delay 1000000)
    (require 'tabbar-ruler)
    (global-set-key (kbd "M-h") 'tabbar-ruler-backward)
    (global-set-key (kbd "s-{") 'tabbar-ruler-backward)
    (global-set-key (kbd "M-l") 'tabbar-ruler-forward)
    (global-set-key (kbd "s-}") 'tabbar-ruler-forward)
    (global-set-key (kbd "M-w") 'kill-this-buffer)
    (global-set-key (kbd "s-w") 'kill-this-buffer)
    (evil-leader/set-key "bk" 'kill-this-buffer)

    ;; for now just override and hack this function to remove tab with TAGS file from projectile project tabs list
    (defun tabbar-ruler-projectile-tabbar-buffer-groups ()
      (if tabbar-ruler-projectile-tabbar-buffer-group-calc
          (symbol-value 'tabbar-ruler-projectile-tabbar-buffer-group-calc)
        (set (make-local-variable 'tabbar-ruler-projectile-tabbar-buffer-group-calc)

             (cond
              ((or (get-buffer-process (current-buffer)) (memq major-mode '(comint-mode compilation-mode))) '("Term"))
              ((string-equal "*" (substring (buffer-name) 0 1)) '("Misc"))
              ((string-prefix-p "TAGS" (buffer-name)) '("Misc"))
              ((condition-case err
                   (projectile-project-root)
                 (error nil)) (list (projectile-project-name)))
              ((memq major-mode '(emacs-lisp-mode python-mode emacs-lisp-mode c-mode c++-mode makefile-mode lua-mode vala-mode)) '("Coding"))
              ((memq major-mode '(javascript-mode js-mode nxhtml-mode html-mode css-mode)) '("HTML"))
              ((memq major-mode '(org-mode calendar-mode diary-mode)) '("Org"))
              ((memq major-mode '(dired-mode)) '("Dir"))
              (t '("Main"))))
        (symbol-value 'tabbar-ruler-projectile-tabbar-buffer-group-calc)))


    (tabbar-ruler-group-by-projectile-project)

    ;; for some reason this is the only (but ugly) way to force tabbar to use any colors different from the default ones
    (run-with-idle-timer 5 nil (lambda ()
                                 (setq tabbar-background-color "#666666")
                                 (set-face-attribute 'tabbar-selected nil :background "#1d1f21")
                                 (set-face-attribute 'tabbar-selected nil :foreground "white")
                                 (tabbar-install-faces)
                                 (set-face-attribute 'tabbar-default nil :background "#666666")))))

(defun env0der/init-smartparens ()
  (use-package smartparens
    :init
    (sp-local-pair 'web-mode "%" "%"
                   :unless '(sp-in-string-or-word-p)
                   :post-handlers '(
                                    (space-and-space-on-each-side "SPC")
                                    (space-on-each-side "=" "#")
                                    ))

    (defun sp-web-mode-is-code-context (id action context)
      (when (and (eq action 'insert)
                 (not (or (get-text-property (point) 'part-side)
                          (get-text-property (point) 'block-side))))

        t))

    (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))
    ))

(defun env0der/init-web-mode ()
  (use-package web-mode
    :config
    (progn
      ;; don't treat _ as a part of a word
      (modify-syntax-entry ?_ "." web-mode-syntax-table)
      ;; (add-to-list 'web-mode-comment-formats '("ruby" . "#"))
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-indent-style 2)
      (setq web-mode-enable-auto-pairing nil))))

(defun env0der/init-company ()
  (use-package company
    :config
    (progn
      (define-key company-mode-map (kbd "M-j") 'company-select-next)
      (define-key company-mode-map (kbd "M-k") 'company-select-previous)
      (define-key company-active-map (kbd "TAB") 'company-complete)
      (define-key company-active-map (kbd "<tab>") 'company-complete)
      (define-key company-active-map (kbd "C-w") 'backward-kill-word)
      (define-key company-active-map (kbd "C-j") 'newline-and-indent-interactive))))

(defun env0der/init-ruby-mode ()
  ;; better ruby intendation
  (setq ruby-deep-indent-paren nil)
  (setq enh-ruby-deep-indent-paren nil)
  (setq ruby-align-to-stmt-keywords '(def if unless))

  (defadvice env0der/ruby-indent-line (after unindent-closing-paren activate)
    (let ((column (current-column))
          indent offset)
      (save-excursion
        (back-to-indentation)
        (let ((state (syntax-ppss)))
          (setq offset (- column (current-column)))
          (when (and (eq (char-after) ?\))
                     (not (zerop (car state))))
            (goto-char (cadr state))
            (setq indent (current-indentation)))))
      (when indent
        (indent-line-to indent)
        (when (> offset 0) (forward-char offset)))))

  (when (configuration-layer/layer-usedp 'auto-completion)
    (spacemacs|defvar-company-backends ruby-mode)
    (spacemacs|add-company-hook ruby-mode)

    (defun ruby/post-init-company ()
      (spacemacs|add-company-hook ruby-mode))))

(defun env0der/init-cperl-mode ()
  (defalias 'perl-mode 'cperl-mode)
  (setq cperl-indent-level 4
        cperl-close-paren-offset -4
        cperl-continued-statement-offset 4
        cperl-indent-parens-as-block t
        cperl-tab-always-indent t)

  (when (configuration-layer/layer-usedp 'auto-completion)
    (spacemacs|defvar-company-backends cperl-mode)
    (spacemacs|add-company-hook cperl-mode)

    (defun cperl/post-init-company ()
      (spacemacs|add-company-hook cperl-mode))))

(defun env0der/init-mo-git-blame ()
  (use-package mo-git-blame
    :init
    (progn
      (evil-leader/set-key "gb" 'mo-git-blame-current))
    :config
    (progn
      (dolist (state '(normal visual insert))
        (evil-define-key state mo-git-blame-mode-map (kbd "q") 'mo-git-blame-quit)))))

(defun env0der/init-js2-mode ()
  (use-package js2-mode
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
    :config
    (progn
      (add-hook 'js2-init-hook (lambda ()
                                 (setq js2-basic-offset 2)))
      (js2-mode-hide-warnings-and-errors))))

(defun env0der/init-bm ()
  (use-package bm
    :init
    (progn
      (setq bm-restore-repository-on-load t))
    :config
    (progn
      (setq bm-cycle-all-buffers t)
      (setq bm-marker 'bm-marker-left)
      (setq bm-highlight-style 'bm-highlight-only-line)
      (setq-default bm-buffer-persistence t)
      (define-key evil-normal-state-map (kbd "gbb") 'bm-toggle)
      (define-key evil-normal-state-map (kbd "gbn") 'bm-next)
      (define-key evil-normal-state-map (kbd "gbp") 'bm-previous)

      (add-hook' after-init-hook 'bm-repository-load)
      (add-hook 'find-file-hooks 'bm-buffer-restore)
      (add-hook 'kill-buffer-hook 'bm-buffer-save)
      (add-hook 'kill-emacs-hook '(lambda nil
                                    (bm-buffer-save-all)
                                    (bm-repository-save)))
      (add-hook 'after-save-hook 'bm-buffer-save)
      (add-hook 'after-revert-hook 'bm-buffer-restore))))

(defun env0der/init-helm-bm ()
  (use-package helm-bm
    :config
    (progn
      (evil-leader/set-key "hb" 'helm-bm))))

(defun env0der/init-evil-matchit ()
  (use-package evil-matchit
    :config
    (progn
      (global-evil-matchit-mode))))

(defun env0der/init-helm ()
  (use-package helm
    :config
    (progn
      (defun env0der/jump-to-definiton-thing-at-point ()
        (interactive)
        (let ((thing (thing-at-point 'symbol 1)))
          (when (and (equal 'ruby-mode major-mode) (equal ":" (substring thing 0 1)))
            ;; in ruby mode try to find appropriate method for ruby symbol at point
            (setq thing (substring thing 1)))
          thing))

      (defun env0der/jump-to-definition-via-tags (arg)
        (interactive "P")
        (let ((thing (env0der/jump-to-definiton-thing-at-point)))
          (when (derived-mode-p 'ruby-mode 'cperl-mode)
            ;; quick fix for searching full qualified class/module names in ruby and perl
            (setq thing (first (reverse (split-string thing "::")))))
          (helm-etags+-select-internal (concat "\\_<" thing "\\_>"))))

      ;; (define-key evil-normal-state-map (kbd "C-]") 'env0der/jump-to-definition-via-tags)
      (define-key evil-normal-state-map (kbd "C-]") 'helm-etags-select)

      (defun env0der/hyper-jump ()
        (interactive)
        (evil-ace-jump-word-mode)
        (message (thing-at-point 'symbol))
        (helm-etags-select nil))

      (define-key evil-normal-state-map (kbd "C-SPC") 'env0der/hyper-jump))))

(defun env0der/init-ctags-update ()
  (use-package ctags-update
    :config
    (progn
      (setq ctags-update-delay-seconds 0)
      (setq tags-revert-without-query t)
      (global-auto-revert-mode t)
      (setq auto-revert-verbose nil)
      (add-hook 'after-save-hook 'ctags-update)
      (evil-leader/set-key "pR" 'ctags-update))))

(defun env0der/init-nlinum ()
  (use-package nlinum
    :config
    (progn
      (global-nlinum-mode t)
      (setq nlinum-format "%d "))))

(defun env0der/init-ag ()
  (use-package ag))

(defun env0der/init-browse-at-remote ()
  (use-package browse-at-remote))

(defun env0der/init-osx-clipboard ()
  (use-package osx-clipboard
    :config
    (progn
      (osx-clipboard-mode +1)
      (diminish 'osx-clipboard-mode))))

(defun env0der/init-avy ()
  (use-package avy
    :config
    (progn
      (setq avy-background t)
      (setq avy-keys (number-sequence ?a ?z))
      (define-key evil-normal-state-map (kbd "SPC") 'avy-goto-word-or-subword-1))))

(defun env0der/init-scratch ()
  (use-package scratch))

(defun env0der/init-rspec-mode ()
  (use-package rspec-mode
    :config
    (progn
      (add-hook 'after-init-hook 'inf-ruby-switch-setup)

      (defadvice rspec-compile (around rspec-compile-around)
        "Use BASH shell for running the specs because of ZSH issues."
        (let ((shell-file-name "/bin/bash"))
          ad-do-it))

      (ad-activate 'rspec-compile))))
