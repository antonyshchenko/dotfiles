;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers '(env0der
                                       colors
                                       html
                                       ;; ruby
                                       clojure
                                       dash
                                       (auto-completion :variables auto-completion-use-tab-instead-of-enter t)
                                       git
                                       csharp
                                       emacs-lisp
                                       eyebrowse)

   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(evil-escape flycheck git-gutter)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  (setq frame-resize-pixelwise t)
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Specify the startup banner. If the value is an integer then the
   ;; text banner with the corresponding index is used, if the value is
   ;; `random' then the banner is chosen randomly among the available banners,
   ;; if the value is a string then it must be a path to a .PNG file,
   ;; if the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'random
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(tango-dark)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Menlo"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key ","
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   ;; dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   ruby-version-manager 'rbenv
   ruby-enable-ruby-on-rails-support t
   git-enable-github-support t
   git-magit-status-fullscreen t
   git-gutter-use-fringe nil
   )
  ;; User initialization goes here
  )

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (server-start)
  (global-hl-line-mode -1)
  (blink-cursor-mode t)
  (setq powerline-default-separator nil)
  ;; (spacemacs/toggle-line-numbers)
  ;; (linum-relative-toggle)
  ;; (setq linum-relative-format "%3s ")
  (add-hook 'css-mode-hook 'rainbow-mode)
  ;; rebind some spacemacs bindings
  (use-package helm
    :config
    (progn
      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
      (define-key key-translation-map [f10] (kbd "s-f"))
      (global-set-key (kbd "s-f") 'helm-imenu)
      (define-key helm-map (kbd "C-z")  'helm-select-action)
      (define-key helm-map (kbd "C-w") 'backward-kill-word)
      (define-key helm-map (kbd "M-c") 'helm-buffer-run-kill-persistent)
      (define-key helm-map (kbd "M-m") 'helm-toggle-visible-mark)
      (define-key helm-buffer-map (kbd "M-m") 'helm-toggle-visible-mark)
      (setq helm-echo-input-in-header-line nil) ;; disable temporarely because of glitches in terminal
      ))

  (global-set-key (kbd "s-/") 'spacemacs/helm-projectile-smart-do-search)
  (global-set-key (kbd "C-s") 'evil-search-forward)
  (evil-leader/set-key "bk" 'spacemacs/kill-this-buffer)
  (evil-leader/set-key "bd" 'env0der/delete-current-buffer-file)

  (define-key evil-normal-state-map "H" "^")
  (define-key evil-normal-state-map "L" "$")

  ;; (define-key evil-insert-state-map (kbd "<tab>") 'evil-normal-state)
  (evil-leader/set-key "pR" 'ctags-update)

  (use-package git-commit
    :config
    (progn
      ;; show magit status after finishing commit
      (define-key git-commit-mode-map (kbd "C-c C-c") (lambda(force)
                                                        (interactive "P")
                                                        (with-editor-finish force)
                                                        (magit-status)))))

  (evil-leader/set-key "tt" 'rspec-toggle-spec-and-target)
  (evil-leader/set-key "te" 'rspec-toggle-spec-and-target-find-example)
  (evil-leader/set-key "ta" 'rspec-verify-all)
  (evil-leader/set-key "tv" 'rspec-verify)
  (evil-leader/set-key "ts" 'rspec-verify-single)
  (evil-leader/set-key "tm" 'rspec-verify-matching)
  (evil-leader/set-key "tr" 'rspec-rerun)
  (evil-leader/set-key "tf" 'rspec-run-last-failed)
  (evil-leader/set-key "tp" 'rspec-toggle-example-pendingness)
  (evil-leader/set-key-for-mode 'dired-mode "tv" 'rspec-dired-verify)
  (evil-leader/set-key-for-mode 'dired-mode "ts" 'rspec-dired-verify-single)

  (define-key evil-visual-state-map (kbd "TAB") 'align-regexp)

  ;; customize theme
  (set-face-attribute 'spacemacs-emacs-face nil :box nil)
  (set-face-attribute 'spacemacs-evilified-face nil :box nil)
  (set-face-attribute 'spacemacs-helm-navigation-ms-face nil :box nil)
  (set-face-attribute 'spacemacs-ido-navigation-ms-face nil :box nil)
  (set-face-attribute 'spacemacs-iedit-face nil :box nil)
  (set-face-attribute 'spacemacs-iedit-insert-face nil :box nil)
  (set-face-attribute 'spacemacs-insert-face nil :box nil)
  (set-face-attribute 'spacemacs-lisp-face nil :box nil)
  (set-face-attribute 'spacemacs-micro-state-binding-face nil :box nil)
  (set-face-attribute 'spacemacs-micro-state-header-face nil :box nil)
  (set-face-attribute 'spacemacs-mode-line-new-version-lighter-error-face nil :box nil)
  (set-face-attribute 'spacemacs-mode-line-new-version-lighter-success-face nil :box nil)
  (set-face-attribute 'spacemacs-mode-line-new-version-lighter-warning-face nil :box nil)
  (set-face-attribute 'spacemacs-motion-face nil :box nil)
  (set-face-attribute 'spacemacs-normal-face nil :box nil)
  (set-face-attribute 'spacemacs-visual-face nil :box nil)

  ;; fix performance issue with showing related paren in a large buffer
  (show-smartparens-global-mode -1) ;; slow for large files
  (show-paren-mode) ;; this is faster

  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :background "#1d1f21" :foreground "gray100" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :foundry "nil"))))
   '(font-lock-builtin-face ((t (:foreground "#f56e80"))))
   '(font-lock-keyword-face ((t (:foreground "#f56e80"))))
   '(font-lock-comment-face ((t (:foreground "gray43"))))
   '(font-lock-string-face ((t (:foreground "yellow green"))))
   '(font-lock-constant-face ((t (:foreground "#94c9fe"))))
   '(font-lock-type-face ((t (:foreground "#4cd8e6"))))
   '(evil-search-highlight-persist-highlight-face ((t (:background "yellow1" :foreground "black"))))
   '(magit-item-highlight ((t (:background "#212526"))))
   '(magit-diff-add ((t (:background "#00a100"))))
   '(magit-diff-del ((t (:background "#c91700"))))
   '(helm-selection ((t (:foreground "#4396e8"))))
   '(helm-source-header ((t (:foreground "white" :weight bold))))
   '(highlight-symbol-face ((t (:underline t))))
   '(rainbow-delimiters-depth-1-face ((t (:foreground "gray100"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "orange1"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "purple1"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "OrangeRed1"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "OliveDrab3"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "cyan1"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "SteelBlue1"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "salmon"))))
   '(rainbow-delimiters-mismatched-face ((t (:foreground "white" :background "#c91700"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "white" :background "#c91700"))))
   '(bm-persistent-face ((t (:foreground "Black" :background "DarkOrange1"))))
   '(region ((t (:background "gray50"))))
   '(sp-pair-overlay-face ((t (:inherit default))))
   '(sp-wrap-overlay-face ((t nil)))
   '(sp-wrap-tag-overlay-face ((t nil)))
   '(powerline-active1 ((t (:background "#e8e8e8" :foreground "black" :box nil))))
   '(powerline-active2 ((t (:background "#e8e8e8" :foreground "black" :box nil))))
   '(mode-line ((t (:background "#e8e8e8" :foreground "black" :box nil))))
   '(powerline-inactive1 ((t (:background "#545454" :foreground "white" :box nil))))
   '(powerline-inactive2 ((t (:background "#545454" :foreground "white" :box nil))))
   '(mode-line-inactive ((t (:background  "#545454" :foreground "white" :box nil))))
   '(fringe ((t (:background "#1d1f21" :foreground "gray80"))))
   `(trailing-whitespace ((t (:background "#1d1f21"))))))
