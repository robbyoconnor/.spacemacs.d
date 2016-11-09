
;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory 'emacs-version
   ;; One of `vim', `emacs' or `hybrid'.
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; (default t)
   dotspacemacs-elpa-https nil
   configuration-layer--inhibit-warnings t
   dotspacemacs-large-file-size 350
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 20
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
  dotspacemacs-configuration-layers
  '(
    bibtex
    ;; rebox
    nginx
    twitter
    graphviz
    ivy
    asciidoc
    elfeed
    speed-reading
    swift
    plantuml
    (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-show-snippets-in-popup t
                      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets")
     better-defaults
     emacs-lisp
     (git :variables
        git-magit-status-fullscreen t)
     github
     gnus
     version-control
     (markdown :variables markdown-live-preview-engine 'vmd)
     syntax-checking
     (latex :variables latex-enable-auto-fill t)
     (colors :variables
              colors-colorize-identifiers 'all
              colors-enable-nyan-cat-progress-bar t)
     docker
     ansible
     puppet
     evil-commentary
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     fasd
     finance
     floobits
     xkcd
     autohotkey
     csv
     nlinum
    (c-c++ :variables
           c-c++-enable-clang-support t)
     (clojure :variables clojure-enable-fancify-symbols t)
     extra-langs
     go
     (haskell :variables
                 haskell-enable-ghci-ng-support t
                 haskell-enable-shm-support t
                 haskell-enable-hindent-style "andrew-gibiansky")
     html
     java
     (javascript :variables javascript-disable-tern-port-files t)
     (python :variables
     	python-enable-yapf-format-on-save t
     	python-test-runner '(nose pytest))
     racket
     (ruby :variables
           ruby-version-manager `rvm)
     ruby-on-rails
     (rust :variables rust-enable-rustfmt-on-save t)
     scala
     shell-scripts
     (restclient :variables
                 restclient-use-org t)
     themes-megapack
     tmux
     vim-empty-lines
     spotify
     pandoc
     vagrant
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     semantic
     deft
     (shell :variables
             shell-default-shell 'ansi-term
             shell-default-position  'bottom
             shell-default-height 30
             shell-default-term-shell "/bin/zsh")
;;      typescript
     erc
     chrome
     d
     emoji
     gtags
     prodigy
     evernote
     (org :variables
          org-enable-github-support t
          org-enable-reveal-js-support t
          ;; org-enable-ioslide t
          )
     search-engine
     evil-little-word
     yaml
     sql
     nim
     ipython-notebook
     lua
     scheme
     purescript
     sml
     common-lisp
     ranger
     ;; wakatime
     dash
     spell-checking
     jabber
     cscope
     vinegar
     rcirc
     games
     ;; stackexchange
     react
     php
     vimscript
     geolocation
     idris
     (elm :variables
          elm-reactor-port "3000"          ; default 8000
          elm-reactor-address "0.0.0.0") ; default 127.0.0.1
     elixir
     (typography :variables typography-enable-typographic-editing nil)
     evil-cleverparens
     ;; emberjs
     pdf-tools
     imenu-list
     slack
     systemd
     command-log
     )

   ;; List of additional packages that will be installed wihout being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
  dotspacemacs-additional-packages '(helm-flycheck marcopolo ob-ipython nvm groovy-mode)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to a .PNG file.
   ;; If the value is nil then no banner is displayed.
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner 'doge
   ;; t if you always want to see the changelog at startup
   dotspacemacs-always-show-changelog nil
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '((bookmarks . 5)
                                (recents . 5)
                                (projects . 20)
                                todos agenda)
   ;; List of themes, the first of the list is 7 loaded when spacemacs starts.7
   ;7 ; Press <SPC> T n to cycle to the next theme in the list (works great
   ;7 ; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai
                         zenburn
                         spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key "SPC"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-which-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state t
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode t
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup `trailing

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   )
  ;; User initialization goes here
  `(add-hook 'doc-view-mode-hook 'auto-revert-mode)
  `(add-to-list 'exec-path "~/.cabal/bin/"))

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (setq backup-directory-alist `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (setq create-lockfiles nil)
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby))
  (global-evil-mc-mode)
  (rvm-use-default)
  (spacemacs/toggle-typographic-substitutions-off )
  (setq-default spacemacs-mode-line-minor-modesp nil
                fancy-battery-last-status t)
  (fancy-battery-mode)
  (setq deft-directory "~/Dropbox/notes")
  (setq edit-server-url-major-mode-alist
         '(("github\\.com" . org-mode)))
  (setq powerline-default-separator 'arrow)
  ;; IBuffer
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (setq ibuffer-saved-filter-groups
          (list (cons "Default"
                      (append
                       (mapcar (lambda (it)
                                 (let ((name (file-name-nondirectory
                                              (directory-file-name it))))
                                   `(,name (filename . ,(expand-file-name it)))))
                               projectile-known-projects)
                       `(("Org" (mode . org-mode))
                         ("Dired" (mode . dired-mode))
                         ("IRC" (mode . erc-mode))
                         ("Emacs"
                          (or (name . "\\*Messages\\*")
                              (name . "\\*Compile-Log\\*")
                              (name . "\\*scratch\\*")
                              (name . "\\*spacemacs\\*")
                              (name . "\\*emacs\\*")))
                         ("Magit" (name . "\\*magit"))
                         ("Help" (name . "\\*Help\\*"))
                         ("Helm" (name . "\\*helm"))
                         ("Terminal" (or (name . "\\*ansi-term\\*")
                                         (name . "\\*eshell\\*")))))))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "Default")))
  (add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '("add-hook" . ?) prettify-symbols-alist)
              (push '("defun" . ?𝆑) prettify-symbols-alist)))

  (global-prettify-symbols-mode)
  ;; UTF-8 please
  (setq locale-coding-system    'utf-8) ; pretty
  (set-terminal-coding-system   'utf-8) ; pretty
  (set-keyboard-coding-system   'utf-8) ; pretty
  (set-selection-coding-system  'utf-8) ; pretty
  (prefer-coding-system         'utf-8) ; please
  (set-language-environment     'utf-8) ; with sugar on top

  (defadvice evil-inner-word (around underscore-as-word activate)
    (let ((table (copy-syntax-table (syntax-table))))
      (modify-syntax-entry ?_ "w" table)
      (with-syntax-table table
        ad-do-it)))
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i ")
  (setq vc-follow-symlinks t)
  (setq tab-width 2)
  (setq magit-push-always-verify nil)

  (when (configuration-layer/layer-usedp 'javascript)
    (setq js2-global-externs '("require" "module" "jest" "jasmine"
                               "it" "expect" "describe" "beforeEach"))
    ;; Fix Identation in JS
    (setq js-indent-level                 2
          js2-basic-offset                2
          js-switch-indent-offset         2
          js2-indent-switch-body          2
          js2-strict-missing-semi-warning t)
    (setq python-indent-offset 2)

    (with-eval-after-load 'org
      (org-babel-do-load-languages
       'org-babel-load-languages
       '(( plantuml . t)))
      (setq org-plantuml-jar-path
            (expand-file-name "~/plantuml.jar")))

    (add-hook 'web-mode-hook
              (lambda ()
                (when (equal web-mode-content-type "jsx")
                  ;; enable flycheck
                  (setq web-mode-indent-style 2
                        web-mode-markup-indent-offset 2
                        web-mode-css-indent-offset 2
                        web-mode-code-indent-offset 2)
                  (flycheck-select-checker 'jsxhint-checker)
                  (flycheck-mode))))

    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (if (equal web-mode-content-type "jsx")
          (let ((web-mode-enable-part-face nil))
            ad-do-it)
        ad-do-it)))
  (setq browse-url-browser-function 'browse-url-generic
        engine/browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome-beta")
  (setq
   ;; Use another eclimd executable
   eclim-executable "~/eclipse/eclim"
   eclimd-default-workspace "~/code/"
   ;; Whether or not to block emacs until eclimd is ready
   eclimd-wait-for-process nil
   eclim-eclipse-dirs "~/eclipse")
  (defalias 'display-buffer-in-major-side-window 'window--make-major-side-window)
  (setq which-key-side-window-location 'bottom))
(setq custom-file "~/.spacemacs.d/custom.el")
(load custom-file)
