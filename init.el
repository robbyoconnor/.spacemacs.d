;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(lua
     clojure
     (dart :variables
           dart-backend 'lsp
           lsp-dart-sdk-dir " ~/flutter/bin/cache/dart-sdk/"
           lsp-enable-on-type-formatting t)
     (php :variables
          php-backend 'lsp)
     windows-scripts
     ;; (tabs :variables
     ;;      tabs-style "bar")

     (crystal :variables
              crystal-backend 'lsp
              crystal-enable-auto-format t)
     (cmake :variables
            cmake-enable-cmake-ide-support t)
     (conda :variables
            conda-anaconda-home "/home/rob/conda")
     restructuredtext
     (vue :variables vue-backend 'lsp)
     helpful
     ietf
     (yang
      :variables
      yang-pyang-rules "ietf")
     dap
     kubernetes
     outshine
     import-js
     prettier
     sphinx
     web-beautify
     transmission
     xclipboard
     (node :variables node-add-modules-path t)
     selectric
     (templates :variables
                templates-private-directory "~/.spacemacs.d/templates")
     coffeescript
     (json :variables
           json-fmt-tool 'prettier
           json-fmt-on-save t
           json-backend 'lsp)
     jsonnet
     julia
     epub
     bibtex
     ;; rebox
     nginx
     twitter
     graphviz
     (ivy :variables
          ivy-enable-advanced-buffer-information t)
     ;; (compleseus :variables compleseus-engine 'vertico)
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
     gnus
     version-control
     (markdown :variables markdown-live-preview-engine 'vmd)
     syntax-checking
     (latex :variables
            latex-backend 'lsp
            latex-enable-auto-fill t)
     (colors :variables
             colors-colorize-identifiers 'all
             colors-enable-nyan-cat-progress-bar t)
     docker
     (ansible :variables ansible-auto-encrypt-decrypt t)
     puppet
     evil-commentary
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     fasd
     finance
     floobits
     xkcd
     autohotkey
     csv
     ;; (c-c++ :variables
     ;;        c-c++-enable-clang-support t
     ;;        c-c++-enable-clang-format-on-save t
     ;;        c-c++-enable-rtags-support t
     ;;        c-c++-enable-google-style t
     ;;        c-c++-enable-google-newline t
     ;;        c-c++-default-mode-for-headers 'c++-mode)
     ;; (clojure  :variables clojure-enable-fancify-symbols t)
     major-modes
     (go :variables
         go-backend 'lsp
         go-use-golangci-lint t
         gofmt-command "goimports"
         go-format-before-save t
         go-use-gometalinter t
         go-use-gocheck-for-testing t
         go-use-test-args "-race -timeout 10s"
         godoc-at-point-function 'godoc-gogetdoc
         go-tab-width 2)
     (haskell :variables
              haskell-enable-ghci-ng-support t
              haskell-enable-shm-support t
              haskell-enable-hindent-style "andrew-gibiansky")
     (html :variables web-fmt-tool 'prettier)

     (java :variables
           java-backend 'lsp)
     (javascript :variables
                 node-add-modules-path t
                 javascript-fmt-on-save t
                 javascript-fmt-tool 'prettier
                 javascript-import-tool 'import-js
                 javascript-backend 'lsp)

     (python :variables
             python-backend 'lsp
             python-lsp-server 'pylsp
             python-formatter 'yapf
             python-format-on-save nil
             python-shell-interpreter "ipython"
             python-enable-yapf-format-on-save t
             python-fill-column 80
             python-auto-set-local-pyenv-version 'on-visit
             python-auto-set-local-pyvenv-virtualenv 'on-visit
             python-sort-imports-on-save t
             python-pipenv-activate t
             python-test-runner '(nose pytest))
     (lsp :variables
          lsp-lens-enable t
          lsp-headerline-breadcrumb-enable t
          lsp-remap-xref-keybindings t
          lsp-ui-doc-enable t
          lsp-ui-sideline-enable t
          lsp-ui-sideline-show-symbol t
          lsp-ui-sideline-ignore-duplicate t)
     racket
     (ruby :variables
           ruby-backend 'lsp
           ruby-test-runner 'rspec
           ruby-version-manager 'rbenv)
     ruby-on-rails
     (rust :variables rust-enable-rustfmt-on-save t)
     ;; (perl5 :variables
     ;;        perl5-backend 'lsp)

     ;; (scala :variables
     ;;        scala-indent:use-javadoc-style t
     ;;        scala-enable-eldoc t
     ;;        scala-auto-insert-asterisk-in-comments t
     ;;        scala-use-unicode-arrows t
     ;;        scala-auto-start-ensime t)
     (shell-scripts :variables shell-scripts-backend 'lsp)
     (restclient :variables
                 restclient-use-org t)
     themes-megapack
     tmux
     vim-empty-lines
     spotify
     pandoc
     vagrant
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     ;; semantic
     deft
     (shell :variables
            close-window-with-teminal t
            shell-default-shell 'vterm
            shell-default-position  'bottom
            shell-default-height 30
            shell-default-term-shell "/bin/zsh")
     typescript
     erc
     chrome
     d
     emoji
     gtags
     prodigy
     evernote
     (org :variables
          org-superstar-bullet-list '("■" "◆" "▲" "▶")
          org-want-todo-bindings t
          org-todo-dependencies-strategy 'naive-auto
          org-enable-bootstrap-support t
          org-enable-github-support t
          org-enable-reveal-js-support t
          org-projectile-file "TODOs.org"
          org-enable-notifications t
          org-start-notification-daemon-on-startup t
          org-enable-org-contacts-support t
          org-enable-org-journal-support t
          org-journal-dir "~/org/journal/"
          org-journal-file-format "%Y-%m-%d"
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%A, %B %d %Y"
          org-journal-time-prefix "* "
          org-journal-time-format ""
          org-enable-sticky-header t
          org-enable-hugo-support t
          ;; org-enable-trello-support t
          org-enable-epub-support t
          org-enable-verb-support t
          org-enable-roam-support t
          org-enable-appear-support t
          org-enable-roam-protocol t)





     search-engine
     yaml
     sql
     nim
     (ipython-notebook :variables ein-backend 'jupyter)
     scheme
     purescript
     sml
     common-lisp
     ranger
     wakatime
     dash
     spell-checking
     jabber
     cscope
     vinegar
     django
     ;; rcirc
     games
     ;; stackexchange
     react
     (vimscript :variables
                vimscript-backend 'lsp)
     geolocation
     idris
     (elm :variables
          elm-reactor-port "3000"          ; default 8000
          elm-reactor-address "0.0.0.0") ; default 127.0.0.1
     elixir
     (typography :variables typography-enable-typographic-editing nil)
     emberjs
     pdf
     imenu-list
     slack
     systemd
     command-log
     (terraform :variables
                terraform-auto-format-on-save t)
     pass
     parinfer
     groovy
     kotlin
     unicode-fonts
     graphql
     streamlink
     (twitch :variables
             twitch-api-username "robbyoconnor"
             twitch-api-oauth-token "j914znalhjha32admfcfevvsl7ql7r")
     hackernews
     lobsters
     djvu
     eww
     (tree-sitter :variables
                  tree-sitter-syntax-highlight-enable t
                  tree-sitter-indent-enable nil
                  tree-sitter-fold-enable t
                  tree-sitter-fold-indicators-enable t)
     (reddit :variables
             reddigg-subs '(spacemacs
                            trymacs_discord))
     (multiple-cursors :variables multiple-cursors-backend 'evil-mc)
     (treemacs))


   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(helm-flycheck marcopolo ob-ipython nvm yasnippet-snippets git-modes)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(ebuild-mode flycheck-mix gitignore-mode gitconfig-mode gitattributes-mode)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t
   dotspacemacs-pretty-docs t))
(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "~/code/emacs/src/emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 15

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style '(vim
                                :variables
                                vim-style-visual-feedback t
                                vim-style-remap-Y-to-y$ t
                                vim-style-retain-visual-state-on-shift t
                                vim-style-visual-line-move-text t
                                vim-style-ex-substitute-global nil)
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner '997

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 25))

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons t

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai
                         doom-city-lights
                         doom-solarized-dark
                         doom-molokai
                         doom-dracula
                         doom-material
                         doom-challenger-deep
                         ;; zenburn
                         spacemacs-dark)


   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("SauceCodePro NF"
                               :size 13
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"



   ;; Major mode leader key is a shortcu
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 350

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state t

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers t


   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'vimish

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   org-roam-v2-ack t

  ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t))

(defun dotspacemacs/user-init ()
  "Initialization for user code:))
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (when window-system
    (if (> (display-pixel-height) 1050)
        (setq-default dotspacemacs-default-font '("Source Code Pro"
                                                  :size 28
                                                  :weight normal
                                                  :width normal
                                                  :powerline-scale 1.1))
      (setq-default dotspacemacs-default-font '("Source Code Pro"
                                                :size 13
                                                :weight normal
                                                :width normal
                                                :powerline-scale 1.1))))

  (set-fontset-font t 'unicode "Symbola" nil 'prepend)
  '(add-hook 'doc-view-mode-hook 'auto-revert-mode))




(defun fetspacemacs/user-load ()
  "Library to load while dumping.
This function is called while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included
in the dump.")


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (setq
   warning-minimum-level :emergency
   warning-minimum-log-level :emergency)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Magit - forge configuration
  ;;
  ;; Set the files that are searched for writing tokens
  ;; by default ~/.authinfo will be used
  ;; and write a token in unencrypted format
  (setq auth-sources '("~/.authinfo.gpg"))
  ;;
  ;; End of Magit - forge configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (require 'window-purpose) ; workaround until https://github.com/bmag/emacs-purpose/issues/158 is fixed
  (require 'semantic/db-file)
  (setq spaceline-org-clock-p t)
  (setq spaceline-org-clock-p t)
  (with-eval-after-load 'org-agenda
    (require 'org-projectile)
    (mapcar '(lambda (file)
               (when (file-exists-p file)
                 (push file org-agenda-files)))
            (org-projectile-todo-files)))
  (setq backup-directory-alist `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (setq python-shell-interpreter 'ipython)
  (setq create-lockfiles nil)
  (setq lsp-auto-guess-root nil)
  (rbenv-use-corresponding)
  (spacemacs/toggle-typographic-substitutions-off)
  (setq-default spacemacs-mode-line-minor-modesp nil
                fancy-battery-last-status t)
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
  (setq python-tab-width 2)
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
        browse-url-generic-program "chromium")
  (setq
   ;; Use another eclimd executable
   eclim-executable "~/eclipse/eclim"
   eclimd-default-workspace "~/code/"
   ;; Whether or not to block emacs until eclimd is ready
   eclimd-wait-for-process nil
   eclim-eclipse-dirs "~/eclipse")

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (setq org-reveal-root "file:///home/rob/reveal.js")
  (setq ansible::vault-password-file ".vault_pass")
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (setq spacemacs--ansible-filename-re
        ".*\\(certbot\.yml|common\.yml|lh\-tomcat\.yml|lh\-ehr\.yml|requirements\.yml|main\.yml\\|site\.yml\\|encrypted\.yml\\|roles/.+\.yml\\|group_vars/.+\\|host_vars/.+|/ansible/.+\\)")
  (defalias 'display-buffer-in-major-side-window 'window--make-major-side-window)
  (setq which-key-side-window-location 'bottom)
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))
  (setq ein:output-area-case-types '(:image/svg+xml :image/png :image/jpeg :text/html :text/plain :application/latex :application/tex :application/javascript))
  ;; (setq flycheck-gometalinter-deadline "45s")
  ;; (setq flycheck-gometalinter-fast t)
  ;; (setq flycheck-gometalinter-test t)
  ;; (setq flycheck-gometalinter-vendor t)
  ;; (setq flycheck-gometalinter-concurrency 4)
  ;; (setq flycheck-gometalinter-disable-all t)
  (setq rst-sphinx-target-parent "/home/rob/sphinx/build")
  (setq rst-sphinx-target-projects
        '(("teleirc" . (html  "/teleirc" nil))))
  (with-eval-after-load 'dockerfile-mode
    (defun dockerfile-indent-line-function ())
    (setq indent-line-function #'dockerfile-indent-line-function))
  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))
  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-key-sequence "jk")
  (global-set-key (kbd "C-c C-g") 'evil-escape)
  (setq-default
   ;; js2-mode
   js2-basic-offset 2
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2)

  (with-eval-after-load 'evil-surround
    (setq-default evil-surround-pairs-alist
                  (push #'(?~ . ("``" . "``")) evil-surround-pairs-alist)))
  (setq flycheck-idle-change-delay 60))










(setq custom-file "~/.spacemacs.d/custom.el")
(load custom-file)
