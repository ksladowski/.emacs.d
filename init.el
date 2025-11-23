;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Straight.el bootstrapping (package.el disabled in early-init)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure straight
(use-package straight
  :custom (straight-use-package-by-default t)  ; Without this we need to add the :straight keyword symbol
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Benchmarking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normally kept commented out unless trying to identify an issue
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Built-In
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  (electric-pair-mode)
  (electric-indent-mode)
  (column-number-mode)
  (global-display-line-numbers-mode)
  (set-face-attribute 'default nil :font "Jetbrains Mono Nerd Font" :height 100)
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 100)
  ;; Put backup files neatly away instead of littering all over the file system
  (let ((backup-dir "~/.local/share/emacs/backups")
        (auto-saves-dir "~/.local/share/emacs/auto-saves/"))
    (dolist (dir (list backup-dir auto-saves-dir))
      (when (not (file-directory-p dir))
        (make-directory dir t)))
    (setq backup-directory-alist `(("." . ,backup-dir))
          auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
          auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
          tramp-backup-directory-alist `((".*" . ,backup-dir))
          tramp-auto-save-directory auto-saves-dir))
  :hook
  (before-save-hook . delete-trailing-whitespace)
  :custom
  (visible-bell t)
  (help-window-select t)  ; auto switch to help window upon opening
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)  ; TODO Try `cape-dict' as an alternative.
  (read-extended-command-predicate #'command-completion-default-include-p)  ; Hide commands in M-x which do not apply to the current mode
  ;; Stop littering
  (create-lockfiles nil)  ; Disable lockfiles
  (backup-by-copying t)  ; Don't delink hardlinks
  (delete-old-versions t)  ; Clean up the backups
  (version-control t)  ; Use version numbers on backups,
  (kept-new-versions 5)  ; keep some new versions
  (kept-old-versions 2)  ; and some old ones, too
  ;; Spaces not tabs
  (indent-tabs-mode nil)
  (tab-width 4)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages (Alphabetical)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ansi-color
  :config
  (defun my/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my/colorize-compilation-buffer))

(use-package ace-window
  :custom
  (aw-dispatch-always t)
  :general
  ;; Queue "?" key command to be sent. Automatically open help when calling ace-window
  (my/leader-def 'normal
    "w"
    '(lambda () (interactive)
       (setq unread-command-events (listify-key-sequence (kbd "?")))
       (ace-window nil))
    )

(use-package avy
  :general
  (general-define-key
   :states '(normal visual motion)
   :prefix "s"
   "s" 'evil-avy-goto-char-timer
   "j" 'evil-avy-goto-line-below
   "k" 'evil-avy-goto-line-above
   )

(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin t))

(use-package consult
  :general
  (general-nmap "C-x b" 'consult-buffer)
  (general-nmap "/" 'consult-line)
  (:keymaps 'minibuffer-local-map
            "C-." 'embark-act
            "M-." 'embark-dwim)
  (my/leader-def 'normal
    "fb" 'consult-bookmark
    "bb" 'consult-buffer))
;;TODOTODO can i consult key? consult variable?
(use-package consult-projectile)
(use-package consult-flycheck)
;; (use-package consult-lsp)

(use-package consult-yasnippet)
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-quit-at-boundary t)
  (corfu-preview-current nil)
  (corfu-preselect 'prompt))

;; TODO dabbrev w/ corfu

;; TODO orderless w/ corfu

;; TODO cape w/ corfu
;; https://github.com/minad/corfu

;; (use-package dabbrev)
(use-package doom-modeline
  :custom
  (doom-modeline-height 22)
  (doom-modeline-bar-width 5)
  (doom-modeline-hud t)
  (doom-modeline-percent-position '(-3 ""))
  :config
  (doom-modeline-mode 1))

(use-package embark
  :preface
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))
  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))
  :general
  (general-nmap "C-." 'embark-act)
  (general-nmap "M-." 'embark-dwim)
  (my/leader-def 'normal
    "." 'embark-act)
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators
   '(embark-which-key-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

(use-package evil
  :preface
  ;; Custom function for :q
  (defun my/ex-kill-buffer-and-close ()
    (interactive)
    (unless (char-equal (elt (buffer-name) 0) ?*)
      (kill-this-buffer)))
  ;; Custom function for :wq
  (defun my/ex-save-kill-buffer-and-close ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  (evil-want-fine-undo t)
  (evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) ;; Treat C-g like <escape>
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-ex-define-cmd "q[uit]" 'my/ex-kill-buffer-and-close)
  (evil-ex-define-cmd "wq" 'my/ex-save-kill-buffer-and-close)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-goggles
  :after evil
  :config (evil-goggles-mode))

(use-package evil-snipe
  :after evil
  :custom
  (evil-snipe-override-mode t)
  (evil-snipe-smart-case t)
  (evil-snipe-repeat-keys t)
  (evil-snipe-override-evil t)
  (evil-snipe-override-evil-repeat-keys 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :diminish flycheck-mode
  :hook
  ((python-mode . flycheck-mode)
   (lisp-interaction-mode . flycheck-mode))
  :init
  (setq flycheck-check-syntax-automatically '(save new-line)
        flycheck-idle-change-delay 5.0
        flycheck-display-errors-delay 0.9
        flycheck-highlighting-mode 'symbols
        flycheck-indication-mode 'left-fringe
        flycheck-standard-error-navigation t
        flycheck-deferred-syntax-check nil))

;; TODO flycheck hydra

(use-package general
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)
  (general-def "<escape>" 'keyboard-escape-quit) ;; Treat <escape> like C-g
  (general-create-definer my/leader-def
    :keymaps '(normal insert visual emacs)
    :global-prefix "C-c"
    :prefix "SPC")
  (my/leader-def
    "w" 'evil-window-map
    "h" 'help-command
    "`" 'tmm-menubar
    "ff" 'find-file
    "fd" 'dired
    "bb" 'consult-buffer
    "bk" 'kill-this-buffer
    ":" 'execute-extended-command))

(use-package magit
  :defer t
  :general
  (my/leader-def 'normal
    "g" 'magit-status))

(use-package marginalia
  :init (marginalia-mode))

(use-package orderless
  :init (setq completion-styles '(orderless partial-completion)
              completion-category-defaults nil
              completion-category-overrides '((file (styles partial-completion)))))

(use-package projectile
  :init (projectile-mode 1)
  :custom
  (projectile-project-search-path '("~/Projects"))
  (projectile-switch-project-action #'projectile-dired)
  (projectile-enable-caching t)
  (projectile-indexing-method 'hybrid)
  (projectile-track-known-projects-automatically nil)
  :general
  (my/leader-def 'normal
    "p" 'projectile-command-map))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package savehist
  :init (savehist-mode))

(use-package treemacs
  :ensure t
  :defer t
  :general
  (my/leader-def 'normal
    "<tab>" 'treemacs))

(use-package undo-tree
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :ensure t
  :general
  (general-mmap undo-tree-visualizer-mode-map
    "<escape>" 'undo-tree-visualizer-abort
    "h" 'undo-tree-visualize-switch-branch-left
    "j" 'undo-tree-visualize-redo
    "k" 'undo-tree-visualize-undo
    "l" 'undo-tree-visualize-switch-branch-right)
  (my/leader-def 'normal
    "u" 'undo-tree-visualize)
  :init
  (global-undo-tree-mode 1)
  ;; undo tree forces itself off if any bindings related to undo are changed. here we override the function to always return nil
  (with-eval-after-load 'undo-tree
    (defun undo-tree-overridden-undo-bindings-p () nil)))

(use-package which-key
  :custom
  (which-key-idle-delay 1)
  :config
  (which-key-mode))

(use-package vertico
  :init (vertico-mode)
  ;; disable straight for extensions since they are installed with the vertico package. we just need to activate them
  :straight '( vertico :files (:defaults "extensions/*")
               :includes (vertico-buffer
                          vertico-directory
                          vertico-flat
                          vertico-indexed
                          vertico-mouse
                          vertico-quick
                          vertico-repeat
                          vertico-reverse)))

(use-package vertico-directory
  :after vertico
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  (yas-global-mode)
  :general
  (my/leader-def 'normal
    "sn" 'yas-new-snippet
    "se" 'yas-visit-snippet-file
    "ss" 'yas-insert-snippet)
  )
(use-package yasnippet-snippets)

;;;; LSP
;; (use-package lsp-mode
;;   :defer t
;;   :commands (lsp lsp-deferred)
;;   :custom
;;   (lsp-completion-providern :none) ;;use corfu
;;  :general
;;   (my/leader-def 'normal
;;     "ll" 'lsp-keymap)
;;   :hook (python-mode . lsp-deferred)
;;   :hook (java-mode . lsp-deferred))
;;;;; lsp-ui
;; (use-package lsp-ui
;;   :after (lsp-mode)
;;   :config
;;   (setq lsp-ui-sideline-enable t
;; 	    lsp-ui-doc-delay 2
;;         lsp-ui-peek-always-show t)
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :general
;;   (my/leader-def 'normal
;;     "ld" 'lsp-ui-peek-find-definitions
;;     "lf" 'lsp-ui-peek-find-references
;;     "lm" 'lsp-ui-imenu))

;;;;; lsp treemacs
;; (use-package lsp-treemacs
;;   :after (lsp-mode treemacs)
;;   :commands 'lsp-treemacs-errors-list
;;   :general
;;   (my/leader-def 'normal
;;     "ld" 'lsp-ui-peek-find-definitions
;;     "lf" 'lsp-ui-peek-find-references
;;     "le" 'lsp-treemacs-errors-list))
;;;; DAP
;; (use-package dap-mode
;;   :defer t
;;   :after lsp-mode
;;   :custom
;;   (dap-java-terminal 'integratedTerminal)
;;   :config
;;   (dap-auto-configure-mode))
;;;; completion
