;; increase garbage collection threshold to prevent it from running during startup
;; re-enable it after startup
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;; Prevent runtime compilation of lisp code
;; this is un-needed as packages are compiled on installation
(setq native-comp-deferred-compilation nil)

;; Disable emacs' builtin package management
;; Replaced with straight.el in main init file
(setq package-enable-at-startup nil)


;; Prevent emacs from redrawing repeatedly on startup
;; Should speed startup and more importantly it looks shitty
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;; Hide additional UI elements. If done in init.el they load and then get disabled
;; I'm sure more can be moved here eventually but apparently some things don't work or cause issue
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(set-fringe-mode 5)
(setq inhibit-startup-screen t)

;; silence common lisp deprecation warning
(setq byte-compile-warnings '(cl-functions))
