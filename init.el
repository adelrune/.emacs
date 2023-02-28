(require 'package)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(setq desktop-path '("~/"))
(desktop-save-mode 1)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  ;; only fetch the archives if you don't have use-package installed
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(load-library "use-package")
;; (package-refresh-contents)
(setq use-package-always-ensure t)
(setq tls-checktrust nil)
(setq gnutls-verify-error nil)

;; region highlight extends only to the end of characters
(set-face-attribute 'region nil :extend nil)

;; cua mode
(cua-mode)

(use-package rainbow-delimiters)

;; gets rid of all the silly auxiliary files
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/aux/\\1" t)))
(setq auto-save-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/aux/\\1" t)))
(setq backup-directory-alist
      '((".*" . "~/.emacs.d/aux/")))


;; undo stuff

(use-package undo-tree
  :bind
  ("C-z" . 'undo-tree-undo)
  ("C-y" . 'undo-tree-redo))

(setq undo-tree-enable-undo-in-region nil)
(setq undo-tree-enable-undo-in-region nil)
(global-undo-tree-mode)

;; Prevent undo tree files from polluting your git repo
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(setq undo-limit 800000)
(setq undo-strong-limit 12000000)
(setq undo-outer-limit 120000000)

(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(use-package bind-key)

;; expand region package with function that do not leave a sticky mark
(defun adelrune/expand-dong ()
  (interactive)
  (progn
    (er/expand-region 1)
    (setq transient-mark-mode (cons 'only transient-mark-mode))
    ))

(use-package expand-region
  :bind ("C-=" . 'adelrune/expand-dong))


(defun adelrune/company-manual-begin-fuzzy ()
  (interactive)
  (progn
    (company-fuzzy-mode t)
    (company-manual-begin)
    (company-fuzzy-mode 0)
    ))

;; thanks https://github.com/radian-software/radian/blob/223abc524f693504af6ebbc70ad2d84d9a6e2d1b/radian-emacs/radian-autocomplete.el#L6-L182
(use-package company
  :demand t
  :bind (;; Replace `completion-at-point' and `complete-symbol' with
         ;; `company-manual-begin'. You might think this could be put
         ;; in the `:bind*' declaration below, but it seems that
         ;; `bind-key*' does not work with remappings.
         ([remap completion-at-point] . company-manual-begin)
         ([remap complete-symbol] . company-manual-begin)

         ;; The following are keybindings that take effect whenever
         ;; the completions menu is visible, even if the user has not
         ;; explicitly interacted with Company.

         :map company-active-map

         ;; Make TAB always complete the current selection. Note that
         ;; <tab> is for windowed Emacs and TAB is for terminal Emacs.
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)

         ;; Prevent SPC from ever triggering a completion.
         ("SPC" . nil)
         ;; keep C-s as save
         ("C-s" . nil)

         ;; The following are keybindings that only take effect if the
         ;; user has explicitly interacted with Company.

         :map company-active-map
         :filter (company-explicit-action-p)

         ;; Make RET trigger a completion if and only if the user has
         ;; explicitly interacted with Company. Note that <return> is
         ;; for windowed Emacs and RET is for terminal Emacs.
         ("<return>" . company-complete-selection)
         ("RET" . company-complete-selection)

         ;; We then do the same for the up and down arrows. Note that
         ;; we use `company-select-previous' instead of
         ;; `company-select-previous-or-abort'. I think the former
         ;; makes more sense since the general idea of this `company'
         ;; configuration is to decide whether or not to steal
         ;; keypresses based on whether the user has explicitly
         ;; interacted with `company', not based on the number of
         ;; candidates.

         ("<up>" . company-select-previous)
         ("<down>" . company-select-next))

  :bind* (;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. Here we
          ;; make sure that no minor modes override this keybinding.
          ("M-TAB" . adelrune/company-manual-begin-fuzzy))

  :diminish company-mode
  :config

  ;; stops company from downcasing in comments and plain text
  (setq company-dabbrev-downcase nil)

  ;; Turn on Company everywhere.
  (global-company-mode 1)

  ;; Show completions instantly, rather than after half a second.
  (setq company-idle-delay 0.2)

  ;; Show completions after typing a single character, rather than
  ;; after typing three characters.
  (setq company-minimum-prefix-length 1)

  ;; Show a maximum of 10 suggestions. This is the default but I think
  ;; it's best to be explicit.
  (setq company-tooltip-limit 10)

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  ;; (setq company-require-match #'company-explicit-action-p)
  (setq company-require-match nil)
  ;; Company appears to override our settings in `company-active-map'
  ;; based on `company-auto-complete-chars'. Turning it off ensures we
  ;; have full control.
  (setq company-auto-complete-chars nil))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))


(use-package flx)
(use-package company-fuzzy
  :init
  (setq company-fuzzy-sorting-backend 'flx
        company-fuzzy-prefix-on-top nil
        ))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-vcs-max-length 50))

(use-package minions
  :init
  (minions-mode 1))


(use-package nim-mode)

(use-package cmake-mode)

(use-package google-c-style)

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)



(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(defun adelrune/leave-c++-mode ()
  (when (eq major-mode 'c++-mode)
      (setq company-minimum-prefix-length 1)))


(defun adelrune/enter-c++-mode-function ()
      (setq company-minimum-prefix-length 4))


(add-hook 'change-major-mode-hook 'adelrune/leave-c++-mode)
(add-hook 'c++-mode-hook 'adelrune/enter-c++-mode-function)

(use-package arduino-mode)

(use-package gdscript-mode)

(use-package processing-mode)

;; (use-package jedi-core
;;   :config
;;   (setq jedi:environment-virtualenv (list "virtualenv" "--system-site-package" "--quiet" "--python=python3")))

;; (add-to-list 'auto-mode-alist '("\\.pyde\\'" . python-mode))

;; (use-package vterm
    ;; :ensure t)

(use-package multi-vterm :ensure t)
(define-key vterm-mode-map (kbd "C-x") #'vterm-send-C-x)
(define-key vterm-mode-map (kbd "C-l") #'vterm-send-C-l)


(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)


(use-package xonsh-mode)

;; (use-package company-jedi)
;; (add-to-list 'company-backends 'company-jedi)

(use-package eglot)

(use-package lsp-mode)

(defun m/projectile-project-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'my/projectile root))))

(cl-defmethod project-root ((pr (head my/projectile)))
  (cdr pr))

(cl-defmethod project-files ((pr (head my/projectile)) &optional _dirs)
  (let ((root (cdr pr)))
    (mapcar
     (lambda (file)
       (concat root file))
     (projectile-project-files root))))

(cl-defmethod project-ignores ((pr (head my/projectile)) _dir)
  (let ((default-directory (cdr pr)))
    (projectile-patterns-to-ignore)))

(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'm/projectile-project-find-function))


;; (use-package company-tern)
;; (add-to-list 'company-backends 'company-tern)
;; (add-hook 'js2-mode-hook (lambda ()
                           ;; (tern-mode)))
(use-package sublimity)

(sublimity-mode 1)
(setq scroll-conservatively 10000)

(use-package multiple-cursors)
(require 'multiple-cursors-core)
(require 'mc-cycle-cursors)

(defvar multiple-cursors-mode-enabled-hook nil)
(defvar multiple-cursors-mode-disabled-hook nil)

(defun on-mc ()
  (setq-default cursor-type 'block))
(defun off-mc ()
  (setq adelrune/mc-mark-next-marked-a-symbol nil)
  (setq-default cursor-type 'bar))

(add-hook 'multiple-cursors-mode-enabled-hook #'on-mc)
(add-hook 'multiple-cursors-mode-disabled-hook #'off-mc)

(define-key mc/keymap (kbd "<return>") nil)
;; Exit mc mode on <escape>
(define-key mc/keymap (kbd "<escape>") 'multiple-cursors-mode)

(setq adelrune/mc-mark-next-marked-a-symbol nil)

(defun adelrune/mc-mark-next-symbol ()
  (interactive)
  (if mark-active
      (progn
        (if adelrune/mc-mark-next-marked-a-symbol
            (mc/mark-next-symbol-like-this 1)
          (mc/mark-next-like-this 1)
          )
        (while (> (overlay-start (mc/furthest-cursor-after-point)) (window-end)) (mc/cycle-forward)))
    (progn
      (mc--mark-symbol-at-point)
      (setq adelrune/mc-mark-next-marked-a-symbol t)
      (setq transient-mark-mode (cons 'only transient-mark-mode)))))

(bind-key* "C-d" 'adelrune/mc-mark-next-symbol)
(bind-key* "C-S-a" 'mc/mark-all-dwim)


;; thanks https://emacs.stackexchange.com/a/51452 (but heavily modified)
(mc/load-lists)
(add-to-list 'mc/cmds-to-run-once
             'cua--prefix-override-handler)
(add-to-list 'mc/cmds-to-run-for-all
             'cua-copy-region)

(advice-add #'cua-copy-region
            :around (lambda (oldfn &rest args)
                      (if (> (mc/num-cursors) 1)
                          (kill-ring-save 0 0 t)
                        (apply oldfn args))))
;; thanks https://stackoverflow.com/a/40390199
;; don't destroy other windows with keyboard-escape-quit
(defadvice keyboard-escape-quit
  (around keyboard-escape-quit-dont-close-windows activate)
  (let ((buffer-quit-function (lambda () ())))
    ad-do-it))

(defun adelrune/kb-escape-quit ()
  (interactive)
  (progn
    (if multiple-cursors-mode
        (multiple-cursors-mode 0)
      (keyboard-escape-quit))
    ()))

(defun adelrune/save-as ()
  (interactive)
  (progn
    (set-visited-file-name (read-file-name "Save as :"))
    (save-buffer)
    ))

(defun adelrune/save ()
  (interactive)
  (if (string-match-p "Nouveau[[:space:]]document[[:space:]][[:digit:]]+" (buffer-name))
      (adelrune/save-as)
    (save-buffer)))

(defun adelrune/begin-line ()
  (interactive "^")
  (beginning-of-line-text))

(bind-key* "<escape>" 'adelrune/kb-escape-quit)
(bind-key* "C-g" 'goto-line)
(bind-key* "C-S-s" 'adelrune/save-as)
(bind-key* "C-s" 'adelrune/save)
(bind-key* "C-q" 'adelrune/begin-line)
(bind-key* "C-a" 'mark-whole-buffer)
(bind-key* "C-w" 'kill-current-buffer)
(bind-key* "M-n" 'next-buffer)
(bind-key* "M-p" 'previous-buffer)
(bind-key* "S-<prior>"  '(lambda () (interactive) (scroll-down 2)))
(bind-key* "S-<next>"  '(lambda () (interactive) (scroll-up 2)))
(global-unset-key (kbd "M-c"))

(defun adelrune/recenter-top-bottom ()
  (interactive)
  (progn
    (recenter-top-bottom)
    (pulse-momentary-highlight-one-line (point))))
(bind-key* "C-l" 'adelrune/recenter-top-bottom)


(pulse-momentary-highlight-one-line (point))

(defun adelrune/dumb-jump-go-flash ()
  (interactive)
  (progn
    (dumb-jump-go)
    (pulse-momentary-highlight-one-line (point))))

(defun adelrune/dumb-jump-back-flash ()
  (interactive)
  (progn
    (dumb-jump-back)
    (pulse-momentary-highlight-one-line (point))))

(use-package dumb-jump
  :bind ("C-r" . adelrune/dumb-jump-go-flash)
         ("C-S-r" . adelrune/dumb-jump-back-flash)
  :config (setq dumb-jump-selector 'helm)
  :ensure)

(define-prefix-command 'sidebar-keymap)
(global-set-key (kbd "C-k") 'sidebar-keymap)

(use-package git-gutter
  :ensure t
  :when window-system
  :defer t
  :diminish ""
  :bind
  :init
  (add-hook 'prog-mode-hook #'git-gutter-mode)
  (add-hook 'text-mode-hook #'git-gutter-mode)
  :config
  (add-hook 'after-save-hook 'git-gutter)
  (use-package git-gutter-fringe
    :ensure t
    :init
    (require 'git-gutter-fringe)
    (when (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'git-gutter-fr:added
        [224 224 224 224 224 224 224 224 224 224 224 224 224
             224 224 224 224 224 224 224 224 224 224 224 224]
        nil nil 'center)
      (define-fringe-bitmap 'git-gutter-fr:modified
        [224 224 224 224 224 224 224 224 224 224 224 224 224
             224 224 224 224 224 224 224 224 224 224 224 224]
        nil nil 'center)
      (define-fringe-bitmap 'git-gutter-fr:deleted
        [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
        nil nil 'center))))
(customize-set-variable
   'git-gutter:update-interval 2)

(use-package hydra)

(defhydra hydra-git-gutter (global-map "C-S-g")

  "
Git gutter:
  _<up>_ next hunk        _s_tage hunk     _q_uit
  _<down>_ previous hunk    _r_evert hunk
  ^ ^                _p_opup hunk
  _f_irst hunk
  _l_ast hunk        set start _R_evision
"
  ("<down>" git-gutter:next-hunk)
  ("<up>" git-gutter:previous-hunk)
  ("f" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue))

;; thanks snippins1987 https://old.reddit.com/r/emacs/comments/eeyhdz/weekly_tipstricketc_thread/fch1bkv/
(defun adelrune/helm-M-x ()
    (interactive)
    (if (call-interactively 'helm-M-x)
        (let ((cmd (intern (car extended-command-history))))
          (if multiple-cursors-mode
              (if (and cmd
                       (not (memq cmd mc--default-cmds-to-run-once))
                       (not (memq cmd mc/cmds-to-run-once))
                       (or mc/always-run-for-all
                           (memq cmd mc--default-cmds-to-run-for-all)
                           (memq cmd mc/cmds-to-run-for-all)
                           (mc/prompt-for-inclusion-in-whitelist cmd)))
                  (mc/execute-command-for-all-fake-cursors cmd))))))
(use-package async)
(use-package helm
  :ensure t
  :bind
  (("M-x" . adelrune/helm-M-x)
   ("C-S-v" . 'helm-show-kill-ring)
   ("C-o" . helm-find-files)
   ("C-<dead-circumflex>" . helm-buffers-list)
   (:map helm-find-files-map ("<tab>" . helm-execute-persistent-action)
   ))
  :config
  (setq-default helm-M-x-fuzzy-match t)
  ;; wtf helm pingin random machines because of stupid ffap defaults ?? stop that please
  (setq ffap-machine-p-known 'reject)
  (progn
    (require 'helm-config)
    (helm-mode 1)
    (setq helm-quick-update                     t ; do not display invisible candidates
          helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-window-prefer-horizontal-split   t
          helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t)))

(use-package ivy
  :config
  (ivy-mode))

(defun adelrune/gimme-gimme-a-region-value-after-midnight ()
  (substring-no-properties (buffer-substring (region-beginning) (region-end))))

(defun adelrune/counsel-rg-take-the-right-thing-plz (beginning end)
  ; interactive r adds beginning and end of region arguments
  (interactive "r")
  (if (use-region-p)
      (counsel-rg (adelrune/gimme-gimme-a-region-value-after-midnight) )
    (counsel-rg(thing-at-point 'symbol))))

(use-package counsel
  :bind
  ("C-S-f" . adelrune/counsel-rg-take-the-right-thing-plz))

(use-package swiper
    :bind
  ("C-f" . swiper))

;; (require 'beacon)
;; (beacon-mode 1)
;; (beacon-blink)
;; (setq beacon-blink-duration 0.1)
;; (setq beacon-blink-delay 0)
;; (setq beacon-size 200)
(defmacro adelrune/flash-when-scroll (the-thing &rest body)
  `(defun ,the-thing ()
     (interactive)
     (let ((win-start (window-start)) (win-end (window-end)))
         ,@body
         ;; if window moved, flash cursor.
         (if (or (< (window-point) win-start) (> (window-point) win-end))
             (pulse-momentary-highlight-one-line (point))))))

(adelrune/flash-when-scroll adelrune/backward-paragraph (call-interactively 'backward-paragraph))
(adelrune/flash-when-scroll adelrune/forward-paragraph (call-interactively 'forward-paragraph))

(bind-key* "C-<up>" 'adelrune/backward-paragraph)
(bind-key* "C-<down>" 'adelrune/forward-paragraph)

(defun adelrune/avy-goto-char-flash ()
  (interactive)
  (progn
    (call-interactively 'avy-goto-char-2)
    (pulse-momentary-highlight-one-line (point))))

(defmacro gabc/avy-define-do-the-thing-no-move (the-thing &rest body)
  `(defun ,the-thing (pt)
     (save-excursion
       (goto-char pt)
       ,@body)
     (select-window
      (cdr
       (ring-ref avy-ring 0)))
     t))

(defmacro adelrune/avy-define-do-the-thing (the-thing &rest body)
  `(defun ,the-thing (pt)
     (goto-char pt)
     ,@body
     t))

(gabc/avy-define-do-the-thing-no-move adelrune/avy-action-ruthlessly-kill-line (adelrune/ruthlessly-kill-lines))
(gabc/avy-define-do-the-thing-no-move adelrune/avy-action-comment-line (adelrune/good-comment))
(adelrune/avy-define-do-the-thing adelrune/avy-action-dumb-jump (dumb-jump-go))
(gabc/avy-define-do-the-thing-no-move adelrune/avy-action-copy-symbol (kill-new (thing-at-point 'symbol)))
(gabc/avy-define-do-the-thing-no-move adelrune/avy-action-copy-symbol (kill-new (thing-at-point 'symbol)))
(adelrune/avy-define-do-the-thing adelrune/avy-action-counsel-rg-symbol (counsel-rg(thing-at-point 'symbol)))


(use-package avy
  :bind
  ("C-SPC" . adelrune/avy-goto-char-flash)
  ("C-c l" . avy-goto-line)
  :config
  (setq avy-background t)
  ;; redefined for reference
  ;; (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq avy-keys (nconc (number-sequence ?a ?z)
                      (number-sequence ?A ?Z)))
  (setf (alist-get ?\C-\S-K avy-dispatch-alist) 'adelrune/avy-action-ruthlessly-kill-line
        (alist-get ?\C-\M-c avy-dispatch-alist) 'adelrune/avy-action-comment-line
        (alist-get ?\C-c avy-dispatch-alist) 'adelrune/avy-action-copy-symbol
        (alist-get ?\C-r avy-dispatch-alist) 'adelrune/avy-action-dumb-jump
        (alist-get ?\C-\S-f avy-dispatch-alist) 'adelrune/avy-action-counsel-rg-symbol))

(use-package embark)

(use-package racer)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(use-package yasnippet
  :config
  :init
  (yas-global-mode 1)
  )
(use-package yasnippet-snippets)

(define-key yas-minor-mode-map [tab] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(use-package projectile
  :ensure t
  :bind
  ("C-<dead-cedilla>" . projectile-switch-to-buffer)
  :init
  (projectile-mode +1))

(use-package helm-projectile
  :config
  (helm-projectile-on)
  :bind
  ("C-p" . helm-projectile-find-file))

(use-package helm-flx)
(helm-flx-mode 1)

(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file))

(use-package dired-sidebar
  :bind (("C-k C-b" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package ibuffer)

(use-package ibuffer-projectile)

(add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))

(use-package ibuffer-sidebar
  :ensure t
  :bind (("C-x C-b" . ibuffer-sidebar-toggle-sidebar))
  :ensure nil
  :commands (ibuffer-sidebar-toggle-sidebar))

;; (use-package sr-speedbar)

;;(use-package perspective)
;;(persp-mode)

;;(use-package persp-projectile)


;; (use-package persp-mode
;;   :init
;;   (setq persp-autokill-buffer-on-remove 'kill-weak)
;;   :config
;;   (persp-mode 1)
;;   )

;; (use-package persp-mode-projectile-bridge
;;   :config
;;   (add-hook 'persp-mode-projectile-bridge-mode-hook
;;             #'(lambda ()
;;                 (if persp-mode-projectile-bridge-mode
;;                     (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
;;                   (persp-mode-projectile-bridge-kill-perspectives))))
;;   (persp-mode-projectile-bridge-mode 1)
;;  )

(use-package yaml-mode)
(use-package json-mode)

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  )

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'web-mode-hook  'flycheck-mode)

(use-package flycheck
  :ensure t
  :init
  :config
  (flycheck-add-mode 'javascript-standard 'web-mode)
  )
;; haaaaack
;; (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))

;; (add-to-list 'eglot-server-programs '(web-mode "typescript-language-server"))


;; gets rid of stupid emac buffer on first project switch
(defun adelrune/switch-project ()
  (interactive)
  ;; helm-projectile-switch-project doesn't exist before a call to helm-projectile ?!?!?

  (progn
    (helm-projectile-on)
    (helm-projectile-switch-project)))

;; thanks ergoemacs person
(defun adelrune--read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun adelrune/projectile-open-default-file ()
  (interactive)
  (let* ( (project-root (projectile-ensure-project (projectile-project-root)))
          (proj-path (expand-file-name ".projectile" project-root))
          (init-files (if (file-exists-p proj-path) (adelrune--read-lines proj-path) nil))
          (readme-path (expand-file-name "README.md" project-root)))
    (if init-files
        (progn
          (mapcar (lambda (filename)
                    (find-file (expand-file-name filename project-root)))
                  init-files))
      (if (file-exists-p readme-path)
          (find-file readme-path)))))

(setq projectile-switch-project-action 'adelrune/projectile-open-default-file)

(bind-key* "C-M-p" 'adelrune/switch-project)

;; Funky stuff

(defun adelrune/ruthlessly-kill-lines ()
  (interactive)
  (if (use-region-p)
      (let* (
            (min (min (point) (mark)))
            (max (max (point) (mark)))
            (num-lines (count-lines min max))
            (i 0))
        (while (< i num-lines)
          (save-excursion
            (goto-char min)
            (delete-region (line-beginning-position) (+ 1 (line-end-position))))
          (setq i (+ 1 i))))
      (delete-region (line-beginning-position) (+ 1 (line-end-position)))))


(bind-key* "C-S-k" 'adelrune/ruthlessly-kill-lines)

(when (display-graphic-p)
  (scroll-bar-mode 1)
  (tool-bar-mode -1))
;;; This is weird in console but whatever.
(menu-bar-mode -1)
(blink-cursor-mode -1)

(ido-mode t)
(global-cwarn-mode 1)
(show-paren-mode)
(setq show-paren-delay 0)
(column-number-mode)
(defalias 'yes-or-no-p 'y-or-n-p)

(defun adelrune/make-emmet-work-in-web-mode ()
  (interactive)
  (if (equal major-mode 'mhtml-mode)
    (emmet-expand-yas)
    (yas-expand)))
(define-key cua-global-keymap (kbd "<C-return>") 'adelrune/make-emmet-work-in-web-mode)

(use-package monokai-theme)
;; (use-package color-theme-sanityinc-tomorrow)

;; (load-theme 'monokai t)
;; (load-theme 'sanityinc-tomorrow-night t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-laserwave t)
  ;; (load-theme 'doom-spacegrey t)
  ;; (load-theme 'doom-one t)
  (load-theme 'doom-solarized-dark-high-contrast t))

(defun minibuffer-bg ()
  (let ((darkened-bg (color-darken-name (face-attribute 'default :background) 2) ))
         (set (make-local-variable 'face-remapping-alist)
              (list (list 'default :background darkened-bg)))))
(add-hook 'minibuffer-setup-hook 'minibuffer-bg)

(add-to-list 'initial-frame-alist '(font . "Fira Code-12"))
(add-to-list 'default-frame-alist '(font . "Fira Code-12"))

(defvar killed-file-list nil
  "List of recently killed files.")

(defun add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the
`killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-file-list)))

(add-hook 'kill-buffer-hook #'add-file-to-killed-file-list)

(defun reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (when killed-file-list
    (find-file (pop killed-file-list))))
(bind-key* "C-S-t" 'reopen-killed-file)

(setq initial-major-mode (quote text-mode))

(use-package highlight-symbol
  :config
   (setq highlight-symbol-idle-delay 0.1)
   :bind ("C-S-d" . highlight-symbol-next))

(highlight-symbol-mode)
(face-spec-set 'highlight-symbol-face
                  '((((class color) (background dark))
                     (:background "#346"))
                    (((class color) (background light))
                     (:background "gray90"))))
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
;; TODO : fix this.
(defun adelrune/good-comment ()
  (interactive)
  (progn
    (if (and (eq (char-before) 10) (looking-at "[:blank:]*\n"))
        (progn
        (comment-dwim nil))
      (save-excursion (comment-line 1)))))

(bind-key* "C-M-c" 'adelrune/good-comment)
(bind-key* "M-c" 'adelrune/good-comment)

;; All of this shit is to simulate sublime's way of dealing with shift
(global-subword-mode 1)
;;Higher order function would be nice but I can't be bothered to learn elisp
;; TODO : make this part less dumb
(defun adelrune/forward-word ()
  (interactive "^")
  (cond ((eq (char-after) 10) (right-char 1))
        ((looking-at "\\W+\n") (end-of-line))
        (t (forward-word))))

(defun adelrune/forward-symbol ()
  (interactive "^")
  (cond ((eq (char-after) 10) (right-char 1))
        ((looking-at "\\W+\n") (end-of-line))
        (t (forward-symbol 1))))

;; (interactive "^") means "please emacs deal with shift correctly.

(defun adelrune/forward-kill-word ()
  (interactive)
  (cond ((eq (char-after) 10) (delete-forward-char 1))
        ((looking-at "\\W+\n") (delete-region (point) (line-end-position)))
        (t (let ((final-char-point nil))
             (save-excursion (forward-word)
                             (setq final-char-point (point)))
             (delete-region (point) final-char-point)))))

(defun adelrune/forward-kill-symbol ()
  (interactive)
  (cond ((eq (char-after) 10) (delete-forward-char 1))
        ((looking-at "\\W+\n") (delete-region (point) (line-end-position)))
        (t (let ((final-char-point nil))
             (save-excursion (forward-symbol 1)
                             (setq final-char-point (point)))
             (delete-region (point) final-char-point)))))

(defun adelrune/backward-word ()
  (interactive "^")
  (let
      ((crossed-a-line nil)
       (old-linum (line-number-at-pos)))
    (if (eq (char-before) 10)
        (left-char 1)
      (save-excursion
        (progn
          (backward-word)
          (setq crossed-a-line (< (line-number-at-pos) old-linum))))
      (if crossed-a-line
          (beginning-of-line)
        (backward-word)))))

(defun adelrune/backward-symbol ()
  (interactive "^")
  (let
      ((crossed-a-line nil)
       (old-linum (line-number-at-pos)))
    (if (eq (char-before) 10)
        (left-char 1)
      (save-excursion
        (progn
          (forward-symbol -1)
          (setq crossed-a-line (< (line-number-at-pos) old-linum))))
      (if crossed-a-line
          (beginning-of-line)
        (forward-symbol -1)))))

(defun adelrune/backward-kill-word ()
  (interactive)
  (let
      ((crossed-a-line nil)
       (old-linum (line-number-at-pos))
       (final-char-point nil))
    (if (eq (char-before) 10)
        (delete-backward-char 1)
      (save-excursion
        (progn
          (backward-word)
          (setq final-char-point (point))
          (setq crossed-a-line (< (line-number-at-pos) old-linum))))
      (if crossed-a-line
          (delete-region (point) (line-beginning-position))
        (delete-region (point) final-char-point)
        ))))
(setq-default cursor-type 'bar)
(defun adelrune/backward-kill-symbol ()
  (interactive)
  (let
      ((crossed-a-line nil)
       (old-linum (line-number-at-pos))
       (final-char-point nil))
    (if (eq (char-before) 10)
        (delete-backward-char 1)
      (save-excursion
        (progn
          (forward-symbol -1)
          (setq final-char-point (point))
          (setq crossed-a-line (< (line-number-at-pos) old-linum))))
      (if crossed-a-line
          (delete-region (point) (line-beginning-position))
        (delete-region (point) final-char-point)
        ))))
  ;; (cond ((eq (char-before) 10) (left-char 1))
  ;;    ((looking-back "\n\s-*\\W+" 250) (progn (message "%s" (char-after)) (beginning-of-line)))
  ;;    (t (backward-word))))
(bind-key* "C-<backspace>" 'adelrune/backward-kill-symbol)
(bind-key* "C-<right>" 'adelrune/forward-symbol)
(bind-key* "C-<left>" 'adelrune/backward-symbol)
(bind-key* "C-<delete>" 'adelrune/forward-kill-symbol)
(bind-key* "M-<backspace>" 'adelrune/backward-kill-word)
(bind-key* "M-<right>" 'adelrune/forward-word)
(bind-key* "M-<left>" 'adelrune/backward-word)
(bind-key* "M-<delete>" 'adelrune/forward-kill-word)
(define-key global-map [(insert)] nil)

(defun adelrune/next-win ()
    (interactive)
  (other-window 1))
(defun adelrune/previous-win ()
    (interactive)
  (other-window -1))
(bind-key* "C-M-<prior>" 'adelrune/next-win)
(bind-key* "C-M-<next>" 'adelrune/previous-win)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package smart-tab
  :init (global-smart-tab-mode 1))


(defun add-emmet-expand-to-smart-tab-completions ()
  ;; Add an entry for current major mode in
  ;; `smart-tab-completion-functions-alist' to use
  ;; `emmet-expand-line'.
  (add-to-list 'smart-tab-completion-functions-alist
               (cons major-mode #'emmet-expand-line)))
(add-emmet-expand-to-smart-tab-completions)
(use-package scad-mode)
(use-package emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'add-emmet-expand-to-smart-tab-completions)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'add-emmet-expand-to-smart-tab-completions)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
(setq emmet-move-cursor-between-quotes t)

;; thanks emacs rocks person
;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

(global-auto-revert-mode)
(setq-default indent-tabs-mode nil)

(use-package visual-regexp)

(use-package visual-regexp-steroids
  :bind ("C-c r" . 'vr/query-replace))

(use-package magit
  :config
  (setq magit-display-buffer-function
        (lambda (buffer)
          (display-buffer
           buffer (if (and (derived-mode-p 'magit-mode)
                           (memq (with-current-buffer buffer major-mode)
                                 '(magit-process-mode
                                   magit-revision-mode
                                   magit-diff-mode
                                   magit-stash-mode
                                   magit-status-mode)))
                      nil
                    '(display-buffer-same-window))))))

(use-package blamer
  :ensure t
  :defer 20
  :custom
  (blamer-idle-time 2)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#686857"
                    :background nil
                    :height 110
                    :italic t)))
  :config
  (global-blamer-mode 1))

(setq blamer-max-commit-message-length 100)

(use-package deadgrep :ensure t
  :bind )

;; thanks https://github.com/Wilfred/deadgrep/issues/66#issuecomment-743785822
(defun pp/deadgrep-view-file ()
  "View result under cursor in other window."
  (interactive)
  (deadgrep-visit-result-other-window)
  (other-window 1))

(use-package deadgrep
  :bind
  ("<f5>" . deadgrep)
  (:map deadgrep-mode-map
              ("v" . pp/deadgrep-view-file)))

;; https://superuser.com/questions/306272/disable-emacs-auto-indentation-for-javascript-mode-completely
(defun fix-js-indendation-mode-hook ()
  (local-set-key (kbd "RET") '(lambda () (interactive) (newline 1))))
(add-hook 'js-mode-hook 'fix-js-indendation-mode-hook)
(add-hook 'web-mode-hook 'fix-js-indendation-mode-hook)

(defun adelrune/new-file ()
                  (interactive)
                  (find-file (string-join (list (projectile-project-p) "Nouveau document " (number-to-string (random 999999))))))

(bind-key* "C-n" 'adelrune/new-file)

(use-package csharp-mode)

(use-package rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)

;; inlines move-lines (https://github.com/targzeta/move-lines/blob/master/move-lines.el) because why not
(defun move-lines--internal (n)
  "Moves the current line or, if region is actives, the lines surrounding
region, of N lines. Down if N is positive, up if is negative"

  (let* (text-start
         text-end
         (region-start (point))
         (region-end region-start)
         swap-point-mark
         delete-latest-newline)

    (when (region-active-p)
      (if (> (point) (mark))
          (setq region-start (mark))
        (exchange-point-and-mark)
        (setq swap-point-mark t
              region-end (point))))

    ;; text-end and region-end
    (end-of-line)
    ;; If point !< point-max, this buffers doesn't have the trailing newline.
    (if (< (point) (point-max))
        (forward-char 1)
      (setq delete-latest-newline t)
      (insert-char ?\n))
    (setq text-end (point)
          region-end (- region-end text-end))

    ;; text-start and region-start
    (goto-char region-start)
    (beginning-of-line)
    (setq text-start (point)
          region-start (- region-start text-end))

    ;; STEP 2: cut and paste.
    (let ((text (delete-and-extract-region text-start text-end)))
      (forward-line n)
      ;; If the current-column != 0, I have moved the region at the bottom of a
      ;; buffer doesn't have the trailing newline.
      (when (not (= (current-column) 0))
        (insert-char ?\n)
        (setq delete-latest-newline t))
      (insert text))

    ;; STEP 3: Restoring.
    (forward-char region-end)

    (when delete-latest-newline
      (save-excursion
        (goto-char (point-max))
        (delete-char -1)))

    (when (region-active-p)
      (setq deactivate-mark nil)
      (set-mark (+ (point) (- region-start region-end)))
      (if swap-point-mark
          (exchange-point-and-mark)))))

(defun move-lines-up (n)
  "Moves the current line or, if region is actives, the lines surrounding
region, up by N lines, or 1 line if N is nil."
  (interactive "p")
  (if (eq n nil)
      (setq n 1))
  (move-lines--internal (- n)))

(defun move-lines-down (n)
  "Moves the current line or, if region is actives, the lines surrounding
region, down by N lines, or 1 line if N is nil."
  (interactive "p")
  (if (eq n nil)
      (setq n 1))
  (move-lines--internal n))

(defun move-lines-binding ()
  "Sets the default key binding for moving lines. M-p or M-<up> for moving up
and M-n or M-<down> for moving down."
  (bind-key* "M-p" 'move-lines-up)
  (bind-key* "M-<up>" 'move-lines-up)
  (bind-key* "M-n" 'move-lines-down)
  (bind-key* "M-<down>" 'move-lines-down))
(move-lines-binding)
(put 'upcase-region 'disabled nil)


(defun adelrune/run-leitmotiv ()
  (interactive)
  (progn
    (shell-command (concat
                    "python /home/guillaume/code/leitmotiv/leitmotiv.py "
                    (buffer-file-name)))
    (revert-buffer :ignore-auto :noconfirm)
    (if (not (boundp 'iimage-mode))
        (iimage-mode))
    (iimage-recenter)))

(defun eval-trad-list ()
  (interactive)
  (message-box (shell-command-to-string "python3 /home/guillaume/Documents/partitions/itm/itm_utils.py")))
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2f8eadc12bf60b581674a41ddc319a40ed373dd4a7c577933acaff15d2bf7cc6" default))
 '(package-selected-packages
   '(ws-butler rainbow-delimiters rainbow-delimiter-mode beacon-mode doom-themes blamer multi-vterm ibuffer-projectile ibuffer-sidebar sr-speedbar treemacs yasnippet-snippets yaml-mode xonsh-mode web-mode vterm vscode-icon visual-regexp-steroids use-package undo-tree tabbar sublimity smart-tab scad-mode racer processing-mode persp-mode-projectile-bridge nim-mode multiple-cursors monokai-theme minions magit lsp-mode json-mode js2-mode irony hydra highlight-symbol helm-projectile helm-fuzzier helm-flx google-c-style git-gutter-fringe gdscript-mode expand-region emmet-mode embark eglot dumb-jump doom-modeline dired-sidebar deadgrep csharp-mode counsel company-quickhelp company-jedi company-fuzzy color-theme-sanityinc-tomorrow color-theme cmake-mode avy arduino-mode))
 '(warning-suppress-log-types '((comp))))
