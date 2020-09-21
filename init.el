(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  ;; only fetch the archives if you don't have use-package installed
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(cua-mode)
(define-key cua-global-keymap [C-return] nil)
(define-key cua-global-keymap (kbd "C-z") nil)
(define-key cua--cua-keys-keymap (kbd "C-z") nil)

(use-package undo-tree
  :bind
  ("C-z" . 'undo-tree-undo)
  ("C-y" . 'undo-tree-redo))
(bind-key* "C-z" 'undo-tree-undo)

(setq undo-tree-enable-undo-in-region nil)

(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(setq undo-limit 800000)
(setq undo-strong-limit 12000000)
(setq undo-outer-limit 120000000)

(require 'bind-key)

(setq tls-checktrust nil)
(setq gnutls-verify-error nil)
(package-initialize)
;; (when (< emacs-major-version 27)
;;   (package-initialize))
;; Install `use-package' if it is not present.

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(load-library "use-package")
;;(package-refresh-contents)
(setq use-package-always-ensure t)
(use-package dired-hacks-utils)
(use-package magit)

(defun adelrune/expand-dong ()
  (interactive)
  (progn
    (er/expand-region 1)
    (setq transient-mark-mode (cons 'only transient-mark-mode))
    ))

(use-package expand-region
  :bind ("C-=" . 'adelrune/expand-dong))

;; (setq python-shell-interpreter "ipython" python-shell-interpreter-args "-i")

;; completion stuff
(use-package company
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.03))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))

;; thanks you random stackoverflow person
(dolist (key '("<return>" "RET"))
  ;; Here we are using an advanced feature of define-key that lets
  ;; us pass an "extended menu item" instead of an interactive
  ;; function. Doing this allows RET to regain its usual
  ;; functionality when the user has not explicitly interacted with
  ;; Company.
  (define-key company-active-map (kbd key)
    `(menu-item nil company-complete
                :filter ,(lambda (cmd)
                           (when (company-explicit-action-p)
                             cmd)))))
(define-key company-active-map (kbd "TAB") #'company-complete-selection)
(define-key company-active-map (kbd "SPC") nil)
(define-key company-active-map (kbd "C-s") nil)

;; Company appears to override the above keymap based on company-auto-complete-chars.
;; Turning it off ensures we have full control.
(setq company-auto-complete-chars nil)

(use-package nim-mode)

(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package arduino-mode)

(use-package gdscript-mode)

(use-package processing-mode)

(use-package jedi-core
  :config
  (setq jedi:environment-virtualenv (list "virtualenv" "--system-site-package" "--quiet" "--python=python3")))

(add-to-list 'auto-mode-alist '("\\.pyde\\'" . python-mode))

(use-package xonsh-mode)

(use-package company-jedi)
(add-to-list 'company-backends 'company-jedi)

(use-package company-tern)
(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)))
(use-package sublimity)
(add-to-list 'initial-frame-alist '(font . "Fira Code-10"))
(add-to-list 'default-frame-alist '(font . "Fira Code-10"))

(sublimity-mode 1)
(setq scroll-conservatively 10000)

(use-package undo-tree
  :bind
  ("C-z" . 'undo-tree-undo)
  ("C-y" . 'undo-tree-redo))
(setq undo-tree-enable-undo-in-region nil)

(add-hook 'write-file-hooks 'delete-trailing-whitespace)

  ;; default in spacemacs is 80000
  (setq undo-limit 800000)

  ;; default in spacemacs is 120000
  (setq undo-strong-limit 12000000)

  ;; default in spacemacs is 12000000
  (setq undo-outer-limit 120000000)
;;sane escape mode
(defun keyboard-escape-quit ()
  "Exit the current \"mode\" (in a generalized sense of the word).
This command can exit an interactive command such as `query-replace',
can clear out a prefix argument or a region,
can get out of the minibuffer or other recursive edit,
cancel the use of the current buffer (for special-purpose buffers),
or go back to just one window (by deleting all but the selected window)."
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
    ((region-active-p)
     (deactivate-mark))
    ((> (minibuffer-depth) 0)
     (abort-recursive-edit))
    (current-prefix-arg
     nil)
    ((> (recursion-depth) 0)
     (exit-recursive-edit))
    (buffer-quit-function
     (funcall buffer-quit-function))
    ((string-match "^ \\*" (buffer-name (current-buffer)))
     (bury-buffer))))

(use-package multiple-cursors)
(require 'multiple-cursors-core)
(require 'mc-cycle-cursors)

(defvar multiple-cursors-mode-enabled-hook nil)
(defvar multiple-cursors-mode-disabled-hook nil)

(defun on-mc ()
  (cua-mode 0)
  (define-key mc/keymap (kbd "C-c") 'kill-ring-save)
  (define-key mc/keymap (kbd "C-v") 'yank)
  (define-key mc/keymap (kbd "C-x") 'kill-region)
  (setq-default cursor-type 'block))
(defun off-mc ()
  (cua-mode 1)
  (setq-default cursor-type 'bar))

(add-hook 'multiple-cursors-mode-enabled-hook #'on-mc)
(add-hook 'multiple-cursors-mode-disabled-hook #'off-mc)

(define-key mc/keymap (kbd "<return>") nil)
;; Exit mc mode on <escape>
(define-key mc/keymap (kbd "<escape>") 'multiple-cursors-mode)
(defun adelrune/mc-mark-next-symbol ()
  (interactive)
  (if mark-active
      (progn
        (mc/mark-next-like-this-symbol 1)
        (while (> (overlay-start (mc/furthest-cursor-after-point)) (window-end)) (mc/cycle-forward)))
    (progn
      (mc--mark-symbol-at-point)
      (setq transient-mark-mode (cons 'only transient-mark-mode)))))

(bind-key* "C-d" 'adelrune/mc-mark-next-symbol)
(bind-key* "C-S-a" 'mc/mark-all-dwim)

(defun adelrune/kb-escape-quit ()
  (interactive)
  (progn
    (if multiple-cursors-mode
        (multiple-cursors-mode 0)
    (keyboard-escape-quit))))

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
(bind-key* "C-w" 'kill-buffer)
(bind-key* "M-n" 'next-buffer)
(bind-key* "M-p" 'previous-buffer)
(bind-key* "C-<prior>" 'previous-buffer)
(bind-key* "C-<next>" 'next-buffer)


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
  ("<up>" git-gutter:next-hunk)
  ("<down>" git-gutter:previous-hunk)
  ("f" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue))

(use-package helm
  :ensure t
  :bind
  (("M-x" . helm-M-x)
   ("C-o" . helm-find-files)
   ("C-S-v" . 'helm-show-kill-ring)
   )
  :config
  (setq-default helm-M-x-fuzzy-match t)
  (progn
    (require 'helm-config)
    (helm-mode 1)
    (setq helm-quick-update                     t ; do not display invisible candidates
          helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t)))

(use-package ivy)
(use-package swiper
    :bind
  ("C-f" . swiper))

(use-package tabbar)
(tabbar-mode 1)
(bind-key* "C-<prior>" 'tabbar-backward-tab)
(bind-key* "C-<next>" 'tabbar-forward-tab)

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

(defun tabbar-move-current-tab-one-place-left ()
  "Move current tab one place left, unless it's already the leftmost."
  (interactive)
  (let* ((bufset (tabbar-current-tabset t))
         (old-bufs (tabbar-tabs bufset))
         (first-buf (car old-bufs))
         (new-bufs (list)))
    (if (string= (buffer-name) (format "%s" (car first-buf)))
        old-bufs
      (setq not-yet-this-buf first-buf)
      (setq old-bufs (cdr old-bufs))
      (while (and
              old-bufs
              (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
        (push not-yet-this-buf new-bufs)
        (setq not-yet-this-buf (car old-bufs))
        (setq old-bufs (cdr old-bufs)))
      (if old-bufs
          (progn
            (push (car old-bufs) new-bufs)
            (push not-yet-this-buf new-bufs)
            (setq new-bufs (reverse new-bufs))
            (setq new-bufs (append new-bufs (cdr old-bufs))))
        (error "Error: current buffer's name was not found in Tabbar's buffer list."))
      (set bufset new-bufs)
      (tabbar-set-template bufset nil)
      (tabbar-display-update))))

(defun tabbar-move-current-tab-one-place-right ()
  "Move current tab one place right, unless it's already the rightmost."
  (interactive)
  (let* ((bufset (tabbar-current-tabset t))
         (old-bufs (tabbar-tabs bufset))
         (first-buf (car old-bufs))
         (new-bufs (list)))
    (while (and
            old-bufs
            (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
      (push (car old-bufs) new-bufs)
      (setq old-bufs (cdr old-bufs)))
    (if old-bufs
        (progn
          (setq the-buffer (car old-bufs))
          (setq old-bufs (cdr old-bufs))
          (if old-bufs
              (push (car old-bufs) new-bufs))
          (push the-buffer new-bufs))
      (error "Error: current buffer's name was not found in Tabbar's buffer list."))
    (setq new-bufs (reverse new-bufs))
    (setq new-bufs (append new-bufs (cdr old-bufs)))
    (set bufset new-bufs)
    (tabbar-set-template bufset nil)
    (tabbar-display-update)))


(bind-key* "C-S-<prior>" 'tabbar-move-current-tab-one-place-left)
(bind-key* "C-S-<next>" 'tabbar-move-current-tab-one-place-right)
(bind-key* "C-w" 'kill-current-buffer)

;; Add a buffer modification state indicator in the tab label, and place a
;; space around the label to make it looks less crowd.c
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value
        (if (and (buffer-modified-p (tabbar-tab-value tab))
                 (buffer-file-name (tabbar-tab-value tab)))
            (concat " + " (concat ad-return-value " "))
          (concat " " (concat ad-return-value " ")))))

;; Called each time the modification state of the buffer changed.
(defun ztl-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))



;; First-change-hook is called BEFORE the change is made.
(defun ztl-on-buffer-modification ()
  (set-buffer-modified-p t)
  (ztl-modification-state-change))
(add-hook 'after-save-hook 'ztl-modification-state-change)

(defun adelrune/tabbar-buffer-groups () ;; customize to show all normal files in one group
  "Returns the name of the tab group names the current buffer belongs to.
    There are two groups: Emacs buffers (those whose name starts with '*', plus
    dired buffers), and the rest.  This works at least with Emacs v24.2 using
    tabbar.el v1.7."
  (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
              ((eq major-mode 'dired-sidebar) "emacs")
              ((eq major-mode 'dired-mode) "emacs")
              (t "user"))))

(setq tabbar-buffer-groups-function 'adelrune/tabbar-buffer-groups)

;aesthetic changes to tabbar mode
(set-face-attribute
 'tabbar-default nil
 :background "gray20"
 :foreground "gray20"
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-unselected nil
 :background "gray30"
 :foreground "white"
 :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute
 'tabbar-selected nil
 :background "gray75"
 :foreground "black"
 :box '(:line-width 5 :color "gray75" :style nil))
(set-face-attribute
 'tabbar-highlight nil
 :background "white"
 :foreground "black"
 :underline nil
 :box '(:line-width 5 :color "white" :style nil))
(set-face-attribute
 'tabbar-button nil
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-separator nil
 :background "gray20"
 :height 0.6)

(defun switch-tab-group (group-name)
  "Switch to a specific tab group."
  (let ((tab-buffer-list (mapcar
          #'(lambda (b)
              (with-current-buffer b
                (list (current-buffer)
                      (buffer-name)
                      (funcall tabbar-buffer-groups-function) )))
               (funcall tabbar-buffer-list-function))))
    (catch 'done
      (mapc
        #'(lambda (group)
          (when (equal group-name (format "%s" (car (car (cdr (cdr group))))))
            (throw 'done (switch-to-buffer (car (cdr group))))))
        tab-buffer-list) )))


;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:update-interval 2 t)
 '(package-selected-packages
   '(gdscript-mode xonsh-mode arduino-mode scad-mode dash-functional dash-functionnal dash spinner company-lsp emacs-w3m w3m csharp-mode nim nim-mode hydra fira-code yasnippet-snippet yasnippet-snippets processing-mode irony racer rust-mode swiper multiple-cursors sublimity markdown-mode dired-hacks-utils dired-hacks magit smooth-scroll smooth-scrolling tabbar git-gutter-fringe tss git-gutter helm projectile vscode-icon dired-sidebar undo-tree color-theme web-mode js2-mode use-package)))
;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))


(add-hook 'first-change-hook 'ztl-on-buffer-modification)

(defun adelrune/avy-goto-char-timer-flash ()
  (interactive)
  (progn
  (avy-goto-char-timer)
  (pulse-momentary-highlight-one-line (point))))

(use-package racer)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(use-package avy
  :bind
  ("C-SPC" . adelrune/avy-goto-char-timer-flash)
  ("C-c l" . avy-goto-line)
  :config
  (setq avy-timeout-seconds 0.3)
  )

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
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package helm-projectile
  :config
  (helm-projectile-on)
  :bind
  ("C-p" . helm-projectile-find-file)
  ("C-S-f" . helm-projectile-grep))


(use-package helm-flx)
(helm-flx-mode 1)

(use-package helm-fuzzier)
(helm-fuzzier-mode 1)


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


;;(use-package perspective)
;;(persp-mode)

;;(use-package persp-projectile)


(use-package persp-mode
  :init
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  :config
  (persp-mode 1)
  )

(use-package persp-mode-projectile-bridge
  :config
  (add-hook 'persp-mode-projectile-bridge-mode-hook
            #'(lambda ()
                (if persp-mode-projectile-bridge-mode
                    (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                  (persp-mode-projectile-bridge-kill-perspectives))))
  (persp-mode-projectile-bridge-mode 1)
  )


;; gets rid of stupid emac buffer on first project switch
(defun adelrune/switch-project ()
  (interactive)
  ;; helm-projectile-switch-project doesn't exist before a call to helm-projectile ?!?!?
  (progn (if (fboundp 'helm-projectile-switch-project)
      (helm-projectile-switch-project)
      (helm-projectile))
         (switch-tab-group "user")))

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
          (find-file readme-path)
        (helm-projectile-find-file)))))

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
  (scroll-bar-mode -1)
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
(use-package color-theme)

(use-package js2-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(use-package web-mode
  :ensure t)

(defun adelrune/make-emmet-work-in-web-mode ()
  (interactive)
  (if (equal major-mode 'mhtml-mode)
    (emmet-expand-yas)
    (yas-expand)))
(bind-key* "<C-return>" 'adelrune/make-emmet-work-in-web-mode)


(use-package monokai-theme)
(load-theme 'monokai t)
;(load-theme 'monokai-alt t)
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
   (setq highlight-symbol-idle-delay 0.01)
   :bind ("C-S-d" . highlight-symbol-next)
  )
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

(defun eval-trad-list ()
  (interactive)
  (message-box (shell-command-to-string "python3 /home/guillaume/Documents/partitions/itm/itm_utils.py")))
(put 'downcase-region 'disabled nil)
