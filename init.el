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

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; cua mode
(cua-mode)

;; gets rid of all the silly auxiliary files
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/aux/\\1" t)))
(setq auto-save-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/aux/\\1" t)))
(setq backup-directory-alist
      '((".*" . "~/.emacs.d/aux/")))

(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d.%m.%Y")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "fr_CA"))
    (insert (format-time-string format))))

;; undo stuff
(message system-time-locale)
(use-package undo-tree
  :bind
  ("C-z" . 'undo-tree-undo)
  ("C-y" . 'undo-tree-redo))

(setq undo-tree-enable-undo-in-region nil)
(setq undo-tree-auto-save-history nil)
(global-undo-tree-mode)

(use-package minimap
  :config
  (setq minimap-window-location 'right)
  (setq minimap-width-fraction 0.05)
  (setq minimap-minimum-width 20))

;; Prevent undo tree files from polluting your git repo
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(setq undo-limit 80000)
(setq undo-strong-limit 120000)
(setq undo-outer-limit 1200000)

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

(use-package flx)

(use-package cfdg-mode)

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


(use-package platformio-mode)

(add-hook 'c++-mode-hook (lambda ()
                           (platformio-conditionally-enable)))
(defun adelrune/unfuck-jan-lawa-lili ()
  (interactive)
  (progn (call-interactively 'mark-whole-buffer)
         (call-interactively 'indent-region)
         (replace-string "   <br/>" "<p data-nanpa-toki=\"\">")
         (beginning-of-buffer)
         (replace-string "<p>" "<p data-nanpa-toki=\"\">")
         (beginning-of-buffer)
         (replace-string "<br/>" "</p>")
         (beginning-of-buffer)
         (call-interactively 'mark-whole-buffer)
         (call-interactively 'indent-region)))

(use-package arduino-mode)

(use-package xonsh-mode)

(use-package processing-mode)

(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)

(use-package which-key :config
  (which-key-mode))

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


(use-package cape
   :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

(setq-local completion-at-point-functions
  (mapcar #'cape-company-to-capf
    (list #'company-files #'company-keywords #'company-dabbrev)))

(use-package sublimity)

(sublimity-mode 1)
(setq scroll-conservatively 10000)

(use-package multiple-cursors)
(require 'multiple-cursors-core)
(require 'mc-cycle-cursors)

(defvar multiple-cursors-mode-enabled-hook nil)
(defvar multiple-cursors-mode-disabled-hook nil)

(defun on-mc ()
  (setq-default cursor-type 'block)
  (global-corfu-mode -1))
(defun off-mc ()
  (setq adelrune/mc-mark-next-marked-a-symbol nil)
  (global-corfu-mode 1)
  (setq-default cursor-type 'bar))

( add-hook 'multiple-cursors-mode-enabled-hook #'on-mc)
( add-hook 'multiple-cursors-mode-disabled-hook #'off-mc)

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

(global-set-key (kbd "<escape>") 'adelrune/kb-escape-quit)
(bind-key* "C-g" 'goto-line)
(bind-key* "C-S-s" 'adelrune/save-as)
(global-set-key (kbd "C-s") 'adelrune/save)
(bind-key* "C-q" 'adelrune/begin-line)
(bind-key* "C-a" 'mark-whole-buffer)
(bind-key* "C-w" 'kill-current-buffer)
(bind-key* "M-n" 'next-buffer)
(bind-key* "M-p" 'previous-buffer)
(bind-key* "S-<prior>"  '(lambda () (interactive) (scroll-down 2)))
(bind-key* "S-<next>"  '(lambda () (interactive) (scroll-up 2)))

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

;; https://gist.github.com/ptrv/7d728b7dc4f2113ef915dba3b66f052c
(defhydra hydra-smerge
  (:color red :hint nil
          :pre (smerge-mode 1))
  "
^Move^ ^Keep^ ^Diff^ ^Pair^
------------------------------------------------------
  _<up>_: next _b_ase _R_efine _<_: base-mine
_<down>_: prev _m_ine _E_diff _=_: mine-other
^ ^ _o_ther _C_ombine _>_: base-other
^ ^ _a_ll _r_esolve
_l_ : recenter
_q_uit _RET_: current
"
  ("RET" smerge-keep-current)
  ("C" smerge-combine-with-next)
  ("E" smerge-ediff)
  ("R" smerge-refine)
  ("a" smerge-keep-all)
  ("b" smerge-keep-base)
  ("m" smerge-keep-mine)
  ("<up>" smerge-next)
  ("o" smerge-keep-other)
  ("<down>" smerge-prev)
  ("r" smerge-resolve)
  ("<" smerge-diff-base-mine)
  ("=" smerge-diff-mine-other)
  (">" smerge-diff-base-other)
  ("l" adelrune/recenter-top-bottom)
  ("q" nil :color blue))

(bind-key* "C-S-m" 'hydra-smerge/body)

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
   ("C-<dead-cedilla>" . helm-buffers-list)
   (:map helm-find-files-map ("<tab>" . helm-execute-persistent-action)
   ))
  :config
  (setq-default helm-M-x-fuzzy-match t)
  ;; wtf helm pingin random machines because of stupid ffap defaults ?? stop that please
  (setq ffap-machine-p-known 'reject)
  (progn
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

(defun adelrune/good-indent-rigidly ()
  (interactive)
  (if (use-region-p)
      (let ((init-region-start (region-beginning)) (init-region-end (region-end)))
        (progn
          (deactivate-mark)
          (goto-char init-region-start)
          (beginning-of-line)
          (set-mark (point))
          (goto-char init-region-end)
          (call-interactively 'indent-rigidly)))))

(bind-key* "C-x <tab>" 'adelrune/good-indent-rigidly)

(defun adelrune/gimme-gimme-a-region-value-after-midnight ()
  (substring-no-properties (buffer-substring (region-beginning) (region-end))))

(defun adelrune/counsel-rg-take-the-right-thing-plz ()
  (interactive)
  (if (use-region-p)
      (counsel-rg (adelrune/gimme-gimme-a-region-value-after-midnight))
    (counsel-rg(thing-at-point 'symbol))))

(use-package counsel
  :bind
  ("C-S-f" . adelrune/counsel-rg-take-the-right-thing-plz))

(defun adelrune/swiper-with-region ()
  (interactive)
  (if (use-region-p)
      (swiper (adelrune/gimme-gimme-a-region-value-after-midnight))
    (swiper)))

(use-package swiper
    :bind
  ("C-f" . adelrune/swiper-with-region))

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
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(define-key yas-minor-mode-map [tab] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(use-package projectile
  :ensure t
  :bind
  ("C-<dead-circumflex>" . projectile-switch-to-buffer)
  :init
  (projectile-mode +1))

(use-package helm-projectile
  :config
  (helm-projectile-on)
  :bind
  ("C-p" . helm-projectile-find-file))

(use-package helm-flx)
(helm-flx-mode 1)

(use-package nerd-icons)

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
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'web-mode-hook  'flycheck-mode)

(use-package flycheck
  :ensure t
  :init
  :config
  (flycheck-add-mode 'javascript-standard 'web-mode))

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
          (readme-paths (mapcar (lambda (x)
                                 (expand-file-name x project-root))
                               '("README.md" "readme.md" "README.rst" "readme.rst"))))
    (if init-files
        (progn
          (mapcar (lambda (filename)
                    (find-file (expand-file-name filename project-root)))
                  init-files))
      (dolist (readme-path readme-paths)
        (if (file-exists-p readme-path)
            (find-file readme-path))))))

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

(defun adelrune/make-emmet-work-in-web-mode ()
  (interactive)
  (if (equal major-mode 'mhtml-mode)
    (emmet-expand-yas)
    (yas-expand)))

(define-key cua-global-keymap (kbd "<C-return>") 'adelrune/make-emmet-work-in-web-mode)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-solarized-dark-high-contrast t))

(defun minibuffer-bg ()
  (let ((darkened-bg (color-darken-name (face-attribute 'default :background) 2) ))
         (set (make-local-variable 'face-remapping-alist)
              (list (list 'default :background darkened-bg)))))

(add-hook 'minibuffer-setup-hook 'minibuffer-bg)

(add-to-list 'initial-frame-alist '(font . "Fantasque Sans Mono-14"))
(add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono-14"))
;; (add-to-list 'initial-frame-alist '(font . "Fira Code-12"))
;; (add-to-list 'default-frame-alist '(font . "Fira Code-12"))

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
   (setq highlight-symbol-idle-delay 0.4)
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

(use-package orderless
  :ensure t
  :custom
  (completion-category-overrides '((file (styles basic partial-completion))))
  :init
  (setq completion-styles '(orderless basic))
  (setq orderless-matching-styles '(orderless-flex)))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.15)
  :bind
  (:map corfu-map
        ("RET" . nil)
        ("<escape>" . corfu-quit)
        ("C-s" . (lambda() (interactive)
                   (progn
                     (adelrune/save)
                     (call-interactively 'corfu-quit)))))
  :init
  (setq completion-styles '(orderless basic))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster) (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)
  (global-corfu-mode))

(global-unset-key (kbd "M-c"))
(bind-key* "C-M-c" 'adelrune/good-comment)
(bind-key* "M-c" 'adelrune/good-comment)

;; All of this shit is to simulate sublime's way of dealing with shift
(global-subword-mode 1)
;; Higher order function would be nice but I can't be bothered to learn elisp
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


(defun blamer-callback-show-commit-diff (commit-info)
  (interactive)
  (let ((commit-hash (plist-get commit-info :commit-hash)))
    (when commit-hash
      (magit-show-commit commit-hash))))

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
  (setq blamer-bindings '(("<mouse-1>" . blamer-callback-show-commit-diff))))

(global-blamer-mode 1)
(setq blamer-max-commit-message-length 100)

;; https://superuser.com/questions/306272/disable-emacs-auto-indentation-for-javascript-mode-completely
(defun fix-js-indendation-mode-hook ()
  (local-set-key (kbd "RET") '(lambda () (interactive) (newline 1))))
(add-hook 'js-mode-hook 'fix-js-indendation-mode-hook)
(add-hook 'web-mode-hook 'fix-js-indendation-mode-hook)

(defun adelrune/new-file ()
                  (interactive)
                  (find-file (string-join (list (projectile-project-p) "Nouveau document " (number-to-string (random 999999))))))

(bind-key* "C-n" 'adelrune/new-file)

(use-package cython-mode)

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

;; region highlight extends only to the end of characters
(set-face-attribute 'region nil :extend nil)

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
   '("2f8eadc12bf60b581674a41ddc319a40ed373dd4a7c577933acaff15d2bf7cc6"
     default))
 '(package-selected-packages nil)
 '(warning-suppress-log-types '((use-package))))
(put 'scroll-left 'disabled nil)
