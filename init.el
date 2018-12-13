(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq tls-checktrust nil)
(setq gnutls-verify-error nil)
(package-initialize)
;; (when (< emacs-major-version 27)
;;   (package-initialize))
;; Install `use-package' if it is not present.

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(load-library "use-package")
(package-refresh-contents)

(use-package magit)

(use-package auto-complete
  :config
  (global-auto-complete-mode t)
  (setq ac-sources '(ac-source-words-in-all-buffer))
  (setq ac-disable-faces nil))



(use-package smooth-scroll
  :config
  (smooth-scroll-mode 1)
  (setq smooth-scroll/vscroll-step-size 5)
  )

(use-package anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(use-package undo-tree)
(defalias 'redo 'undo-tree-redo)

(setq use-package-always-ensure t)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(set-face-attribute 'default nil :height 80)

;;sane escape mode
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-g") 'goto-line)

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


(global-set-key (kbd "C-S-s") 'isearch-forward)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-q") 'beginning-of-line-text)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)
(global-set-key (kbd "C-y") 'redo)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-w") 'kill-buffer)
;;(global-set-key (kbd "C-S-o") 'helm-buffers-list)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "M-n") 'next-buffer)
(global-set-key (kbd "M-p") 'previous-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)
(global-set-key (kbd "C-<next>") 'next-buffer)

(use-package git-gutter
  :ensure t
  :when window-system
  :defer t
  :bind (("C-x P" . git-gutter:previous-hunk)
         ("C-x N" . git-gutter:next-hunk)
         ("C-c G" . git-gutter:popup-hunk))
  :diminish ""
  :init
  (add-hook 'prog-mode-hook #'git-gutter-mode)
  (add-hook 'text-mode-hook #'git-gutter-mode)
  :config
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

(use-package helm
  :ensure t
  :bind
  (("M-x" . helm-M-x)
   ("C-o" . helm-find-files)
   ("C-S-v" . 'helm-show-kill-ring))
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
(use-package helm-swoop)
;(global-set-key (kbd "M-x") 'execute-extended-command)


(use-package tabbar)
(tabbar-mode 1)
(global-set-key (kbd "C-<prior>") 'tabbar-backward-tab)
(global-set-key (kbd "C-<next>") 'tabbar-forward-tab)

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


(global-set-key (kbd "C-S-<prior>") 'tabbar-move-current-tab-one-place-left)
(global-set-key (kbd "C-S-<next>") 'tabbar-move-current-tab-one-place-right)
(global-set-key (kbd "C-w") 'kill-current-buffer)

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

(defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
  "Returns the name of the tab group names the current buffer belongs to.
    There are two groups: Emacs buffers (those whose name starts with '*', plus
    dired buffers), and the rest.  This works at least with Emacs v24.2 using
    tabbar.el v1.7."
  (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
              ((eq major-mode 'dired-sidebar) "emacs")
              ((eq major-mode 'dired-mode) "emacs")
              (t "user"))))

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

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

;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:update-interval 2)
 '(package-selected-packages
   '(magit smooth-scroll smooth-scrolling tabbar git-gutter-fringe tss git-gutter helm projectile vscode-icon dired-sidebar undo-tree color-theme web-mode js2-mode use-package)))
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

;; This doesn't work for revert, I don't know.
;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
(add-hook 'first-change-hook 'ztl-on-buffer-modification)

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package helm-projectile
  :config
  (helm-projectile-on)
  :bind
  ("C-M-p" . helm-projectile-switch-project)
  ("C-p" . helm-projectile-find-file))

(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file))

(define-prefix-command 'sidebar-keymap)
(global-set-key (kbd "C-k") 'sidebar-keymap)

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


;; Funky stuff
(global-set-key (kbd "C-S-k") 'kill-whole-line)

(cua-mode)

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
(use-package web-mode
  :ensure t)
;(use-package color-theme-monokai)
;(load-theme 'monokai t)
(load-theme 'monokai-alt t)



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
(global-set-key (kbd "C-S-t") 'reopen-killed-file)

(use-package open-junk-file)

(setq initial-major-mode (quote text-mode))
(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (setq buffer-offer-save t)
    (switch-to-buffer $buf)
    $buf
    ))

(global-set-key (kbd "C-M-c") 'comment-line)

;;kek
(defun my-forward-word ()
  (interactive "^")
  (cond ((eq (char-after) 10) (right-char 1))
	((looking-at "\\W+\n") (progn (message "%s" (char-after)) (end-of-line)))
	(t (forward-word))))

;; (interactive "^") means "please emacs deal with shift correctly.
(defun my-backward-word ()
  (interactive "^")
  (cond ((eq (char-before) 10) (left-char 1))
	((looking-back "\n\s-*\\W+" 250) (progn (message "%s" (char-after)) (beginning-of-line)))
	(t (backward-word))))

(global-set-key (kbd "C-<right>") 'my-forward-word)
(global-set-key (kbd "C-<left>") 'backward-word)
