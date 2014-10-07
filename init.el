(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/org-mode/lisp")

(add-hook 'org-mode-hook (lambda ()
                           (set (make-local-variable 'electric-indent-functions)
                                (list (lambda (arg) 'no-indent)))))

(setq org-src-fontify-natively t)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'mercurial)
(require 'dired)
(setq dired-recursive-deletes (quote top))

(require 'bs)
(setq bs-configurations '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))

(define-key dired-mode-map (kbd "f") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "^") (lambda ()
                                       (interactive)
                                       (find-alternate-file "..")))

(defvar my-keys-minor-mode-map (make-keymap) "my keys")

; start auto-complete with emacs
(require 'auto-complete-config)
(ac-config-default)

(require 'smartparens-config)

; start yasnippet with emacs
(yas-global-mode 1)

(setq save-place-file (concat user-emacs-directory "saveplace.el"))
(setq-default save-place t)

(defun select-current-line ()
  "Selects the current line"
  (interactive)
  (end-of-line)
  (push-mark (line-beginning-position) nil t))

(defun backward-kill-line (arg)
  "Kill ARG lines backward"
  (interactive "p")
  (kill-line (- 1 arg))
  )

(defun line-above ()
  "Pastes line above"
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun cut-line-or-region ()
  ""
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2))))

(global-set-key [remap kill-region] 'cut-line-or-region)

(defun copy-line-or-region ()
  ""
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))))

(global-set-key [remap kill-ring-save] 'copy-line-or-region)

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (beginning-of-visual-line)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/c++/v1")
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/5.1/include")
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"))

(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)
(add-hook 'markdown-mode-hook 'smartparens-mode)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq gc-cons-threshold 20000000)

;; SML environment

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/usr/local/smlnj-110.75/bin:" (getenv "PATH")))
(setq exec-path (cons "/usr/local/bin" exec-path))
(setq exec-path (cons "/usr/local/smlnj-110.75/bin" exec-path))

;; Key remappings

(define-key my-keys-minor-mode-map (kbd "M-k") 'next-line)
(define-key my-keys-minor-mode-map (kbd "M-i") 'previous-line)
(define-key my-keys-minor-mode-map (kbd "M-j") 'backward-char)
(define-key my-keys-minor-mode-map (kbd "M-l") 'forward-char)
(define-key my-keys-minor-mode-map (kbd "C-=") 'er/expand-region)
(define-key my-keys-minor-mode-map (kbd "M-s-l") 'select-current-line)
(define-key my-keys-minor-mode-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key my-keys-minor-mode-map (kbd "M-RET") 'line-above)
(define-key my-keys-minor-mode-map (kbd "C-c d") 'duplicate-current-line-or-region)
(define-key my-keys-minor-mode-map (kbd "C-c k") 'kill-whole-line)
(define-key my-keys-minor-mode-map (kbd "C-c u") 'backward-kill-line)
(define-key my-keys-minor-mode-map (kbd "C->") 'mc/mark-next-like-this)
(define-key my-keys-minor-mode-map (kbd "C->") 'mc/mark-previous-like-this)
(define-key my-keys-minor-mode-map (kbd "C-c C-<") 'mc/mark-all-like-this)
(define-key my-keys-minor-mode-map (kbd "C-S-c C-S-c") 'mc/edit-lines)
(define-key my-keys-minor-mode-map (kbd "C-x C-;") 'comment-or-uncomment-region)
(define-key my-keys-minor-mode-map (kbd "C-c ;") 'iedit-mode)
(define-key my-keys-minor-mode-map (kbd "C-c .") 'yas-expand)
(define-key my-keys-minor-mode-map (kbd "<f2>") 'bs-show)

(electric-indent-mode t)
(electric-pair-mode t)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)

(delete-selection-mode t)
(blink-cursor-mode t)
(show-paren-mode t)

;; Gui tune
(if (eq system-type 'darwin)
    (set-frame-font "Source Code Pro for Powerline-12"))

(menu-bar-mode -1)
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  )

(define-minor-mode my-keys-minor-mode
  "A minor mode for my custom keys"
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode t)
(projectile-global-mode)

(global-visual-line-mode t) ;; Don't break inside words

(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable backups
(setq backup-inhibited t)
(setq inhibit-startup-message   t)   ; Don't want any startup message
(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving

(setq search-highlight           t) ; Highlight search object
(setq query-replace-highlight    t) ; Highlight query object
(setq mouse-sel-retain-highlight t) ; Keep mouse high-lightening

(setq scroll-step 1)

(put 'dired-find-alternate-file 'disabled nil)

(when (window-system)
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
   '(custom-safe-themes (quote ("58fb295e041032fd7a61074ca134259dfdef557ca67d37c4240dbfbb11b8fcc7" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
   '(sp-base-key-bindings (quote sp)))

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
)

; turn on semantic
(semantic-mode 1)
(global-semantic-idle-scheduler-mode 1)

(defun my:add-semantic-to-autocomplete ()
  (add-to-list 'ac-sources 'ac-source-semantic)
  )

(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(elpy-enable)
