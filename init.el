(require 'server)
(unless (server-running-p) (server-start))

(load-theme 'wombat)
;;(load-theme 'homebrew)

(setq frame-title-format "emacs")

(menu-bar-mode -1)

(tool-bar-mode -1)

(scroll-bar-mode -1)

(ido-mode)

(column-number-mode)

(show-paren-mode)

(winner-mode t)

(windmove-default-keybindings)

(set-language-environment 'UTF-8)
(set-locale-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(global-linum-mode 1)

(set-frame-font "Source Code Pro 14" nil t)

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let (( proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(global-set-key (kbd "C-z") 'set-mark-command)

(defun show-file-name ()
  "显示当前文件路径并拷贝到剪贴板"
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))
(defalias 'pwd 'show-file-name)

(require 'package)

(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
		  ("elpa" . "http://tromey.com/elpa/")
		  ("melpa stable" . "http://stable.melpa.org/packages/")
		  ("melpa-china" . "http://elpa.emacs-china.org/melpa/")
		  ("gnu" . "http://elpa.emacs-china.org/gnu/")
		  ))
  (add-to-list 'package-archives source t))
(package-initialize)

;;标题栏显示文件路径
(setq frame-title-format
      (list '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(require 'cl)

(defvar my-packages '(csharp-mode color-theme pug-mode multi-term magit exec-path-from-shell markdown-mode window-numbering pcomplete-extension eshell-manual flycheck ac-emacs-eclim ac-html-angular ac-html-csswatcher ac-php ac-python auto-complete-clang auto-complete-etags org-ac ac-html-bootstrap powerline ac-html ac-js2 web-mode ace-jump-mode undo-tree autopair smex auto-complete) "custom packages")

(setq package-selected-packages my-packages)

(defun my-packages-installed-p ()
  (loop for pkg in my-packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (my-packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(exec-path-from-shell-initialize)

(global-set-key (kbd "M-x") 'smex)

;;(global-set-key (kbd "C-c C-c") 'execute-extended-command)

(ac-config-default)

;;(nlinum-mode)

(autopair-global-mode)

(global-undo-tree-mode)

(global-set-key (kbd "M-/") 'undo-tree-visualize)

(global-set-key (kbd "C->") 'ace-jump-mode)

(powerline-center-theme)

(setq powerline-default-separator 'wave)

;;; web mode
(add-to-list 'auto-mode-alist '("\\.phtml'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl|\\.php'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.api'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?" . web-mode))
(add-hook 'web-mode-hook
	  (lambda ()
	    (if (equal web-mode-content-type "javascript")
		(web-mode-set-content-type "jsx")
	      (message "now set to: %s" web-mode-content-type))))


(global-flycheck-mode)

;;eshell
(defvar ac-source-eshell-pcomplete
  '((candidates . (pcomplete-completions))))
(defun ac-complete-eshell-pcomplete ()
  (interactive)
  (auto-complete '(ac-source-eshell-pcomplete)))
(add-to-list 'ac-modes 'eshell-mode)
(setq ac-sources '(ac-source-eshell-pcomplete))

;;pug-mode
(require 'pug-mode)
(add-to-list 'ac-modes 'pug-mode)
(add-to-list 'auto-mode-alist '("\\.pug?" . pug-mode))

(window-numbering-mode)
;; (defvar window-numbering-keymap
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "C-0" 'select-window-0)
;;     (define-key map "C-1" 'select-window-1)
;;     (define-key map "C-2" 'select-window-2)
;;     (define-key map "C-3" 'select-window-3)
;;     (define-key map "C-4" 'select-window-4)
;;     (define-key map "C-5" 'select-window-5)
;;     (define-key map "C-6" 'select-window-6)
;;     (define-key map "C-7" 'select-window-7)
;;     (define-key map "C-8" 'select-window-8)
;;     (define-key map "C-9" 'select-window-9)
;;     map)
;;   "Keymap used in by `window-numbering-mode'.")

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-crypt-tag-matcher "secret")
(setq org-tags-exclude-from-inheritance (quote ("secret")))
(setq org-crypt-key nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (csharp-mode color-theme homebrew-mode pug-mode seq moe-theme magit markdown-mode window-numbering pcomplete-extension eshell-manual flycheck ac-emacs-eclim ac-html-angular ac-html-csswatcher ac-php ac-python auto-complete-clang auto-complete-etags org-ac ac-html-bootstrap powerline ac-html ac-js2 web-mode ace-jump-mode undo-tree autopair smex auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;multi-term
(require 'multi-term)
(setq multi-term-program "/bin/zsh")
(setq system-uses-terminfo nil)
(add-hook 'term-mode-hook
	  (lambda ()
	    (setq term-buffer-maximum-size 0)))
(add-hook 'term-mode-hook
	  (lambda ()
	    (define-key term-raw-map (kbd "C-y") 'term-paste)))
(defun ab/is-at-end-line ()
  "判断是否在最后一行"
  (equal (line-number-at-pos) (count-lines (point-min) (point-max))))
(defun ab/is-term-mode ()
  "判断是否在终端模式"
  (string= major-mode "term-mode"))
(defun ab/move-beginning-of-line ()
  "move begin"
  (interactive)
  (if (not (ab/is-term-mode))
      (beginning-of-line)
    (if (not (ab/is-at-end-line))
	(beginning-of-line)
      (term-send-raw))))
(add-hook 'term-mode-hook
	  (lambda ()
	    (define-key term-raw-map (kbd "C-a") 'ab/move-beginning-of-line)))
(defun ab/move-end-of-line ()
  "move to end of line"
  (interactive)
  (if (not (ab/is-term-mode))
      (end-of-line)
    (if (not (ab/is-at-end-line))
	(end-of-line)
      (term-send-raw))))
(add-hook 'term-mode-hook
	  (lambda ()
	    (define-key term-raw-map (kbd "C-e") 'ab/move-end-of-line)))

