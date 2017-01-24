(load-theme 'wombat)

(setq frame-title-format "emacs")

(menu-bar-mode -1)

(tool-bar-mode -1)

(scroll-bar-mode -1)

(ido-mode)

(column-number-mode)

(show-paren-mode)

(winner-mode t)

(windmove-default-keybindings)

(set-terminal-coding-system 'utf-8)

(set-keyboard-coding-system 'utf-8)

(prefer-coding-system 'utf-8)

(global-linum-mode 1)
;;(setq linum-format "%d ")

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let (( proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(require 'package)

(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
		  ("elpa" . "http://tromey.com/elpa/")
		  ("melpa stable" . "http://stable.melpa.org/packages/")
		  ("melpa-china" . "http://elpa.emacs-china.org/melpa/")
		  ("gnu" . "http://elpa.emacs-china.org/gnu/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(require 'cl)

(defvar my-packages '(window-numbering pcomplete-extension eshell-manual exec-path-from-shell flycheck ac-emacs-eclim ac-html-angular ac-html-csswatcher ac-php ac-python auto-complete-clang auto-complete-etags org-ac ac-html-bootstrap powerline ac-html ac-js2 web-mode ace-jump-mode undo-tree autopair smex auto-complete) "custom packages")

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

(global-set-key (kbd "M-x") 'smex)

(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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
(exec-path-from-shell-initialize)

;;eshell
(defvar ac-source-eshell-pcomplete
  '((candidates . (pcomplete-completions))))
(defun ac-complete-eshell-pcomplete ()
  (interactive)
  (auto-complete '(ac-source-eshell-pcomplete)))
(add-to-list 'ac-modes 'eshell-mode)
(setq ac-sources '(ac-source-eshell-pcomplete))

(window-numbering-mode)


