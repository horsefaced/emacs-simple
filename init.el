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
		  ("melpa stable" . "http://stable.melpa.org/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (gtags imenu-anywhere markdown-mode+ window-numbering pcomplete-extension eshell-manual exec-path-from-shell flycheck ac-emacs-eclim ac-html-angular ac-html-csswatcher ac-php ac-python auto-complete-clang auto-complete-etags org-ac ac-html-bootstrap powerline ac-html ac-js2 web-mode ace-jump-mode undo-tree autopair smex auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


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

;;imenu-anywhere
(global-set-key (kbd "C-.") #'imenu-anywhere)


(defun es-js-imenu-create-index ()
  "Return an imenu index for the current buffer."
  (save-excursion
    (save-restriction
      (let (unique-names result name pos)
        (widen)
        (goto-char (point-min))
        (while (re-search-forward
                (concat "\\(\\_<.+?\\_>\\) = function"
                        "\\|"
                        "\\(\\_<.+?\\_>\\): function"
                        "\\|"
                        "function \\(\\_<.+?\\_>\\)")
                nil t)
          (setq name (or (match-string-no-properties 1)
                         (match-string-no-properties 2)
                         (match-string-no-properties 3))
                pos (or (match-beginning 1)
                        (match-beginning 2)
                        (match-beginning 3)))
          (when (member name unique-names)
            (let ((counter 2)
                  new-name)
              (while (progn (setq new-name (concat name "[" (int-to-string counter) "]"))
                            (member new-name unique-names))
                (incf counter))
              (setq name new-name)))
          (push name unique-names)
          (push (cons name (js--maybe-make-marker pos)) result))
        (nreverse result)))))


(defun es-js-imenu-goto-function-at-point ()
  (interactive)
  (let* (( word (or (thing-at-point 'symbol)
                    (return-from es-js-imenu-goto-function-at-point)))
         ( index (es-js-imenu-create-index))
         ( pair (or (find-if (lambda (cons)
                               (and (string-prefix-p word (car cons))
                                    (not (= (point) (marker-position (cdr cons))))))
                             index)
                    (return-from es-js-imenu-goto-function-at-point))))
    (ring-insert find-tag-marker-ring (point-marker))
    (goto-char (marker-position (cdr pair))))
  )

(add-hook 
 'web-mode-hook
 '(lambda ()
    (setq-local imenu-create-index-function 'es-js-imenu-create-index)
    (gtags-mode 1)))
;; (setq javascript-common-imenu-regex-list
;;       '(("Controller" "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ("Controller" "[. \t]controllerAs:[ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ("Filter" "[. \t]filter([ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ("State" "[. \t]state[(:][ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ("Factory" "[. \t]factory([ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ("Service" "[. \t]service([ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ("Module" "[. \t]module( *['\"]\\([a-zA-Z0-9_.]+\\)['\"], *\\[" 1)
;;         ("ngRoute" "[. \t]when(\\(['\"][a-zA-Z0-9_\/]+['\"]\\)" 1)
;;         ("Directive" "[. \t]directive([ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ("Event" "[. \t]\$on([ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ("Config" "[. \t]config([ \t]*function *( *\\([^\)]+\\)" 1)
;;         ("Config" "[. \t]config([ \t]*\\[ *['\"]\\([^'\"]+\\)" 1)
;;         ("OnChange" "[ \t]*\$(['\"]\\([^'\"]*\\)['\"]).*\.change *( *function" 1)
;;         ("OnClick" "[ \t]*\$([ \t]*['\"]\\([^'\"]*\\)['\"]).*\.click *( *function" 1)
;;         ("Watch" "[. \t]\$watch( *['\"]\\([^'\"]+\\)" 1)
;;         ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
;;         ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
;;         ;; {{ es6 beginning
;;         ("Function" "^[ \t]*\\([A-Za-z_$][A-Za-z0-9_$]+\\)[ \t]*([a-zA-Z0-9, ]*) *\{ *$" 1) ;; es6 fn1 () { }
;;         ("Function" "^[ \t]*\\([A-Za-z_$][A-Za-z0-9_$]+\\)[ \t]*=[ \t]*(?[a-zA-Z0-9, ]*)?[ \t]*=>" 1) ;; es6 fn1 = (e) =>
;;         ;; }}
;;         ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ))

;; (add-hook 'web-mode-hook
;; 	  (lambda ()
;; 	    (setq imenu-generic-expression javascript-common-imenu-regex-list)))

