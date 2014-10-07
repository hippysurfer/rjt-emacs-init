;; Load el-get to manage the packages.
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(require 'el-get)

;; Packages that require local config.
(setq el-get-sources
      '((:name magit
         :after (global-set-key (kbd "C-x C-z") 'magit-status))
	
	(:name autopair
	 :after (autopair-global-mode))

	(:name flycheck
	 :after (global-flycheck-mode))

	(:name auto-complete
	 :after (add-hook 'python-mode-hook 'auto-complete-mode))

	(:name jedi
	 :after (add-hook 'python-mode-hook 'jedi:setup))

	))


(setq my-packages
      (append
       '(cl-lib  ;; Packages that do not require local config.
	 autopep8
	 ctable
	 dash
	 deferred
	 el-get
	 epc
	 epl
	 hexrgb ;; required by one-key
	 f
	 fuzzy
	 package
	 pkg-info
	 popup
	 python-environment
	 s)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get-cleanup my-packages)
(el-get 'sync my-packages)

;; Prepackaged mode config.
(show-paren-mode 1)
(require 'ido) (ido-mode t)

;; Python Mode
(setq python-indent 4)
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 4
			    python-indent 4))))


(setq custom-file "~/.emacs.d/rjt-custom.el")
(load custom-file)
