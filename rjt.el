;; Put backup files away from the projects.
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs-saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Turn off the toolbar
(tool-bar-mode -1)


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

	(:name projectile
	 :after (projectile-global-mode))

	(:name expand-region
	 :after (global-set-key (kbd "C-=") 'er/expand-region))

	(:name multiple-cursors
	 :after (progn
		  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
		  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
		  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
		  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)			   
		  ))
	))


(setq my-packages
      (append
       '(cl-lib  ;; Packages that do not require local config.
	 autopep8
	 bookmark+
	 bookmark+-mac
	 bookmark+-bmu
	 bookmark+-1
	 bookmark+-key
	 bookmark+-lit
	 bookmark+-doc
	 bookmark+-chg
	 ctable
	 dash
	 deferred
	 el-get
	 epc
	 epl
	 hexrgb ;; required by one-key
	 f
	 flx ;; required by projectile
	 fuzzy
	 package
	 pkg-info
	 popup
	 python-environment
	 s)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get-cleanup my-packages)
(el-get 'sync my-packages)

(require 'bookmark+)

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
;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
 
;; enable recent files mode.
(recentf-mode t)
 
; 50 files ought to be enough.
(setq recentf-max-saved-items 50)
 
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(setq custom-file "~/.emacs.d/rjt-custom.el")
(load custom-file)
