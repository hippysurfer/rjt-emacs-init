;; Put backup files away from the projects.
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs-saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Confirm before quiting
(setq confirm-kill-emacs 'y-or-n-p)

;; Turn off the toolbar
(tool-bar-mode -1)

;; Load the notes as the initial buffer
(setq initial-buffer-choice "~/.emacs.d/rjt-emacs-notes.org")

;; Set emshell smart mode.
(require 'eshell)
  (require 'em-smart)
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)


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
		  )))
      )


(setq my-packages
      (append
       '(cl-lib  ;; Packages that do not require local config.
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
	 pyvenv
	 pydoc-info
	 s
	 yasnippet
	 yasnippets
	 yasnippet-snippets)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get-cleanup my-packages)
(el-get 'sync my-packages)

(require 'bookmark+)
(setq bookmark-default-file "~/.emacs.d/rjt-bookmarks.el")

;; Prepackaged mode config.
(show-paren-mode 1)
(require 'ido) (ido-mode t)

;; Python Mode

;; To install python docs:
;;   sudo apt-get install mercurial
;;   wget https://bitbucket.org/jonwaltman/pydoc-info/downloads/python.info.gz
;; gunzip python.info
;; sudo cp python.info /usr/share/info
;; sudo install-info --info-dir=/usr/share/info python.info

(add-to-list 'load-path "~/.emacs.d/rjt-extras")
(require 'pytest)
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key "\C-ca" 'pytest-all)
            (local-set-key "\C-cm" 'pytest-module)
            (local-set-key "\C-c." 'pytest-one)
            (local-set-key "\C-cd" 'pytest-directory)
            (local-set-key "\C-cpa" 'pytest-pdb-all)
            (local-set-key "\C-cpm" 'pytest-pdb-module)
            (local-set-key "\C-cp." 'pytest-pdb-one)))

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


;; Autopep8 support
(defun autopep8 ()
  "Apply autopep8 to the current region or buffer"
  (interactive)
  (unless (region-active-p)
    (mark-whole-buffer))
  (shell-command-on-region
   (region-beginning) (region-end) ;; beginning and end of region or buffer
   "autopep8 -"                    ;; command and parameters
   (current-buffer)                ;; output buffer
   t                               ;; replace?
   "*autopep8 errors*"             ;; name of the error buffer
   t))       

;; autopep8 with ediff
(defun autopep8-and-ediff ()
  "Compare the current buffer to the output of autopep8 using ediff"
  (interactive)
  (let ((p8-output
         (get-buffer-create (format "* %s autopep8 *" (buffer-name)))))
    (shell-command-on-region
     (point-min) (point-max)    ;; beginning and end of buffer
     "autopep8 -"               ;; command and parameters
     p8-output                  ;; output buffer
     nil                        ;; replace?
     "*autopep8 errors*"        ;; name of the error buffer
     t)                         ;; show error buffer?
    (ediff-buffers (current-buffer) p8-output)
    ))

(setq custom-file "~/.emacs.d/rjt-custom.el")
(load custom-file)
