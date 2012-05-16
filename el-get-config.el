(setq el-get-sources
      '((:name smex
               :after (progn
                        (setq smex-save-file (concat runtime-data-dir ".smex-items"))
                        (global-set-key (kbd "M-x") 'smex)
                        (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

        (:name autopair
               :after (progn
                        (setq autopair-autowrap t)

                        (defvar my-autopair-enabled-mode-hook-list
                          '(c-mode-common-hook
                            python-mode-hook
                            js2-mode-hook)
                          "List of modes that have autopair-mode enabled.")

                        (mapc (lambda (hook)
                                (add-hook hook (lambda () (autopair-mode))))
                              my-autopair-enabled-mode-hook-list)))
        (:name paredit
               :after (progn
                        (defvar my-enable-paredit-mode-hook-list
                          '(emacs-lisp-mode-hook
                            lisp-mode-hook
                            lisp-interaction-mode-hook)
                          "List of hooks of modes that have paredit enabled.")

                        (mapc (lambda (hook)
                                (add-hook hook (lambda () (paredit-mode +1))))
                              my-enable-paredit-mode-hook-list)))

        (:name auto-complete
               :after (progn
                        (require 'auto-complete-config)
                        (add-to-list 'ac-dictionary-directories (concat dotfiles-dir "plugins/autocomplete/dict"))
                        (ac-config-default)
                        (setq ac-comphist-file (concat runtime-data-dir "ac-comphist.dat"))
                        (setq ac-auto-start nil)
                        (define-key ac-mode-map (kbd "M-/") 'auto-complete)
                        (setq ac-use-quick-help nil)
                        (setq ac-use-menu-map t)
                        (define-key ac-menu-map "\C-n" 'ac-next)
                        (define-key ac-menu-map "\C-p" 'ac-previous)

                        ;; (ac-define-source etags
                        ;;   '((candidates . (lambda ()
                        ;;                     (all-completions ac-target (tags-completion-table))))
                        ;;     (requires . 3)
                        ;;     (summary "auto-complete source for Etags")))

                        ;; (add-hook 'c-mode-common-hook
                        ;;           '(lambda ()
                        ;;              (add-to-list 'ac-omni-completion-sources
                        ;;                           (cons "\\." '(ac-source-semantic)))
                        ;;              (add-to-list 'ac-omni-completion-sources
                        ;;                           (cons "->" '(ac-source-semantic)))
                        ;;              (add-to-list 'ac-sources 'ac-source-etags)))
                        ))

        ;; (:name browse-kill-ring+
        ;;        :after (progn
        ;;                 (ad-enable-advice 'kill-new 'around 'browse-kill-ring-no-kill-new-duplicates)
        ;;                 (ad-activate 'kill-new)
        ;;                 (setq browse-kill-ring-quit-action 'save-and-restore
        ;;                       browse-kill-ring-no-duplicates t)
        ;;                 ))

        (:name python
               :after (progn
                        (defun my-insert-self ()
                          "Insert `self.' at the beginning of the current expression."
                          (interactive)
                          (save-excursion
                            (search-backward-regexp "[ \n\t,(-]\\|^")
                            (if (not (looking-at "^"))
                                (forward-char))
                            (insert "self.")))

                        (defun my-python-mode-hook ()
                          (setq python-indent-offset 4)

                          ;; Adjust autopair behavior for Python triple-quotes
                          (setq autopair-handle-action-fns
                                (list 'autopair-default-handle-action
                                      'autopair-python-triple-quote-action)))
                        (add-hook 'python-mode-hook 'my-python-mode-hook)))

        (:name org-mode
               :website "http://orgmode.org/"
               :description "Org-mode is for keeping notes, maintaining ToDo lists, doing project planning, and authoring with a fast and effective plain-text system."
               :type git
               :url "git://orgmode.org/org-mode.git"
               :info "doc"
               :load-path ("." "lisp" "contrib/lisp")
               :autoloads nil
               :features org-install)
        ))


(setq el-get-packages
      '(el-get
        browse-kill-ring
        helm))

;; add a hook listener for post-install el-get
(defun post-install-hook (pkg)
  ;; after installing el-get, load the local package list
  (if (string-equal pkg "el-get")
      (el-get 'sync
              (append el-get-packages
                      (mapcar 'el-get-source-name el-get-sources)))))
(add-hook 'el-get-post-install-hooks 'post-install-hook)

;; add the el-get directory to the load path
(add-to-list 'load-path
             (concat (file-name-as-directory user-emacs-directory)
                     (file-name-as-directory "el-get")
                     "el-get"))

;; try to require el-get master branch
(if (eq (require 'el-get nil t) nil)

    ;; urp, need to bootstrap
    (url-retrieve
     "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
     (lambda (s)
       (let (el-get-master-branch)
         (goto-char (point-max))
         (eval-print-last-sexp))))

  ;; successfully required el-get, load the packages!
  (post-install-hook "el-get"))
