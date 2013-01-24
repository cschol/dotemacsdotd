;; Inspired by Alex Ott's configuration found here: https://gist.github.com/3930120

(setq cedet-root-path (file-name-as-directory (concat dotfiles-dir "el-get/cedet/")))
(add-to-list 'load-path (concat cedet-root-path "contrib"))
(add-to-list 'Info-directory-list (concat cedet-root-path "/doc/info"))

(setq semanticdb-default-save-directory (concat runtime-data-dir "semanticdb/"))

;; select which submodes we want to activate
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)

;; Activate semantic
(semantic-mode 1)

;; Disable pulsing
(setq pulse-flag 'never)

;; CEDET integration with auto-complete
;; FIXME Move to separate hook
(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq ac-sources (append '(ac-source-semantic) ac-sources))
             (linum-mode t)
             ))

;; load contrib library
(require 'eassist)

;; customisation of modes
(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'my-cedet-hook)

(defun my-c-mode-cedet-hook ()
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref))
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

(when (cedet-gnu-global-version-check t)
 (semanticdb-enable-gnu-global-databases 'c-mode t)
 (semanticdb-enable-gnu-global-databases 'c++-mode t))

(when (cedet-ectag-version-check t)
  (semantic-load-enable-primary-ectags-support))

;; SRecode
;(global-srecode-minor-mode 1)

;; EDE
;(global-ede-mode 1)
;(ede-enable-generic-projects)

