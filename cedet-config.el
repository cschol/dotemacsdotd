;; CEDET
(require 'semantic)

(setq semanticdb-default-save-directory (concat runtime-data-dir "semanticdb/"))
(global-semantic-idle-completions-mode t)
(global-semantic-decoration-mode t)
(global-semantic-highlight-func-mode t)
(global-semantic-show-unmatched-syntax-mode t)
(semanticdb-enable-gnu-global-databases 'c-mode)

;; CEDET integration with auto-complete
;; FIXME Move to separate hook
(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq ac-sources (append '(ac-source-semantic) ac-sources))
             (linum-mode t)
             (semantic-mode 1)))
