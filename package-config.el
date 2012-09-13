;; Inspired by: 
;; http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/

(require 'cl)
(require 'package)

(setq package-user-dir (concat dotfiles-dir "/vendor/packages"))
(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(defvar emacs-packages
  '(ack-and-a-half
    solarized-theme
    zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(defun emacs-packages-installed-p ()
  (loop for p in emacs-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (emacs-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p emacs-packages)
    (when (not (package-installed-p p))
      (package-install p))))
