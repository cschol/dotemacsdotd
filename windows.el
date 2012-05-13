;; Emacs settings specific to the Windows platform

;; some helper functions
(defun my-update-path-and-exec-path (path-alist)
  "Update PATH and exec-path with contents of path-alist."
  (mapc
   (lambda (x)
     (let ((path (expand-file-name x)))
       (cond
        ((file-exists-p path)
         (setenv "PATH" (concat path ";" (getenv "PATH")))
         (setq exec-path (cons path exec-path)))
        (t
         (message (format "Path could not be added: %s" path))))))
   path-alist))

;; set up default font
(set-face-attribute 'default nil :font "Consolas-9")

;; cygwin integration
;; (my-update-path-and-exec-path
;;  (list "c:\\cygwin\\bin"
;;        "c:\\cygwin\\usr\\sbin"
;;        "c:\\cygwin\\usr\\local\\bin"))

(defun platform-windows7-p ()
  (let ((version (w32-version)))
    (and (eq (car version) 6)
         (eq (cadr version) 1))))

(let ((version (w32-version)))
  ;; On Windows 7 we need to use Unx Utils, since `find' has a bug in GnuWin32.
  (if (platform-windows7-p)
      ;; Unx Utils
      (my-update-path-and-exec-path
       (list "C:\\Program Files (x86)\\UnxUtils\\usr\\local\\wbin"))
    ;; GnuWin32 integration (paths support 64-bit Windows)
    (my-update-path-and-exec-path
     (list "c:\\Program Files (x86)\\GnuWin32\\bin"
           "c:\\Program Files\\GnuWin32\\bin"))))

;; Python integration.
(my-update-path-and-exec-path
 (list "c:\\python27"))

;; puTTY (if available)
(my-update-path-and-exec-path
 (list "C:\\program Files (x86)\\puTTY"))
