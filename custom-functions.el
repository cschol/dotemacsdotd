;; Custom emacs functions

(defun my-kill-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (when (y-or-n-p "Kill all buffers?")
    (dolist (buf (buffer-list))
      (kill-buffer buf))
    (delete-other-windows)))

(defun ctags-process-callback (process event)
  "Show status of asynchronous ctags-process after it finishes."
  (cond
   ((string-equal event "finished\n")
    (message "Creating tags file...done")
    (kill-buffer (get-buffer "*ctags*")))
   (t
    (message "Creating tags file...failed")
    (pop-to-buffer (get-buffer "*ctags*"))
    (compilation-mode))))

(defvar ctags-path (concat dotfiles-dir "plugins/ctags/ctags")
  "Path to exuberant ctags executable.")

(defvar ctags-options
  "-e -R -h default --c-kinds=+l+x+p --fields=+i+a+S --extra=+q"
  "Custom options to build ctags database.")

(defun my-create-tags-file ()
  (interactive)
  (let ((proc-name "ctags-process"))
    (message "Creating tags file...")
    (start-process-shell-command proc-name "*ctags*" (concat ctags-path " " ctags-options))
    (set-process-sentinel (get-process proc-name) 'ctags-process-callback)))

(defun my-create-tags-file-in-current-dir ()
  "Create tags file in current directory."
  (interactive)
  ; Check if any source files exist in current directory
  (if (equal (file-expand-wildcards "*.[chCH]" nil) nil)
      (message "No *.[ch] files found! No tags file created.")
    (my-create-tags-file)))

(eval-after-load 'cc-mode
  '(progn
     ;; Add hook after loading cc-mode because `c-mode-map' is void before
     (add-hook 'c-common-mode-hook (define-key c-mode-map (kbd "C-c t")
                                     'my-create-tags-file-in-current-dir))))

(defun my-duplicate-line (n)
  "Duplicate current line n times, leaving point in lower line."
  (interactive "*p")
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t) ; disable undo while duplucating
            (count n))
        (dotimes (count n)
          (newline)
          (insert line))
        )
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    )
  (forward-line n))


(defun my-duplicate-start-of-line-or-region ()
  (interactive)
  (if (equal (use-region-p) t)
      (duplicate-region)
    (duplicate-start-of-line)))
(global-set-key (kbd "C-S-d") 'my-duplicate-start-of-line-or-region)

(autoload 'beginning-of-thing "thingatpt")
(defun duplicate-start-of-line ()
  "Duplicate start of line, leaving point after inserted line."
  (let ((text (buffer-substring (point)
                                (beginning-of-thing 'line))))
    (forward-line)
    (push-mark)
    (insert text)
    (open-line 1)))

(defun duplicate-region ()
  "Duplicate currently selected region, leaving point after inserted region."
  (let* ((eor (region-end))
         (text (buffer-substring (region-beginning) eor)))
    (goto-char eor)
    (newline)
    (insert text)
    (push-mark)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

;; from http://stackoverflow.com/questions/2173324/emacs-equivalents-of-vims-dd-o-o
(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun my-vi-open-line (&optional belowp)
  "Insert a newline above the current line and put point at beginning.
With a prefix argument, insert a newline below the current line."
  (interactive "P")
  (if belowp
      (vi-open-line-below)
    (vi-open-line-above)))


(defun my-eval-enum ()
  "Get value of enumeration member on current line."
  (interactive)
  (save-excursion
    (let ((currline (line-number-at-pos)))
      (c-beginning-of-statement-1)
      (message "Enum value: %d" (- currline (line-number-at-pos))))))

(autoload 're-builder "re-builder" nil t)
(defun my-reb-copy ()
  "Copy current RE into the kill ring without quotes and single
backslashes for later insertion."
  (interactive)
  (reb-update-regexp)
  (let* ((re (with-output-to-string
               (print (reb-target-binding reb-regexp))))
         (str (substring re 2 (- (length re) 2))))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (search-forward "\\\\" nil t)
        (replace-match "\\" nil t))
      (kill-new (buffer-substring (point-min) (point-max))))
    (message "Regexp copied to kill-ring")))

(eval-after-load 're-builder
  '(progn
     (define-key reb-mode-map "\C-c\C-t" 'my-reb-copy)))

;; from http://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))

(defun my-enumerate-region (n)
  "Consecutively enumerate all numbers found in selected
region starting at number specfied by parameter n."
  (interactive "*p")
  (let (start end bounds)
    (if (and transient-mark-mode mark-active)
        (setq start (region-beginning) end (region-end))
      (message "No region selected."))
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((count n))
        (while (re-search-forward "\\([0-9]+\\)" nil t)
          (replace-match (number-to-string count))
          (setq count (+ 1 count)))))))

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun my-comment-dwim-line (&optional arg)
  "If no region is selected and current line is not blank and we are
not at the end of the line, then comment current line.  Replaces
default behaviour of comment-dwim, when it inserts comment at the
end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun isearch-yank-word-at-point ()
  "Pull word at point into the search string."
  (interactive)
  ;; Only yank if point is on a word constituent or
  ;; symbol constituent per the syntax table.
  (when (or (= (char-syntax (or (char-after) 0)) ?w)
            (= (char-syntax (or (char-after) 0)) ?_))
    ;; If part of the string has been yanked to the search string
    ;; already, unwind the isearch state stack to the beginning to
    ;; start over.
    (while (not (string= isearch-string ""))
      (isearch-pop-state))

    ;; Go to beginning of word at point
    (goto-char (car (bounds-of-thing-at-point 'sexp)))
    ;; and yank entire word into search string.
    (isearch-yank-internal
     (lambda ()
       (point)
       (cdr (bounds-of-thing-at-point 'sexp))))))

(define-key isearch-mode-map (kbd "C-a") 'isearch-yank-word-at-point)

;; Original idea from
;; http://www.emacswiki.org/cgi-bin/wiki/setup-cygwin.el
(defun set-shell-bash()
  "Set shell to `bash'. It assumes `bash' is available, e.g. via Cygwin."
  (interactive)
  (setq shell-file-name "bash")
  (setq shell-command-switch "-lc")
  (setq explicit-shell-file-name "bash")
  (setenv "SHELL" explicit-shell-file-name)
  (setenv "LC_ALL" "en_US") ;; cygwin bash needs locale set here
  (setq w32-quote-process-args ?\"))

(defun set-shell-cmdproxy()
  "Set shell to `cmdproxy'."
  (interactive)
  (setq shell-file-name "cmdproxy")

  (setq shell-command-switch "-c")
  (setq explicit-shell-file-name "cmdproxy")
  (setenv "SHELL" explicit-shell-file-name)
  (setq w32-quote-process-args nil))

;; Original idea from:
;; http://emacs-fu.blogspot.com/2009/02/transparent-emacs.html
(defun my-modify-opacity (&optional dec)
  "Modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps."
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
	 (oldalpha (if alpha-or-nil alpha-or-nil 100))
	 (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha)))))
  (message "Frame opacity: %d" (frame-parameter nil 'alpha)))

;; C-8 will increase opacity (== decrease transparency)
;; C-9 will decrease opacity (== increase transparency
;; C-0 will returns the state to normal
(global-set-key (kbd "C-8") '(lambda()(interactive)(my-modify-opacity)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(my-modify-opacity t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

(modify-frame-parameters nil `((alpha . 80))) ;; default opacity on startup
(setq frame-inherited-parameters '(alpha))

;; from http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(global-set-key [pause] 'toggle-current-window-dedication)

;; from: http://whattheemacsd.com/setup-paredit.el-03.html
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
