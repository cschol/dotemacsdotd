;; imap/mail configuration
(setq send-mail-function (quote smtpmail-send-it))
(setq user-mail-address "cschol2112@gmail.com")
(setq smtpmail-smtp-server "imap.gmail.com")
(setq smtpmail-smtp-service 587)
(setq smtpmail-stream-type 'starttls)
