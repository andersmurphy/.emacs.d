;;; totp.el --- *scratch* but for text -*- lexical-binding: t -*-
;;; Commentary: minimal TOTP implementation
;;; Code:
(require 'bindat)
(require 'gnutls)
(require 'hexl)
(require 'auth-source)
(require 'base32)

(defun totp--hex-decode-string (string)
  "Hex-decode STRING and return the result as a unibyte string."
  (apply #'unibyte-string
         (seq-map (lambda (s) (hexl-htoi (aref s 0) (aref s 1)))
                  (seq-partition string 2))))

(defun totp (string &optional time digits)
  "Return a TOTP token using the secret hex STRING and current time.
TIME is used as counter value instead of current time, if non-nil.
DIGITS is the number of pin digits and defaults to 6."
  (let* ((key-bytes (totp--hex-decode-string (upcase string)))
         (counter (truncate (/ (or time (time-to-seconds)) 30)))
         (digits (or digits 6))
         (format-string (format "%%0%dd" digits))
         ;; we have to manually split the 64 bit number (u64 not supported in Emacs 27.2)
         (counter-bytes (bindat-pack  '((:high u32) (:low u32))
                                      `((:high . ,(ash counter -32)) (:low . ,(logand counter #xffffffff)))))
         (mac (gnutls-hash-mac 'SHA1 key-bytes counter-bytes))
         (offset (logand (bindat-get-field (bindat-unpack '((:offset u8)) mac 19) :offset) #xf)))
    (format format-string
            (mod
             (logand (bindat-get-field (bindat-unpack '((:totp-pin u32)) mac  offset) :totp-pin)
                     #x7fffffff)
             (expt 10 digits)))))

(defun totp-display (auth)
  "Select a TOTP AUTH from `auth-sources' and display its TOTP."
  (interactive
   (list
    (let ((candidates (mapcar
                       (lambda (auth)
                         (cons (format "User '%s' on %s"
                                       (propertize (plist-get auth :user)
                                                   'face
                                                   'font-lock-keyword-face)
                                       (propertize (plist-get auth :host)
                                                   'face
                                                   'font-lock-string-face))
                               auth))
                       (seq-filter
                        (lambda (auth)
                          (string-prefix-p "TOTP:" (plist-get auth :host)))
                          (auth-source-search :max 10000)))))
      (cdr (assoc (completing-read "Pick a TOTP> " candidates) candidates)))))
  (let ((code (totp (base32-hex-decode (funcall (plist-get auth :secret))))))
    (message "Your TOTP for '%s' is: %s (sent to kill ring)"
             (propertize (plist-get auth :host) 'face font-lock-keyword-face)
             (propertize code 'face 'font-lock-string-face))
    (kill-new code)
    code))

(provide 'totp)
;;; totp.el ends here
