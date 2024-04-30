;;; base32.el --- *scratch* but for text -*- lexical-binding: t -*-
;;; Commentary: partial implementation of base32 hex decoding
;;; Code:
(require 'subr-x)

(defconst base32-alphabet
  (let ((tbl (make-char-table nil)))
    (dolist (mapping
             '(("A" . 0)  ("B" . 1)  ("C" . 2)  ("D" . 3)
               ("E" . 4)  ("F" . 5)  ("G" . 6)  ("H" . 7)
               ("I" . 8)  ("J" . 9)  ("K" . 10) ("L" . 11)
               ("M" . 12) ("N" . 13) ("O" . 14) ("P" . 15)
               ("Q" . 16) ("R" . 17) ("S" . 18) ("T" . 19)
               ("U" . 20) ("V" . 21) ("W" . 22) ("X" . 23)
               ("Y" . 24) ("Z" . 25) ("2" . 26) ("3" . 27)
               ("4" . 28) ("5" . 29) ("6" . 30) ("7" . 31)))
      (aset tbl (string-to-char (car mapping)) (cdr mapping)))
    tbl)
  "Base-32 mapping table, as defined in RFC 4648.")

(defun base32-hex-decode (string)
  "The cheats' version of base-32 decode.

This is not a 100% faithful implementation of RFC 4648. The
concept of encoding partial quanta is not implemented fully.

No attempt is made to pad the output either as that is not
required for HMAC-TOTP."
  (if (= (mod (length string) 8) 0)
      (format
       "%X"
       (seq-reduce
        (lambda (acc char) (+ (ash acc 5) (aref base32-alphabet char)))
        (thread-first
          (upcase string)
          (string-trim-right "=+")
          (append nil))
        0))
    (error "Padding is incorrect")))

(provide 'base32)
;;; base32.el ends here
