;;; org2blog-def.el --- Org2Blog System Utility      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Grant Rettke

;; Author: Grant Rettke <grant@wisdomandwonder.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;; Constant

(defconst owp--package
  (let ((p (make-hash-table :test 'equal)))
    (puthash "name" "org2blog" p)
    (puthash "version" "1.1.0" p)
    (puthash "doc" "Blog from Org mode to WordPress" p)
    (puthash "emacs" "26.2" p)
    (puthash "requirements"
             '((htmlize "1.55" "https://github.com/hniksic/emacs-htmlize.git")
               (hydra "0.14.0" "https://github.com/abo-abo/hydra.git")
               (org "9.1.9" "https://code.orgmode.org/bzg/org-mode")
               (xml-rpc "1.6.12" "https://github.com/hexmode/xml-rpc-el.git"))
             p)
    (puthash "keywords" '("comm" "convenience" "outlines" "wp") p)
    (puthash "authors" '(("Puneeth Chaganti" .
                          "punchagan+org2blog@gmail.com"))
             p)
    (puthash "maintainer" '("Grant Rettke" . "grant@wisdomandwonder.com") p)
    (puthash "homepage" "https://github.com/org2blog/org2blog" p)
    p)
  "Internal package definition.")

(defun owp--pkg (key)
  "Get KEY from ‘owp--package’."
  (gethash key owp--package))

(defun org2blog--update-pkg ()
  "Update package definition."
  (interactive)
  (save-buffer)
  (save-excursion
    (with-current-buffer (find-file "org2blog-pkg.el")
      (erase-buffer)
      (pp
       `(define-package ,(owp--pkg "name") ,(owp--pkg "version") ,(owp--pkg "doc")
          ',(owp--pkg "requirements")
          :authors
          ',(owp--pkg "authors")
          :maintainer
          ',(owp--pkg "maintainer")
          :keywords
          ',(owp--pkg "keywords")
          :homepage
          ,(owp--pkg "homepage"))
       (current-buffer))
      (save-buffer))))

(provide 'org2blog-def)
;;; org2blog-def.el ends here
