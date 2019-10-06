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

(defconst org2blog-def--package
  (let ((p (make-hash-table :test 'equal)))
    (puthash "name" "org2blog" p)
    (puthash "version" "1.1.0" p)
    (puthash "doc" "Blog from Org mode to WordPress" p)
    (puthash "emacs" "26.3" p)
    (puthash "org" "9.1.9" p)
    (puthash "requirements"
             '((htmlize "1.54" "https://github.com/hniksic/emacs-htmlize.git")
               (hydra "0.15.0" "https://github.com/abo-abo/hydra.git")
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

(defun org2blog-def--pkg (key)
  "Get KEY from ‘org2blog-def--package’."
  (gethash key org2blog-def--package))

(defun org2blog-def-update-artifacts ()
  "Update dependent artifacts."
  (interactive)
  (org2blog-def--update-readme)
  (org2blog-def--update-header)
  (org2blog-def--update-pkg)
  (org2blog-def--update-oxwp))

(defun org2blog-def--update-readme ()
  "Update README.org"
  (interactive)
  (find-file "README.org")
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^Org2Blog requires Emacs")
    (kill-whole-line 1)
    (insert (format "Org2Blog requires Emacs %s and Org mode %s.\n"
                    (org2blog-def--pkg "emacs")
                    (org2blog-def--pkg "org")))))

(defun org2blog-def--contact-info (contact)
  "Create string from CONTACT info."
  (let ((result (concat (car contact) " <" (cdr contact) ">")))
    result))

(defun org2blog-def--interpose (sep list)
  "Return a new list of all elements in LIST separated by SEP."
  (let* ((it (mapcar (lambda (x) (list x sep)) list))
         (it (apply 'concatenate 'list it))
         (it (seq-take it (- (length it) 1))))
    it))

(defun org2blog-def--contacts-info (contacts)
  "Create string from CONTACTS info."
  (let* ((contacts (mapcar
                    'org2blog-def--contact-info
                    contacts))
         (separated (org2blog-def--interpose ", " contacts))
         (all (apply 'concat separated)))
    all))

(defun org2blog-def--update-header ()
  "Update Org2Blog file header."
  (interactive)
  (find-file "org2blog.el")
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^;; Author: ")
    (kill-whole-line 6)
    (insert (format ";; Author: %s\n" (org2blog-def--contacts-info (org2blog-def--pkg "authors"))))
    (insert (format ";; Maintainer: %s\n" (org2blog-def--contact-info
                                           (org2blog-def--pkg "maintainer"))))
    (insert (format ";; Version: %s\n" (org2blog-def--pkg "version")))
    (insert (format ";; Package-Requires: (%s)\n"
                    (let* ((ls (cons (cons 'emacs (list (org2blog-def--pkg "emacs")))
                                     (org2blog-def--pkg "requirements")))
                           (defs (mapcar (lambda (req)
                                           (format "(%s \"%s\")" (car req) (cadr req)))
                                         ls))
                           (spcd (org2blog-def--interpose " " defs))
                           (result (apply 's-concat spcd)))
                      result)))
    (insert (format ";; Keywords: %s\n"
                    (apply 'concat (org2blog-def--interpose ", " (org2blog-def--pkg "keywords")))))
    (insert (format ";; Homepage: %s\n" (org2blog-def--pkg "homepage")))))

(defun org2blog-def--update-pkg ()
  "Update package definition."
  (interactive)
  (save-buffer)
  (save-excursion
    (with-current-buffer (find-file "org2blog-pkg.el")
      (erase-buffer)
      (pp
       `(define-package ,(org2blog-def--pkg "name") ,(org2blog-def--pkg "version") ,(org2blog-def--pkg "doc")
          ',(org2blog-def--pkg "requirements")
          :authors
          ',(org2blog-def--pkg "authors")
          :maintainer
          ',(org2blog-def--pkg "maintainer")
          :keywords
          ',(org2blog-def--pkg "keywords")
          :homepage
          ,(org2blog-def--pkg "homepage"))
       (current-buffer))
      (save-buffer))))

(defun org2blog-def--update-oxwp ()
  "Update ox-wp defgroup."
  (interactive)
  (find-file "ox-wp.el")
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^(defgroup org-export-wp nil")
    (kill-whole-line 6)
    (insert
     (format
      "(defgroup org-export-wp nil
  \"WordPress specific export options.\"
  :tag \"Org WordPress\"
  :group 'org-export
  :version \"%s\"
  :package-version '(Org . \"%s\"))
"
      (org2blog-def--pkg "emacs")
      (org2blog-def--pkg "org")))))

(defun org2blog-def-checkout-statement ()
  "Create Git checkout commands for system code and packages into INSTALL-DIR.

Copy them from the *Messages* buffer into your Terminal."
  (interactive)
  (let ((install-dir (read-directory-name "Directory:")))
    (mapcar (lambda (pkg) (princ (format
                             "git clone %s %s%s\n"
                             (caddr pkg)
                             install-dir
                             (car pkg))))
            (org2blog-def--pkg "requirements"))))

(defun org2blog-def-load-statement ()
  "Create Elisp code to load the libraries."
  (interactive)
  (let* ((install-dir (read-directory-name "Directory:"))
         (install-dir (concat install-dir
                              (if (string-match "/\\'"
                                                install-dir)
                                  "" "/"))))
    (mapcar (lambda (pkg)
              (princ (format "(add-to-list 'load-path \"%s%s\")\n"
                             install-dir
                             (car pkg)))
              (princ (format "(require '%s)\n" (car pkg))))
            (org2blog-def--pkg "requirements"))))

(provide 'org2blog-def)
;;; org2blog-def.el ends here
