;;; org2blog.el --- Blog from Org mode to WordPress -*- lexical-binding: t; byte-compile-warnings: (not docstrings); -*-

;; Copyright (C) 2008 Ashish Shukla
;; Copyright (C) 2010 Benjamin Beckwith <bnbeckwith@gmail.com>
;; Copyright (C) 2010 Marcel van der Boom <marcel@hsdev.com>
;; Copyright (C) 2010-2014 Puneeth Chaganti <punchagan+org2blog@muse-amuse.in>
;; Copyright (C) 2010 Sacha Chua <sacha@sachachua.com>
;; Copyright (C) 2010 Giovanni Moretti <Giovanni@reflections.co.nz>
;; Copyright (C) 2010 Matt Price <matt@roke.mercey.dyndns.org>
;; Copyright (C) 2011 Mykola Nikishov <mn@mn.com.ua>
;; Copyright (C) 2013 Peter Vasil <mail@petervasil.net>
;; Copyright (C) 2015-2022 Grant Rettke <grant@wisdomandwonder.com>

;; Author: Puneeth Chaganti <punchagan+org2blog@gmail.com>
;; Maintainer: Grant Rettke <grant@wisdomandwonder.com>
;; Version: 1.1.16
;; Package-Requires: ((emacs "27.1") (htmlize "1.56") (hydra "0.15.0") (xml-rpc "1.6.15") (writegood-mode "2.2.0") (metaweblog "1.1.16"))
;; Keywords: comm, convenience, outlines, wp
;; Homepage: https://github.com/org2blog/org2blog

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

;; A portion of the code in this file is based on blog.el posted to
;; http://www.mail-archive.com/gnu-emacs-sources@gnu.org/msg01576.html
;; copyrighted by Ashish Shukla.

;;;; Code:

;;; System Definition

(defconst org2blog/wp-version "1.1.16"
  "Current version of org2blog.el.")

(cl-defstruct
    org2blog-pkg
  name
  version
  (version-prefix "")
  url)

(cl-defstruct
    org2blog-def
  name
  version
  wordpress-version
  metaweblog
  ox-wp
  doc
  emacs
  emacs-api
  org
  requirements
  keywords
  authors
  maintainer
  homepage)

(defconst org2blog-defi
  (make-org2blog-def
   :name "org2blog"
   :version org2blog/wp-version
   :metaweblog org2blog/wp-version
   :wordpress-version "6.0"
   :ox-wp org2blog/wp-version
   :doc "Blog from Org mode to WordPress"
   :emacs "28.1"
   :emacs-api "27.1"
   :org "9.5.2"
   :requirements (list
                  (make-org2blog-pkg
                   :name "htmlize"
                   :version "1.56"
                   :version-prefix "release/"
                   :url "https://github.com/hniksic/emacs-htmlize.git")
                  (make-org2blog-pkg
                   :name "hydra"
                   :version "0.15.0"
                   :url "https://github.com/abo-abo/hydra.git")
                  (make-org2blog-pkg
                   :name "xml-rpc"
                   :version "1.6.15"
                   :url "https://github.com/hexmode/xml-rpc-el.git")
                  (make-org2blog-pkg
                   :name "writegood-mode"
                   :version "2.2.0"
                   :version-prefix "v"
                   :url "https://github.com/bnbeckwith/writegood-mode")
                  (make-org2blog-pkg
                   :name "metaweblog"
                   :version org2blog/wp-version
                   :version-prefix "v"
                   :url "https://github.com/org2blog/org2blog.git"))
   :keywords '("comm" "convenience" "outlines" "wp")
   :authors '(("Puneeth Chaganti" . "punchagan+org2blog@gmail.com"))
   :maintainer '("Grant Rettke" . "grant@wisdomandwonder.com")
   :homepage "https://github.com/org2blog/org2blog"))

(defun org2blog-def-update-artifacts ()
  "Update dependent artifacts with version information.

Use data from `org2blog-def--package'.

Before calling this update and evaluate `org2blog-def--package'
with the new release version number.

This function requires that you are calling it while visiting a
file located in the project's top level directory because it
opens all of the files relatively.

It also leaves the file buffers open because you probably want to
inspect the generated code."
  (interactive)
  (org2blog-def--update-readme)
  (org2blog-def--update-org2blog)
  (org2blog-def--update-ox-wp)
  (org2blog-def--update-metaweblog)
  (org2blog-def--update-pkg))

(defmacro org2blog-def--update-the (file &rest body)
  "Visit FILE and evaluate BODY."
  `(save-excursion
     (save-buffer)
     (let ((origin (current-buffer)))
       (find-file ,file)
       (save-excursion
         ,@body)
       (save-buffer)
       (unless (eq origin (current-buffer))
         (previous-buffer)))))

(defun org2blog-def--update-readme ()
  "Update README.org."
  (interactive)
  (org2blog-def--update-the
   "README.org"
   (goto-char (point-min))
   (re-search-forward "^Current Requirements: Org2Blog")
   (kill-whole-line 1)
   (insert (format "Current Requirements: Org2Blog %s, Emacs %s, which includes Org mode %s, and WordPress %s.\n"
                   (org2blog-def-version org2blog-defi)
                   (org2blog-def-emacs org2blog-defi)
                   (org2blog-def-org org2blog-defi)
                   (org2blog-def-wordpress-version org2blog-defi))))
  (org2blog-def--update-the
   "README.org"
   (goto-char (point-min))
   (re-search-forward "^Start by installing Emacs Version ")
   (kill-whole-line 1)
   (insert (format "Start by installing Emacs Version %s.\n"
                   (org2blog-def-emacs org2blog-defi)))))

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

(defun org2blog-def--update-header (file version requirements keywords)
  "Update FILE header with VERSION, REQUIREMENTS, and KEYWORDS."
  (interactive)
  (org2blog-def--update-the
   file
   (goto-char (point-min))
   (re-search-forward "^;; Author: ")
   (kill-whole-line 6)
   (insert (format ";; Author: %s\n" (org2blog-def--contacts-info
                                      (org2blog-def-authors org2blog-defi))))
   (insert (format ";; Maintainer: %s\n" (org2blog-def--contact-info
                                          (org2blog-def-maintainer org2blog-defi))))
   (insert (format ";; Version: %s\n" version))
   (insert (format ";; Package-Requires: ((emacs \"%s\")"
                   (org2blog-def-emacs-api org2blog-defi)))
   (mapc
    (lambda (req)
      (let* ((name (org2blog-pkg-name req))
             (version (org2blog-pkg-version req))
             (str (format " (%s \"%s\")" name version)))
        (insert str)))
    requirements)
   (insert (format ")\n"))
   (insert (format ";; Keywords: %s\n"
                   (apply 'concat (org2blog-def--interpose ", " keywords))))
   (insert (format ";; Homepage: %s\n" (org2blog-def-homepage org2blog-defi)))))

(defun org2blog-def--update-org2blog ()
  "Update Org2Blog file."
  (interactive)
  (org2blog-def--update-header
   "org2blog.el"
   (org2blog-def-version org2blog-defi)
   (org2blog-def-requirements org2blog-defi)
   (org2blog-def-keywords org2blog-defi)))

(defun org2blog-def--update-pkg ()
  "Update package definition."
  (interactive)
  (org2blog-def--update-the
   "org2blog-pkg.el"
   (erase-buffer)
   (pp
    `(define-package ,(org2blog-def-name org2blog-defi) ,(org2blog-def-version org2blog-defi) ,(org2blog-def-doc org2blog-defi)
       ',(seq-mapcat
          (lambda (req)
            (list
             (list (intern (org2blog-pkg-name req))
                   (org2blog-pkg-version req)
                   (org2blog-pkg-url req))))
          (org2blog-def-requirements org2blog-defi))
       :authors
       ',(org2blog-def-authors org2blog-defi)
       :maintainer
       ',(org2blog-def-maintainer org2blog-defi)
       :keywords
       ',(org2blog-def-keywords org2blog-defi)
       :homepage
       ,(org2blog-def-homepage org2blog-defi))
    (current-buffer))))

(defun org2blog-def--update-ox-wp ()
  "Update ox-wp file."
  (interactive)
  (let ((file "ox-wp.el"))
    (org2blog-def--update-header
     file
     (org2blog-def-version org2blog-defi)
     nil
     (org2blog-def-keywords org2blog-defi))
    (org2blog-def--update-the
     file
     (goto-char (point-min))
     (re-search-forward "(defconst ox-wp-version")
     (kill-whole-line 2)
     (insert (format "(defconst ox-wp-version \"%s\"\n"
                     (org2blog-def-ox-wp org2blog-defi)))
     (insert (format "  \"Current version of ox-wp.el.\")\n")))))

(defun org2blog-def--update-metaweblog ()
  "Update metaweblog file."
  (interactive)
  (let ((file "metaweblog.el"))
    (org2blog-def--update-header
     file
     (org2blog-def-metaweblog org2blog-defi)
     nil
     '("comm"))
    (org2blog-def--update-the
     file
     (goto-char (point-min))
     (re-search-forward "(defconst metaweblog-version")
     (kill-whole-line 2)
     (insert (format "(defconst metaweblog-version \"%s\"\n"
                     (org2blog-def-metaweblog org2blog-defi)))
     (insert (format "  \"Current version of metaweblog.el.\")\n")))))

(defun org2blog-def-checkout-statement ()
  "Create Git checkout commands for system code and packages into INSTALL-DIR.

  Copy them from the *Messages* buffer into your Terminal."
  (interactive)
  (let ((install-dir (read-directory-name "Directory:")))
    (princ (format "cd %s\n" install-dir))
    (mapcar
     (lambda (req)
       (let* ((name (org2blog-pkg-name req))
              (version (org2blog-pkg-version req))
              (version-prefix (org2blog-pkg-version-prefix req))
              (url (org2blog-pkg-url req)))
         (princ (format
                 "git clone %s %s/%s\n"
                 url
                 install-dir
                 name))
         (princ (format "cd %s\n" name))
         (princ (format
                 "git checkout %s%s\n"
                 version-prefix
                 version))
         (princ (format "cd ..\n" name))))
     (org2blog-def-requirements org2blog-defi))))

(defun org2blog-def-load-statement ()
  "Create Elisp code to load the libraries."
  (interactive)
  (let* ((install-dir (read-directory-name "Directory:"))
         (install-dir (concat install-dir
                              (if (string-match "/\\'"
                                                install-dir)
                                  "" "/"))))
    (mapcar
     (lambda (req)
       (let* ((name (org2blog-pkg-name req)))
         (princ (format "(add-to-list 'load-path \"%s%s\")\n"
                        install-dir
                        name))
         (princ (format "(require '%s)\n" name))))
     (org2blog-def-requirements org2blog-defi))))

(defun org2blog-estimated-word-count (&optional subtreep)
  "Estimate word count using a UTF-8 text export.

Review the export by opening the buffer:
'(switch-to-buffer-other-window \"*Org ASCII Export*\"))'

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.
"
  (interactive)
  (let ((async nil)
        (visible-only nil)
        (body-only nil)
        (ext-plist
         '(:ascii-charset utf-8))
        (org-export-show-temporary-export-buffer nil)
        (export-buffer "*Org ASCII Export*"))
    (org-ascii-export-as-ascii
     async subtreep visible-only body-only ext-plist)
    (with-current-buffer export-buffer
      (let ((word-count
             (count-words (point-min) (point-max))))
        (message "Estimated Word Count: %s" word-count)))
    (when (get-buffer export-buffer) (kill-buffer export-buffer))))

;;; Requires

;; Packages - External

(require 'htmlize)
(require 'hydra)
(require 'xml-rpc)
(require 'metaweblog)
(require 'writegood-mode)

;; Packages - Internal

(require 'org)
(require 'ox-wp)
(eval-when-compile (require 'subr-x))

;;; Constants

(defconst org2blog/wp-required-org-version (org2blog-def-org org2blog-defi)
  "Minimum variable ‘org-version’ required to run this package.")

(defconst org2blog--minimal-emacs (org2blog-def-emacs org2blog-defi)
  "Minimum variable ‘emacs-version’ required to run this package.")

(defconst org2blog--default-blogid "1"
  "Default WordPress Blog ID.

Nearly every XML-RPC API call requires the blog id.
What exactly is BLOGID?

There are two ways to run a WordPress site. Number one is a
single “site” with one installation directory and one blog.
Number two is a Multisite “network of sites” mode with one
installation hosting any number of blogs.

When you talk to WordPress over its API you need to tell it
which blog you are talking to. If you are running a “site”
then it will be ID “1”. If you are running a Multisite then
it will be “1” or above.

The most likely case for bloggers is a single “site” setup.
Therefore after you login and Org2Blog can’t find an ‘:id’,
then it defaults to this value: it defaults to “1”.
“1”.

The blog specific property is: :id

Example: An integer defined by a string.")

(defconst org2blog-deprecation "2.0.0"
  "Release in which obsolete objects will be removed.")

;;; Deprecations

(mapc (lambda (ls)
        (define-obsolete-function-alias (car ls) (cadr ls) org2blog-deprecation))
      '((org2blog/wp-buffer-kill-prompt org2blog-on-new-entry-kill)
        (org2blog/wp-complete-category org2blog-complete)
        (org2blog/wp-delete-entry org2blog-post-trash)
        (org2blog/wp-delete-page org2blog-page-trash)
        (org2blog/wp-format-buffer org2blog-entry-buffer-make)
        (org2blog/wp-insert-post-or-page-link org2blog-insert-link)
        (org2blog/wp-login org2blog-user-login)
        (org2blog/wp-logout org2blog-user-logout)
        (org2blog/wp-new-entry org2blog-buffer-new)
        (org2blog/wp-org-mode-hook-fn org2blog-maybe-start)
        (org2blog/wp-password org2blog-user-set-password)
        (org2blog/wp-post-buffer org2blog-buffer-post-save)
        (org2blog/wp-post-buffer-and-publish org2blog-buffer-post-publish)
        (org2blog/wp-post-buffer-as-page org2blog-buffer-page-save)
        (org2blog/wp-post-subtree org2blog-subtree-post-save)
        (org2blog/wp-post-subtree-and-publish org2blog-subtree-post-publish)
        (org2blog/wp-post-subtree-as-page org2blog-subtree-page-save)
        (org2blog/wp-post-subtree-as-page-and-publish org2blog-subtree-page-publish)
        (org2blog/wp-preview-buffer-post org2blog-buffer-post-or-page-view)
        (org2blog/wp-preview-subtree-post org2blog-subtree-post-or-page-view)
        (org2blog/wp-reload-entry-mode-map org2blog-reload-entry-mode-map)
        (org2blog/wp-track-buffer org2blog-buffer-track)
        (org2blog/wp-track-subtree org2blog-subtree-track)))

(mapc (lambda (ls)
        (define-obsolete-variable-alias (car ls) (cadr ls) org2blog-deprecation))
      '((org2blog/wp-after-new-post-or-page-functions org2blog-buffer-entry-save-hook)
        (org2blog/wp-blog org2blog-blog)
        (org2blog/wp-blog-name org2blog-blog-key)
        (org2blog/wp-buffer-name org2blog-buffer-name)
        (org2blog/wp-categories-list org2blog-categories)
        (org2blog/wp-entry-mode-map org2blog-mode-map)
        (org2blog/wp-export-options org2blog-export-options)
        (org2blog/wp-logged-in org2blog-logged-in)
        (org2blog/wp-mode-hook org2blog-mode-hook)
        (org2blog/wp-pages-list org2blog-pages)
        (org2blog/wp-server-blogid org2blog-blogid)
        (org2blog/wp-server-pass org2blog-password)
        (org2blog/wp-server-userid org2blog-username)
        (org2blog/wp-server-xmlrpc-url org2blog-xmlrpc)
        (org2blog/wp-tags-list org2blog-tags)))

;;; Variables

(defvar org2blog-blog nil
  "Parameters of the currently selected blog.")

(defvar org2blog-blog-key nil
  "Name of the blog, to pick from `org2blog/wp-blog-alist'.")

(defvar org2blog-categories nil
  "List of weblog categories.")

(defvar org2blog-tags nil
  "List of weblog tags.")

(defvar org2blog-pages nil
  "List of WP pages.")

(defvar org2blog-xmlrpc nil
  "WordPress server XML-RPC URL.

The blog specific property is: :url

Example: \"https://www.wisdomandwonder.com/xmlrpc.php\"")

(defvar org2blog-username nil
  "WordPress server user id.

The blog specific property is: :url

Example: \"admin\"")

(defvar org2blog-blogid nil
  "WordPress Blog ID.")

(defvar org2blog-mode-map nil
  "Keymap for blog entry buffer.")

(defvar org2blog-logged-in nil
  "Flag whether user is logged-in or not.")

(defvar org2blog-buffer-name "*Org2Blog (%s): %s*"
  "Name of the blog buffer.")

(defvar org2blog-mode-hook nil
  "Hook to run upon entry into mode.
Here is an example of creating keybindings:

  (defun ahook ()
    (local-set-key (kbd \"M-9\") #'org2blog-user-interface)
    (local-set-key (kbd \"M-0\") #'org2blog-complete))
  (add-hook 'org2blog/wp-mode-hook #'ahook).")

(defvar org2blog-buffer-entry-save-hook nil
  "Hooks run after a new post or page save.

Each function is called with one argument, the object
representing the aforementioned post or page.

Here is an example that outputs the entire object to the *Messages* buffer:

  (defun ahook (entry)
    (pp entry))

  (add-hook 'org2blog-buffer-entry-save-hook #'ahook).")

(defvar org2blog-export-options
  '(:section-numbers
    nil
    :with-priority nil
    :with-sub-superscript nil
    :with-toc nil
    :with-tags nil
    :with-todo-keywords nil)
  "Export options to be used when exporting buffers and subtrees.
Look at `org-export-options-alist' for the available options.
Also, note that these options are over-ridden by in-file
options.")

(defvar org2blog-password nil
  "WordPress user password.

The blog specific property is: :password

Example: \"bilbo\"")

(defvar org2blog-step-time 0.2 "Number of seconds to sleep between actions.

Must be greater than or equal to 0.2 seconds.")

;;; Groups

(defgroup org2blog/wp nil
  "Blog from Org mode to WordPress"
  :group 'org2blog/wp)

;;; Customize

(defcustom org2blog/wp-blog-alist nil
  "User blog definitions.

Association list to set information for each blog.
Each element of the alist is a blog name.  The CAR of each
element is a string, uniquely identifying the project.  The CDR
of each element is a well-formed property list with an even
number of elements, alternating keys and values, specifying
parameters for the blog.

  (:property value :property value ... )

When a property is given a value in org2blog/wp-blog-alist, its
setting overrides the value of the corresponding user
variable (if any) during publishing.

  :url and :username are required.

All the other properties are optional. They over-ride the global variables.

  :group 'org2blog/wp
  :type '(alist :value-type plist)

Example:
  (\"myblog\"
    :url \"https://www.wisdomandwonder.com/xmlrpc.php\"
    :username username
    :password password
    :confirm t)"
  :group 'org2blog/wp
  :type '(alist))

(defcustom org2blog/wp-default-categories '("Org2Blog" "WordPress")
  "Default list of categories for a new buffer entry.

The blog specific property is: :default-categories

Example: '(\"category 1\" \"category 2\")"
  :group 'org2blog/wp
  :type '(repeat string))

(defcustom org2blog/wp-default-categories-subtree '("Org2Blog" "WordPress")
  "Default list of categories for a new subtree entry.

The blog specific property is: :default-categories-sub

Example: See default value."
  :group 'org2blog/wp
  :type '(repeat string))

(defcustom org2blog/wp-default-tags '("Emacs" "Lisp")
  "Default list of tags for a new buffer entry.

The blog specific property is: :default-tags

Example: '(\"tag 1\" \"tag 2\")"
  :group 'org2blog/wp
  :type '(repeat string))

(defcustom org2blog/wp-default-tags-subtree '("Emacs" "Lisp")
  "Default list of tags for a new subtree entry.

The blog specific property is: :default-tags-sub

Example: '(\"tag 1\" \"tag 2\")"
  :group 'org2blog/wp
  :type '(repeat string))

(defcustom org2blog/wp-buffer-template
  "#+ORG2BLOG:
#+DATE: %s
#+OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil
#+CATEGORY: %s
#+TAGS: %s
#+DESCRIPTION:
#+TITLE: %s
\n"
  "The default template to be inserted in a new buffer entry.

It is passed to ‘format’ with 4 string arguments:
- Today’s date and time
- Your configuration of default buffer entry categories
- Your configuration of default buffer entry tags
- Your configuration of default buffer entry title."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-buffer-template-prefix nil
  "A prefix to the default template used for a new post buffer."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-buffer-subtree-template
  "#+ORG2BLOG

* %s
:PROPERTIES:
:BLOG: %s
:DATE: %s
:OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil
:CATEGORY: %s
:POST_TAGS: %s
:END:\n\n"
  "The default template to be inserted in a new subtree entry.

It is passed to ‘format’ with 5 string arguments:
- Your configuration of default subtree title.
- Your Blog ID.
- Today’s date and time
- Your configuration of default subtree entry categories.
- Your configuration of default subtree entry tags."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-buffer-subtree-template-prefix nil
  "A prefix to the default template used for a new subtree entry."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-buffer-format-function 'org2blog-entry-buffer-make
  "Function formatting a buffer according to `org2blog/wp-buffer-template'."
  :group 'org2blog/wp
  :type 'function)

(defcustom org2blog/wp-buffer-subtree-format-function 'org2blog-entry-subtree-make
  "Function formatting an entry according to `org2blog/wp-buffer-subtree-template'."
  :group 'org2blog/wp
  :type 'function)

(defcustom org2blog/wp-default-title "Hello, Buffer"
  "Title of a newly generated buffer entry.

The blog specific property is: :default-title

Example: See default value."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-default-title-subtree "Hello, Subtree"
  "Title of a newly generated subtree entry.

The blog specific property is: :default-title-sub

Example: See default value"
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-use-tags-as-categories nil
  "Non-nil means assign :tags: to WordPress categories instead.

The blog specific property is: :tags-as-categories

Example: A Boolean value."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-confirm-post nil
  "Non-nil means confirm before Publishing a post or page.

The blog specific property is: :confirm

Example: A Boolean value."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-safe-trash t
  "Non-nil means confirm before Trashing a post or page.

The blog specific property is: :safe-trash

Example: A Boolean value."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-safe-new-entry-buffer-kill t
  "Non-nil means confirm before killing a new entry buffer.

The blog specific property is: :safe-new-entry-buf-kill

Example: A Boolean value."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-show-post-in-browser 'dont
  "How to automatically display an entry after save or publish.

Eventually you will have a lot of examples of how you prefer to
blog. This is your “personal workflow” and this option should
reflect that.

This variable is a symbol with options:

  - ask ::  Ask you whether to display it or not.
            This is useful when your workflow is to make
            continuous changes that you just don’t know
            whether or not you want to display it each time.
  - show :: Show it immediately.
            This is useful your workflow is to write your entry
            once and basically have it be perfect on the first
            try. That way you save it, review it, see that
            it looks good, publish it, and you are done.
  - dont :: Don’t show it at all.
            This is useful when your workflow is to display
            your entry once and manually refresh the page
            yourself after saving or publishing. If you’ve
            blogged before then this is the easiest and least
            surprising approach.

The blog specific property is: :show"
  :group 'org2blog/wp
  :type 'symbol)

(defcustom org2blog/wp-keep-new-lines nil
  "Non-nil means do not strip newlines.

When Org mode exports to HTML it removed line endings so
the web page “looks right”. If for some reason you don’t
what that typical behavior set this to program t.

The blog specific property is: :keep-new-lines

Example: A boolean value."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-use-sourcecode-shortcode nil
  "Non-nil means convert <pre> tags to WP sourcecode blocks.

The blog specific property is: :wp-code"
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-use-wp-latex t
  "Non-nil means convert LaTeX to WP LaTeX blocks.

The blog specific property is: :wp-latex

Example: A boolean value."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-shortcode-langs-map nil
  "Map source language to SyntaxHighlighter Evolved.

Each element of the list maps the Org mode source code
language (key) to the language spec that should be used
for syntax highlighting in shortcode blocks.

Read more about this here:

- SyntaxHighlighter Evolved :
  URL ‘https://wordpress.org/plugins/syntaxhighlighter/’
- Org mode source blocks:
  URL ‘https://orgmode.org/manual/Working-with-source-code.html’"
  :group 'org2blog/wp
  :type '(alist :key-type string :value-type string))

(defcustom org2blog/wp-track-posts
  (list ".org2blog.org" "Posts")
  "Tracking file name location and parent headline.

.org file in which to save logs about posts, and
corresponding headline in file under which the logs should
be added.

The blog specific property is: :track-posts

Example: See default value."
  :group 'org2blog/wp
  :type '(list string string))

(defcustom org2blog/wp-keymap-prefix
  "C-c M-p"
  "Mode keymap prefix.

Call `org2blog-reload-entry-mode-map' after making
change for them to takes effect."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-image-upload nil
  "Non-nil means upload supported Org mode image types.

Upload to the WordPress Media Library.

The blog specific property is: :image-upload

Example: A boolean value."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-image-thumbnails nil
  "Non-nil means WordPress inserts a thumbnail link to a full-size image."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-image-thumbnail-size "medium_large"
  "Default image thumbnail size.

  Choices are: thumbnail (150px), medium (300px),
  medium_large (768px) or large (1024px)"
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/link-selection-size 100
  "Number of most recent entries to present for insertion.

function ‘org2blog-insert-link’ inserts an
Org link for an entry ID. Retrieve the
variable ‘org2blog/link-selection-size' most recent entries
to present to the user for selection.

It is only as useful as quickly it helps the writer.
If you set the value too high it will take too long to
load and interrupt the authors flow. If you set the value
too low then it won’t help for looking things up forcing
them to just open the blog in a browser and search for the
entry.

Since usage style is a personal preference, the default setting
here seemed to be a good balance between speed and value(s)."
  :group 'org2blog/wp
  :type 'integer)

;;; User Interface

(defun org2blog--hlpf (name)
  "Abstracts displaying information about symbol NAME."
  (describe-symbol name))

(defun org2blog--hlpv (name)
  "Abstracts displaying information about variable NAME."
  (describe-variable name))

(defconst org2blog--hydra-letters-main-menu
  "
Char : ABCDE FGHIJ KLMNO PQRST UVWXY Z1234 56789 0`-=[ ]\;', ./
None : XoXXX XoXXX XXooX XXXoo XXXoo oXXXX ooooo ooooo ooXoo oo
Shift: ooooo oooXX XXooX Xoooo XXXoo ooooo ooooo ooooo ooXoo oo
"
  "Documents Main Menu Shortcut Letters

This is for the top-level main-menu.

Review this before making changes to it―
document this after making them.

Legend:
  Columns: Key name and modifier applied
  Rows:    Key itself

`o': Un-used
`X': Used")

(defhydra org2blog--hydra-main (:color blue :hint nil)
  "
╔═^═════════╗
║ ^Org2Blog ║^ (Main Menu) %(org2blog--login-status)^
╠═^═════════╩^═══════╦══════════════^═══^╦════════^════════════^╦═^═══════^═══════════^═╗
║ ^Admin^            ║ ^Use^             ║ ^Buffer^             ║ ^Subtree^           ^ ║
╚═^═════^════════════╩═^═══^═════════════╩═^══════^═════════════╩═^═══════^═══════════^═╝
 [_4_] Login          [_e_] New Buffer    [_j_] Save Post Draft  [_u_] Save Post Draft
 [_3_] Reporting On   [_r_] New Subtree   [_k_] View Post        [_i_] View Post
 [_2_] Reporting Off  [_c_] Completion    [_l_] Publish Post     [_o_] Publish Post
 [_1_] Logout         [_f_] Trash ID      [_;_] Trash Post       [_p_] Trash Post
 [_a_] About          [_d_] Insert A      [_J_] Save Page Draft  [_U_] Save Page Draft
 [_W_] Version        [_v_] README        [_K_] View Page        [_I_] View Page
 [_h_] Help           [_V_] Variables     [_L_] Publish Page     [_O_] Publish Page
 [_q_] Quit           [_w_] Writer Tools  [_:_] Trash Page       [_P_] Trash Page
"
  ("4" org2blog-user-login :exit nil)
  ("3" org2blog-user-report-on :exit nil)
  ("2" org2blog-user-report-off :exit nil)
  ("1" org2blog-user-logout :exit nil)
  ("a" org2blog-about)
  ("W" org2blog-version-info)
  ("h" (org2blog--hydra-main-help/body))
  ("q" nil)

  ("e" org2blog-buffer-new)
  ("r" org2blog-subtree-new)
  ("c" org2blog-complete)
  ("f" org2blog-entry-trash-prompt)
  ("d" org2blog--hydra-main-inserts/body)
  ("v" org2blog-readme)
  ("V" org2blog--hydra-main-variables/body)
  ("w" org2blog--hydra-main-writer-tools/body)

  ("j" org2blog-buffer-post-save)
  ("k" org2blog-buffer-post-view)
  ("l" org2blog-buffer-post-publish)
  (";" org2blog-buffer-post-trash)

  ("J" org2blog-buffer-page-save)
  ("K" org2blog-buffer-page-view)
  ("L" org2blog-buffer-page-publish)
  (":" org2blog-buffer-page-trash)

  ("u" org2blog-subtree-post-save)
  ("i" org2blog-subtree-post-view)
  ("o" org2blog-subtree-post-publish)
  ("p" org2blog-subtree-post-trash)

  ("U" org2blog-subtree-page-save)
  ("I" org2blog-subtree-page-view)
  ("O" org2blog-subtree-page-publish)
  ("P" org2blog-subtree-page-trash))

(defhydra org2blog--hydra-main-help (:color blue :hint nil)
  "
╔═^═════════╗
║ ^Org2Blog ║^ (Main Menu → Help) Select any item for more detail
╠═^═════════╩^═══════╦══════════════^═══^╦════════^════════════^╦═^═══════^═══════════^═╗
║ ^Admin^            ║ ^Use^             ║ ^Buffer^             ║ ^Subtree^           ^ ║
╚═^═════^════════════╩═^═══^═════════════╩═^══════^═════════════╩═^═══════^═══════════^═╝
 [_4_] Login          [_e_] New Buffer    [_j_] Save Post Draft  [_u_] Save Post Draft
 [_3_] Reporting On   [_r_] New Subtree   [_k_] View Post        [_i_] View Post
 [_2_] Reporting Off  [_c_] Completion    [_l_] Publish Post     [_o_] Publish Post
 [_1_] Logout         [_f_] Trash ID      [_;_] Trash Post       [_p_] Trash Post
 [_a_] About          [_d_] Insert A      [_J_] Save Page Draft  [_U_] Save Page Draft
 [_W_] Version        [_v_] README        [_K_] View Page        [_I_] View Page
  ^ ^                 [_V_] Variables     [_L_] Publish Page     [_O_] Publish Page
 [_q_] Back           [_w_] Writer Tools  [_:_] Trash Page       [_P_] Trash Page
"
  ("4" (org2blog--hlpf 'org2blog-user-login))
  ("3" (org2blog--hlpf 'org2blog-user-report-on))
  ("2" (org2blog--hlpf 'org2blog-user-report-off))
  ("1" (org2blog--hlpf 'org2blog-user-logout))
  ("a" (org2blog--hlpf 'org2blog-about))
  ("W" (org2blog--hlpf 'org2blog-version-info))
  ("q" org2blog--hydra-main/body)

  ("e" (org2blog--hlpf 'org2blog-buffer-new))
  ("r" (org2blog--hlpf 'org2blog-subtree-new))
  ("c" (org2blog--hlpf 'org2blog-complete))
  ("f" (org2blog--hlpf 'org2blog-entry-trash-prompt))
  ("d" (org2blog--hlpf 'org2blog--main-inserts))
  ("v" (org2blog--hlpf 'org2blog-readme))
  ("V" (org2blog--hlpf 'org2blog--main-variables))
  ("w" (org2blog--hlpf 'org2blog--main-writer-tools))

  ("j" (org2blog--hlpf 'org2blog-buffer-post-save))
  ("k" (org2blog--hlpf 'org2blog-buffer-post-view))
  ("l" (org2blog--hlpf 'org2blog-buffer-post-publish))
  (";" (org2blog--hlpf 'org2blog-buffer-post-trash))

  ("J" (org2blog--hlpf 'org2blog-buffer-page-save))
  ("K" (org2blog--hlpf 'org2blog-buffer-page-view))
  ("L" (org2blog--hlpf 'org2blog-buffer-page-publish))
  (":" (org2blog--hlpf 'org2blog-buffer-page-trash))

  ("u" (org2blog--hlpf 'org2blog-subtree-post-save))
  ("i" (org2blog--hlpf 'org2blog-subtree-post-view))
  ("o" (org2blog--hlpf 'org2blog-subtree-post-publish))
  ("p" (org2blog--hlpf 'org2blog-subtree-post-trash))

  ("U" (org2blog--hlpf 'org2blog-subtree-page-save))
  ("I" (org2blog--hlpf 'org2blog-subtree-page-view))
  ("O" (org2blog--hlpf 'org2blog-subtree-page-publish))
  ("P" (org2blog--hlpf 'org2blog-subtree-page-trash)))

(defun org2blog--main-writer-tools ()
  "Open the “Writer Tools” menu."
  (org2blog--hydra-main-writer-tools/body))

(defhydra org2blog--hydra-main-writer-tools (:color blue :hint nil)
  "
╔══════════╗
║ ^Org2Blog^ ║ (Main Menu → Writer Tools)
╠══════════╩^════^═╦═^═══════════^══════════╦═^══════════^═══╗
║ ^Tools^          ║ ^Spell Check^          ║ ^Word Count^   ║
╚═^═════^══════════╩═^═══════════^══════════╩═^══════════^═══╝
 [_l_] Grade Level  [_b_] Buffer or Region   [_c_] Buffer
 [_m_] Writegood    [_w_] Word               [_C_] Subtree
  ^ ^               [_r_] Region
  ^ ^                 ^ ^
 [_h_] Help
 [_q_] Back
"
  ("l" writegood-reading-ease)
  ("m" writegood-mode)

  ("b" ispell)
  ("w" ispell-word)
  ("r" ispell-region)

  ("c" org2blog-estimated-word-count)
  ("C" (lambda () (org2blog-estimated-word-count t)))

  ("h" org2blog--hydra-main-writer-tools-help/body)
  ("q" org2blog--hydra-main/body))

(defhydra org2blog--hydra-main-writer-tools-help (:color blue :hint nil)
  "
╔══════════╗
║ ^Org2Blog^ ║ (Main Menu → Writer Tools)
╠══════════╩^════^═╦═^═══════════^══════════╦═^══════════^═══╗
║ ^Tools^          ║ ^Spell Check^          ║ ^Word Count^   ║
╚═^═════^══════════╩═^═══════════^══════════╩═^══════════^═══╝
 [_l_] Grade Level  [_b_] Buffer or Region   [_c_] Buffer
 [_m_] Writegood    [_w_] Word               [_C_] Subtree
  ^ ^               [_r_] Region
  ^ ^                 ^ ^
  ^ ^
 [_q_] Back
"
  ("l" (org2blog--hlpf 'writegood-reading-ease) :exit nil)
  ("m" (org2blog--hlpf 'writegood-mode) :exit nil)

  ("b" (org2blog--hlpf 'ispell) :exit nil)
  ("w" (org2blog--hlpf 'ispell-word) :exit nil)
  ("r" (org2blog--hlpf 'ispell-region) :exit nil)

  ("c" (org2blog--hlpf 'org2blog-estimated-word-count) :exit nil)
  ("C" (org2blog--hlpf 'org2blog-estimated-word-count) :exit nil)

  ("q" org2blog--hydra-main-writer-tools/body))

(defun org2blog--main-inserts ()
  "Open the Insert A menu."
  (org2blog--hydra-main-inserts/body))

(defhydra org2blog--hydra-main-inserts (:color blue :hint nil)
  "
  ╔══════════╗
  ║ Org2Blog ║ (Main Menu → Insert → Help) Select any item for more detail
  ╚══════════╩═══════╦═════════════^═════════^╗
                     ║ ^Insert A^             ║
                     ╚═^═══════════^══════════╝
                      [_m_] More Tag
                      [_t_] MathJax Shortcode
                      [_x_] LaTeX Name
                      [_r_] Link To Post
                      [_g_] Link To Page
                      [_o_] #+ORG2BLOG
                      [_c_] Unicode Char
                      [_l_] Template
  [_h_] Help          ^ ^
  [_q_] Back          ^ ^
  "
  ("m" org2blog-insert-more)
  ("t" org2blog-insert-mathjax)
  ("x" org2blog-insert-latex)
  ("r" org2blog-insert-link-to-post)
  ("g" org2blog-insert-link-to-page)
  ("o" org2blog-org2blog-keyword-check)
  ("c" insert-char)
  ("l" org-insert-structure-template)

  ("h" org2blog--hydra-main-inserts-help/body)
  ("q" org2blog--hydra-main/body))

(defhydra org2blog--hydra-main-inserts-help (:color blue :hint nil)
  "
  ╔══════════╗
  ║ Org2Blog ║ (Main Menu → Insert → Help) Select any item for more detail
  ╚══════════╩═══════╦═════════════^═════════^╗
                     ║ ^Insert A^             ║
                     ╚═^═══════════^══════════╝
                      [_m_] More Tag
                      [_t_] MathJax Shortcode
                      [_x_] LaTeX Name
                      [_r_] Link To Post
                      [_g_] Link To Page
                      [_o_] #+ORG2BLOG
                      [_c_] Unicode Char
                      [_l_] Template

  [_q_] Back          ^ ^
  "
  ("m" (org2blog--hlpf 'org2blog-insert-more) :exit nil)
  ("t" (org2blog--hlpf 'org2blog-insert-mathjax) :exit nil)
  ("x" (org2blog--hlpf 'org2blog-insert-latex) :exit nil)
  ("r" (org2blog--hlpf 'org2blog-insert-link-to-post) :exit nil)
  ("g" (org2blog--hlpf 'org2blog-insert-link-to-page) :exit nil)
  ("o" (org2blog--hlpf 'org2blog-org2blog-keyword-check) :exit nil)
  ("c" (org2blog--hlpf 'insert-char) :exit nil)
  ("l" (org2blog--hlpf 'org-insert-structure-template) :exit nil)
  ("q" org2blog--hydra-main-inserts/body))

(defun org2blog--main-variables ()
  "Open the Variables menu."
  (org2blog--hydra-main-variables/body))

(defhydra org2blog--hydra-main-variables (:color blue :columns 2)
  "
  ╔══════════╗
  ║ Org2Blog ║ (Main Menu → Variables) Select any item for more detail
  ╚══════════╩═════════════════════════════════════════════════════════╝
  "
  ("aa" (org2blog--hlpv 'org2blog/link-selection-size)
   "org2blog/link-selection-size")
  ("ab" (org2blog--hlpv 'org2blog/wp-blog-alist)
   "org2blog/wp-blog-alist")
  ("ac" (org2blog--hlpv 'org2blog/wp-buffer-format-function)
   "org2blog/wp-buffer-format-function")
  ("ad" (org2blog--hlpv 'org2blog/wp-buffer-subtree-format-function)
   "org2blog/wp-buffer-subtree-format-function")
  ("ae" (org2blog--hlpv 'org2blog/wp-buffer-subtree-template)
   "org2blog/wp-buffer-subtree-template")
  ("af" (org2blog--hlpv 'org2blog/wp-buffer-subtree-template-prefix)
   "org2blog/wp-buffer-subtree-template-prefix")
  ("ag" (org2blog--hlpv 'org2blog/wp-buffer-template)
   "org2blog/wp-buffer-template")
  ("ah" (org2blog--hlpv 'org2blog/wp-buffer-template-prefix)
   "org2blog/wp-buffer-template-prefix")
  ("ai" (org2blog--hlpv 'org2blog/wp-confirm-post)
   "org2blog/wp-confirm-post")
  ("aj" (org2blog--hlpv 'org2blog/wp-default-categories)
   "org2blog/wp-default-categories")
  ("ak" (org2blog--hlpv 'org2blog/wp-default-categories-subtree)
   "org2blog/wp-default-categories-subtree")
  ("al" (org2blog--hlpv 'org2blog/wp-default-tags)
   "org2blog/wp-default-tags")
  ("am" (org2blog--hlpv 'org2blog/wp-default-tags-subtree)
   "org2blog/wp-default-tags-subtree")
  ("an" (org2blog--hlpv 'org2blog/wp-default-title)
   "org2blog/wp-default-title")
  ("ao" (org2blog--hlpv 'org2blog/wp-default-title-subtree)
   "org2blog/wp-default-title-subtree")
  ("ap" (org2blog--hlpv 'org2blog/wp-image-upload)
   "org2blog/wp-image-upload")
  ("aq" (org2blog--hlpv 'org2blog/wp-image-thumbnail-size)
   "org2blog/wp-image-thumbnail-size")
  ("ar" (org2blog--hlpv 'org2blog/wp-image-thumbnails)
   "org2blog/wp-image-thumbnails")
  ("as" (org2blog--hlpv 'org2blog/wp-keep-new-lines)
   "org2blog/wp-keep-new-lines")
  ("at" (org2blog--hlpv 'org2blog/wp-keymap-prefix)
   "org2blog/wp-keymap-prefix")
  ("au" (org2blog--hlpv 'org2blog/wp-safe-new-entry-buffer-kill)
   "org2blog/wp-safe-new-entry-buffer-kill")
  ("av" (org2blog--hlpv 'org2blog/wp-safe-trash)
   "org2blog/wp-safe-trash")
  ("aw" (org2blog--hlpv 'org2blog/wp-shortcode-langs-map)
   "org2blog/wp-shortcode-langs-map")
  ("ax" (org2blog--hlpv 'org2blog/wp-show-post-in-browser)
   "org2blog/wp-show-post-in-browser")
  ("ay" (org2blog--hlpv 'org2blog/wp-track-posts)
   "org2blog/wp-track-posts")
  ("az" (org2blog--hlpv 'org2blog/wp-use-sourcecode-shortcode)
   "org2blog/wp-use-sourcecode-shortcode")
  ("ba" (org2blog--hlpv 'org2blog/wp-use-tags-as-categories)
   "org2blog/wp-use-tags-as-categories")
  ("bb" (org2blog--hlpv 'org2blog/wp-use-wp-latex)
   "org2blog/wp-use-wp-latex")

  ("q" org2blog--hydra-main/body "Back"))

;;; Functions - Public

(defun org2blog-readme ()
  "Display project README.org.

  Load the project's readme file into a buffer,
  start Org mode, and make buffer readonly.

  This is the real project readme displayed
  on the project host site (GitHub at the moment)."
  (interactive)
  (catch 'return
    (condition-case-unless-debug err
        (let* ((match (find-function-noselect 'org2blog-readme t))
               (_ (unless (and (consp match) (cdr match))
                    (org2blog--error (concat "I’m sorry but I can’t show you the "
                                             "README using ‘org2blog-readme’."))
                    (throw 'return nil)))
               (srcbuf (car match))
               (srcfile (with-current-buffer srcbuf
                          (buffer-file-name)))
               (_ (unless (file-exists-p srcfile)
                    (message
                     (concat "I’m sorry I ran into a problem trying "
                             "to find the readme file"
                             "for ‘org2blog-readme’. Please "
                             "report this as an error."))
                    (org2blog--error "Couldn’t open README")
                    (throw 'return nil)))
               (title "*Org2Blog README (COPY)*")
               (destbuf (get-buffer-create title))
               (readme (concat (file-name-directory srcfile) "README.org")))
          (switch-to-buffer destbuf)
          (condition-case-unless-debug err
              (insert-file-contents readme)
            (error
             (org2blog--error
              (format (concat "I’m sorry I ran into a problem trying load "
                              "and the contents %s in ‘org2blog-readme’.")
                      readme)
              (format "%s" err))
             (throw 'return nil)))
          (goto-char (point-min))
          (org-mode)
          (message
           (format
            (concat
             "I just copied README.org into this buffer. You can "
             "leave it open as a reference, take notes in it, "
             "copy and paste things into your own configuration, "
             "and even save it to your own file. "
             "I didn't include its screenshots but you "
             "can find them here %s. "
             "When you are finished you can save "
             "this buffer's contents into "
             "your own file or just kill the buffer to quit.")
            (org2blog-def-homepage org2blog-defi))))
      (error
       (org2blog--error
        (format (concat "I’m sorry I ran into a problem trying to display "
                        "the readme somewhere in ‘org2blog-readme’."))
        (format "%s" err))
       (throw 'return nil)))))

;;;###autoload
(defun org2blog-user-interface ()
  "Invoke the graphical user interface."
  (interactive)
  (org2blog--hydra-main/body))

;;;###autoload
(defun org2blog-on-new-entry-kill (kind)
  "Handler for a new KIND of entry buffer closing.

  KIND must be either ’buffer or ’subtree.

  Use like this:

  (add-hook 'kill-buffer-hook
            (apply-partially #'org2blog-on-new-entry-kill ’buffer)
            nil 'local)
  ."
  (catch 'return
    (let* ((save-buffer? (and (org2blog--blog-property-or
                               :safe-new-entry-buf-kill
                               org2blog/wp-safe-new-entry-buffer-kill)
                              (not (buffer-file-name))
                              (y-or-n-p
                               (concat "This entry hasn’t been saved to a file yet. "
                                       "Should I save it to a file?"))))
           (published? (when save-buffer?
                         (y-or-n-p
                          (concat "I’m about to try to save the details "
                                  "about this entry and I need to know: "
                                  "Has it already "
                                  "been published?")))))
      (when save-buffer?
        (save-buffer)
        (org2blog-entry-track kind published?)))))

;;;###autoload
(defun org2blog-maybe-start ()
  "Enable function `org2blog/wp-mode' when `#+ORG2BLOG:' is present.

  Use it like this:

  (add-hook 'org-mode-hook #'org2blog-maybe-start)"
  (with-current-buffer (current-buffer)
    (when (org2blog--bprop "ORG2BLOG")
      (org2blog/wp-mode t))))

;;;###autoload
(defun org2blog-user-report (on)
  "Report library actions if ON is non-nil.

  Call with a prefix-argument to enable, and without one
  to disable debugging.

  org2blog/wp operates using the following APIs in the order
  listed below, followed by details about their debug output:

  - org2blog: Application Layer
  - ox-wp: WordPress API
  - Display export in a text buffer: *Org WordPress Export*
  - xml-rpc: Message processing layer
  - The XML message content sent to the server over HTTPS.
  Useful for testing with cURL and comparing the results
  to xml-rpc.
  - View call request data in buffer: request-data
  - The internal data structure used to make the
  post call. Useful for a quick view of the call details
  as an Elisp list.
  - View xml-rpc method call data in buffer: func-call
  - url-util: Message transfer layer
  - Debug messages output in buffer: *URL-DEBUG*
  - gnutls: Secure communications layer
  - Debug messages output in buffer: *Messages*

  Investigate by going through layer's messages from top to bottom.
  Call function ‘org2blog-version-info’ to display runtime version numbers

  You usually only need to keep track of what is happening between
  two of them because if it is doing what you expect then you
  can move on.

  Consider print messages where you need them and also using Edebug.
  With virtually no setup Edebug lets you walk through a function
  and evaluate local variables to see precisely what is happening.

  If after studying the request body, messages, and control flow
  things still don't work then the best thing to do is to test the
  call using another tool. Paste the request-data into a file named
  `test.txt' and make the request using cURL like this:

  curl --data @test.txt https://www.yourblog.com/xmlrpc.php

  By this point you'll have a better sense of where things are
  happening, or not, and now might be the time to move on to the
  transfer layer.

  If you are investigating at the GnuTLS layer it helps to study
  the debug messages side by side with the output of an analysis
  tool like tcpdump or Wireshark. Viewing them side-by-side helps
  to make better sense of the flow and interactions between what
  you expected, the APIs tried to do, and what really happened
  over the wire. If the time comes to dig deeper into the
  communications layer then start by reading more in the variable
  ‘gnutls-algorithm-priority’ and it's referenced GnuTLS
  documentation. GnuTLS doesn’t expose a version number as a
  variable, but you will see it in the detailed logging
  messages.

  This is beyond the domains of Emacs and into GnuTLS. However,
  it will let you do things like selectively enable and disable
  protocols to help narrow down what works and what doesn't, helping
  you further investigate the issue. The contents of the debug
  buffer include things like certificate version and issuer, public
  key algorithm, and protocol. The protocol information is particularly
  important because when clients connect to a server the protocol
  is often negotiated and it might not be what you expect. For
  example this is why your XML request might work using cURL
  but not using gnutls: the negotiated protocol version might not quite work
  right between your client and the server! A solution here then is to
  force a different method by customizing ‘gnutls-algorithm-priority’.
  If you get this far, then give yourself a pat on the back for digging
  deeper. It is actually pretty fun to look behind the curtain and what
  is happening on the socket layer. Of course that is only looking
  back at it—at the time it is pretty unpleasant!

  Tracking down the unexpected behavior requires no magic–just
  patience and persistence and definitely talking it through
  with others. Before getting overwhelmed, take a break and
  consider reaching out using email or an Issue Request.

  Remember: Org2Blog is trying to keep the fun in blogging. So
  enjoy working through your debugging session, it is one step
  closer to doing more blogging!"
  (interactive "P")
  (setq org-export-show-temporary-export-buffer (if on t nil))
  (setq xml-rpc-debug (if on 3 0))
  (setq url-debug (if on t nil))
  (setq gnutls-log-level (if on 2 0))
  (message "%s detailed reporting about *everything* that I am doing. %s"
           (if on "Enabling" "Disabling")
           (if on "Hold onto your seat!" "Enjoy the silence.")))

;;;###autoload
(defun org2blog-user-report-on ()
  "Enable ‘org2blog-user-report’ ’ing."
  (interactive)
  (org2blog-user-report t))

;;;###autoload
(defun org2blog-user-report-off ()
  "Disable ‘org2blog-user-report’ ’ing."
  (interactive)
  (org2blog-user-report nil))

;;;###autoload
(defun org2blog-version-info (&optional value)
  "Display critical library information or return as a VALUE if non-nil.

  Hydra doesn't provide a version number."
  (interactive)
  (let ((msg (format
              "Org2Blog Runtime: Org2Blog %s, Emacs %s, Org Mode %s,
  MetaWeblog %s, XML-RPC %s, HTMLize %s"
              org2blog/wp-version
              emacs-version
              org-version
              metaweblog-version
              xml-rpc-version
              htmlize-version)))
    (if value msg
      (message msg))))

;;;###autoload
(defun org2blog-user-set-password ()
  "Set password “in memory”.

  This does not change your password on the blog.

  This does not change your password in your configuration file.

  It does change your password in memory during this session.

  See messages below for details."
  (interactive)
  (catch 'return
    (let ((new (read-passwd
                (concat "What would you like to be "
                        "the password for your blog "
                        "user that I store right now "
                        "“in memory” in this session? "))))
      (when (zerop (length new))
        (message (concat
                  "It looks like you set your password to nothing. "
                  "I’m not sure what you mean by that so I’m going to "
                  "leave your password alone."))
        (throw 'return nil))
      (setq org2blog-password new)
      (message (concat
                "I just set your password “on your computer in memory. "
                "I mean that, your password remains the same on "
                "your blog and I’ll use the new password you just told me "
                "from now on to do things there.  "
                "If you already changed your password on your blog, or will "
                "do it soon, then you would probably use this function.")))))

;;;###autoload
(defun org2blog-user-login (&optional blog-name)
  "Log in to BLOG-NAME if non-nil, otherwise choose from a list.

  Please note that this login is only from the User's perspective.
  Org2Blog uses the XML-RPC API to interact with WordPress.
  Org2Blog has to send the Users password with every API call.
  The API is stateless: there is no concept of logging in. Every
  API call is a new one requiring the password each time. Despite
  that Org2Blog has to provide some concept of being logged in
  for the User. Given that goal Org2Blog must know some basics
  about the User's blog. Using that information it must make some
  decisions about how to configure itself for the most common
  usage scenario.

  The most common usage scenario here is defined by imagined usage
  and lack of Issue Requests stating otherwise. It looks like this:

  - You must tell me about at least one blog available for use
  now. Unless you defined a blog in `org2blog/wp-blog-alist' I'm
  stopping.
  - You must choose a blog to use for this session. Unless you
  choose one I'm stopping.
  - Thus far Org2Blog hasn't made any API calls. Therefore we
  still don't know if User's password works or not. This is OK
  because the User can still use Org2Blog without logging
  successfully. The only limitation is that the User won't have
  completion data.
  - `org2blog-complete' needs completion data to work. Therefore
  categories, tags, and pages are loaded here. If any of the
  loads fail then I'll notify the user. It is an error because
  we don't know why it didn't work. It could be network
  connectivity or a problem with Org2Blog itself. However the
  User can still continue moving forward to edit an Org2Blog file
  without that data. Consequently the function proceeds instead
  of failing here.
  "
  (interactive)
  (catch 'return
    (when (not org2blog/wp-blog-alist)
      (message "%s" (concat "I’m sorry, I can’t find any blogs for you to "
                            "login to. Please add your blog to "
                            "‘org2blog/wp-blog-alist’ and try "
                            "logging in again."))
      (throw 'return nil))
    (setq org2blog-blog-key
          (or
           blog-name
           (and (equal (length org2blog/wp-blog-alist) 1)
                (car (car org2blog/wp-blog-alist)))
           (completing-read
            "What blog would you like to log in to? ([Tab] to see list): "
            (mapcar 'car org2blog/wp-blog-alist) nil t)))
    (unless (> (length org2blog-blog-key) 1)
      (message
       (concat "I’m sorry, I can’t log in to blogs with names less than 2 "
               "characters long! It is weird, but I just can’t! Please "
               "run me again and tell  me about a blog with a name at "
               "least 2 characters long. There are 3 ways to do it: "
               "tell me inside \"this call\", configure "
               "‘org2blog/wp-blog-alist’, or choose a different blog from "
               "the list you are presented."))
      (throw 'return nil))
    (setq org2blog-blog (assoc org2blog-blog-key org2blog/wp-blog-alist)
          org2blog-xmlrpc (org2blog-blog-get :url)
          org2blog-username (org2blog-blog-get :username)
          org2blog-blogid (or (org2blog-blog-get :id) org2blog--default-blogid)
          org2blog-password
          (or
           (org2blog-blog-get :password)
           (read-passwd
            (format (concat
                     "It looks like you still haven’t entered a password, "
                     "and I need that to log you in. "
                     "What is your password for ‘%s’ on ‘%s’? "
                     "(type C-g to quit)")
                    org2blog-username org2blog-blog-key))))
    (condition-case val
        (progn
          (message "Loading categories...")
          (sit-for org2blog-step-time)
          (org2blog--load-categories))
      (:success
       (setq org2blog-categories val)
       (message "Categories loaded."))
      (error
       (org2blog--error
        (format (concat "I’m sorry I ran into a problem trying to load categories "
                        "inside of ‘org2blog-user-login’."))
        (format "%s" val))))
    (condition-case val
        (progn
          (message "Loading tags...")
          (sit-for org2blog-step-time)
          (org2blog--load-tags))
      (:success
       (setq org2blog-tags val)
       (message "Tags loaded."))
      (error
       (org2blog--error
        (format (concat "I’m sorry I ran into a problem trying to load tags "
                        "inside of ‘org2blog-user-login’."))
        (format "%s" val))))
    (condition-case val
        (progn
          (message "Loading page list...")
          (sit-for org2blog-step-time)
          (org2blog--load-pages 'summaries))
      (:success
       (setq org2blog-pages val)
       (message "Pages loaded."))
      (error
       (org2blog--error
        (format (concat "I’m sorry I ran into a problem trying to load page "
                        "summaries inside of ‘org2blog-user-login’."))
        (format "%s" val))))
    (setq org2blog-logged-in t)
    (message (concat
              "You are now ready to use your blog “%s”. "
              "Hope you have fun blogging and have a great day!")
             org2blog-blog-key)))

;;;###autoload
(defun org2blog-user-logout()
  "Log out of blog."
  (interactive)
  (setq org2blog-xmlrpc nil
        org2blog-username nil
        org2blog-blogid nil
        org2blog-password nil
        org2blog-categories nil
        org2blog-tags nil
        org2blog-pages nil
        org2blog-logged-in nil)
  (message (concat
            "You are now logged out of your blog “%s”. "
            "Hope you had fun blogging and have a great day!")
           org2blog-blog-key))

(defun org2blog--new (destination)
  "Create new entry buffer for DESTINATION.
  Destination is either a symbol ‘buffer’ or a ‘subtree’."
  (catch 'return
    (unless (or (eq destination 'buffer) (eq destination 'subtree))
      (org2blog--error
       (format
        (concat "I’m sorry I ran into a problem "
                (format "inside of ‘org2blog--new’ with a destination ‘%s’."
                        destination))))
      (throw 'return nil))
    (when (and (not org2blog-logged-in)
               (y-or-n-p
                (concat
                 "It looks like you are not logged in right now. "
                 "Would you like to login before composing "
                 "this new entry?")))
      (org2blog-user-login))
    (let* ((buf-name (cond ((eq destination 'buffer) "Buf")
                           ((eq destination 'subtree) "Sub")))
           (buf (generate-new-buffer
                 (format org2blog-buffer-name buf-name org2blog-blog-key)))
           content)
      (switch-to-buffer buf)
      (add-hook 'kill-buffer-hook
                (apply-partially #'org2blog-on-new-entry-kill destination) nil 'local)
      (org-mode)
      (cond ((eq destination 'buffer)
             (setq content
                   (concat
                    (or org2blog/wp-buffer-template-prefix "")
                    (funcall org2blog/wp-buffer-format-function
                             org2blog/wp-buffer-template))))
            ((eq destination 'subtree)
             (setq content
                   (concat
                    (or org2blog/wp-buffer-subtree-template-prefix "")
                    (funcall org2blog/wp-buffer-subtree-format-function
                             org2blog/wp-buffer-subtree-template)))))
      (insert content)
      (org2blog/wp-mode t))))

;;;###autoload
(defun org2blog-buffer-new ()
  "Create new post entry."
  (interactive)
  (org2blog--new 'buffer))

;;;###autoload
(defun org2blog-subtree-new ()
  "Create new subtree entry."
  (interactive)
  (org2blog--new 'subtree))

;;;###autoload
(defun org2blog-buffer-post-save (&optional publish)
  "Save new or existing post. Publish if PUBLISH is non-nil."
  (interactive "P")
  (org2blog-entry-save 'buffer 'post publish))

;;;###autoload
(defun org2blog-buffer-post-publish ()
  "Publish post."
  (interactive)
  (org2blog-buffer-post-save t))

;;;###autoload
(defun org2blog-buffer-page-save (&optional publish)
  "Save new page to the blog or edits an existing page. Publish if PUBLISH is non-nil. Do as subtree if SUBTREE-P is non-nil."
  (interactive "P")
  (org2blog-entry-save 'buffer 'page publish))

;;;###autoload
(defun org2blog-buffer-page-publish ()
  "Publish page."
  (interactive)
  (org2blog-buffer-page-save t))

;;;###autoload
(defun org2blog-subtree-post-save (&optional publish)
  "Save the current subtree entry as a draft. Publish if PUBLISH is non-nil."
  (interactive "P")
  (catch 'return
    (org2blog--in-subtree-check)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (org-id-get nil t "o2b")
        (org2blog-entry-save 'subtree 'post publish)
        (widen)
        (when (buffer-file-name)
          (save-buffer))))))

;;;###autoload
(defun org2blog-subtree-post-publish ()
  "Publish subtree post."
  (interactive)
  (org2blog-subtree-post-save t))

;;;###autoload
(defun org2blog-subtree-page-save (&optional publish)
  "Save new subtree page to the blog or edits an existing page. If PUBLISH is non-nil then save and publish it."
  (interactive "P")
  (catch 'return
    (org2blog--in-subtree-check)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (org-id-get nil t "o2b")
        (org2blog-entry-save 'subtree 'page publish)
        (widen)
        (when (buffer-file-name)
          (save-buffer))))))

;;;###autoload
(defun org2blog-subtree-page-publish ()
  "Publish page."
  (interactive)
  (org2blog-subtree-page-save t))

;;;###autoload
(defun org2blog-entry-save (source type &optional publish)
  "Save new or existing entry of TYPE from SOURCE. In non-nil PUBLISH, do. If non-nil SUBTREE-P, do."
  (interactive "P")
  (org2blog/wp-mode t)
  (org2blog--ensure-login)
  (save-excursion
    (save-restriction
      (catch 'return
        (let* ((from-buffer (eq source 'buffer))
               (from-subtree (eq source 'subtree))
               (to-post (eq type 'post))
               (to-page (eq type 'page))
               (thing (symbol-name type))
               (made-new-entry nil)
               (post (org2blog--export-as-post from-subtree))
               (confirm (and
                         (org2blog--blog-property-or :confirm org2blog/wp-confirm-post)
                         publish))
               (show (or (org2blog-blog-has :show)
                         org2blog/wp-show-post-in-browser))
               post-id)
          (org2blog--create-categories (cdr (assoc "categories" post)))
          (setq post-id (cdr (assoc "post-id" post)))
          (when confirm
            (when (not (y-or-n-p
                        (concat
                         (format "Would you like to publish your %s: “%s” (ID “%s”)"
                                 thing (cdr (assoc "title" post)) post-id)
                         "?")))
              (message
               (concat "Canceled publishing your %s: “%s” (ID “%s”).")
               thing (cdr (assoc "title" post))
               post-id)
              (throw 'return (list 'failure post-id "User canceled"))))
          (condition-case-unless-debug err
              (cond ((and to-post post-id)
                     (metaweblog-edit-post org2blog-xmlrpc
                                           org2blog-username
                                           org2blog-password
                                           post-id
                                           post
                                           publish))
                    ((and to-post (not post-id))
                     (setq post-id (metaweblog-new-post
                                    org2blog-xmlrpc
                                    org2blog-username
                                    org2blog-password
                                    org2blog-blogid
                                    post
                                    publish))
                     (setq made-new-entry t))
                    ((and to-page post-id)
                     (metaweblog-wp-edit-page org2blog-xmlrpc
                                              org2blog-username
                                              org2blog-password
                                              org2blog-blogid
                                              post-id
                                              post
                                              publish))
                    ((and to-page (not post-id))
                     (setq post-id
                           (metaweblog-wp-new-page org2blog-xmlrpc
                                                   org2blog-username
                                                   org2blog-password
                                                   org2blog-blogid
                                                   post
                                                   publish))
                     (setq made-new-entry t)))
            (error
             (org2blog--error
              (format (concat "I’m sorry I ran into a problem "
                              "on %s: “%s” (ID “%s”) "
                              "inside of ‘org2blog-entry-save’.")
                      thing (cdr (assoc "title" post)) post-id)
              (format "%s" err))
             (throw 'return (list 'failure post-id "RPC error"))))
          (when made-new-entry
            (run-hook-with-args
             'org2blog-buffer-entry-save-hook
             (org2blog--get-post-or-page post-id))
            (when to-page
              (condition-case-unless-debug err
                  (setq org2blog-pages (org2blog--load-pages 'summaries))
                (error
                 (org2blog--error
                  (format (concat "I just saved your new page, "
                                  "but it won’t show up yet "
                                  "when you try to complete it. "
                                  "After you log out and back in "
                                  "though it will show up."))
                  (format "%s" err)))))
            (when from-buffer
              (goto-char (point-min))
              (when to-post (insert (concat "#+BLOG: " org2blog-blog-key "\n")))
              (insert (concat "#+POSTID: " post-id "\n")))
            (when from-subtree
              (when to-post (org-entry-put (point) "BLOG" org2blog-blog-key))
              (org-entry-put (point) "POSTID" post-id)))
          (org2blog--save-details post post-id publish from-subtree)
          (let* ((did (format
                       (if publish
                           "Published your %s: “%s”. Its ID is “%s”. "
                         "Saved your %s as a draft: “%s”. Its ID is “%s”. ")
                       thing (cdr (assoc "title" post)) post-id))
                 (showit (or (and (atom show) (symbolp show) (not (listp show)) show) (cadr show)))
                 (dont (equal showit 'dont))
                 (show (equal showit 'show))
                 (ask (equal showit 'ask)))
            (cond (dont (message
                         (concat did
                                 "It looks like you decided not to automatically display "
                                 "your %s, so I won’t. If you ever want to change "
                                 "it then try customizing "
                                 "‘org2blog/wp-show-post-in-browser’.")
                         thing))
                  ((not org2blog-logged-in)
                   (message
                    (concat did
                            "It looks like you wanted to display your %s, but "
                            "I couldn’t because you are not logged in to your "
                            "blog. Please log in to your blog and try doing "
                            "this again.")
                    thing))
                  (show (message "%s" did)
                        (org2blog-entry-view source type))
                  ((and ask (y-or-n-p
                             (format
                              (concat did
                                      "Would you like to display "
                                      "your %s: “%s” (ID “%s”)? ")
                              thing (cdr (assoc "title" post)) post-id)))
                   (org2blog-entry-view source type))))
          (throw 'return (list 'success post-id "It worked")))))))

;;;###autoload
(defun org2blog-entry-trash-prompt (id)
  "Prompt for an entry ID then trash it."
  (interactive "nPlease type the entry ID you want to trash (type C-g to quit): ")
  (catch 'return
    (when (null id)
      (message (concat "It looks like you didn’t type an entry ID "
                       "so I won’t try to trash anything right now."))
      (throw 'return nil))
    (when (y-or-n-p
           (format
            (concat "Is “%s” a ***POST*** that you want to trash? "
                    "(type C-g to quit)") id))
      (org2blog-entry-trash 'post id)
      (throw 'return nil))
    (when (y-or-n-p
           (format
            (concat "Is “%s” a ***PAGE*** that you want to trash? "
                    "(type C-g to quit)") id))
      (org2blog-entry-trash 'page id)
      (throw 'return nil))
    (message (concat
              "It looks like you don’t want to trash "
              "either a post or page "
              "so I won’t try to trash anything right now."))))

;;;###autoload
(defun org2blog-buffer-post-trash (&optional post-id)
  "Trash buffer post. If POST-ID is non-nil trash that."
  (interactive "P")
  (org2blog-entry-trash 'post post-id))

;;;###autoload
(defun org2blog-subtree-post-trash (&optional post-id)
  "Trash subtree post. If POST-ID is non-nil trash that."
  (interactive "P")
  (catch 'return
    (org2blog--in-subtree-check)
    (org2blog-entry-trash 'post post-id)))

;;;###autoload
(defun org2blog-buffer-page-trash (&optional page-id)
  "Trash page. If PAGE-ID is non-nil trash that."
  (interactive "P")
  (org2blog-entry-trash 'page page-id))

;;;###autoload
(defun org2blog-subtree-page-trash (&optional page-id)
  "Trash page. If PAGE-ID is non-nil trash that."
  (interactive "P")
  (catch 'return
    (org2blog--in-subtree-check)
    (org2blog-entry-trash 'page page-id)))

;;;###autoload
(defun org2blog-entry-trash (type &optional entry-id)
  "Trash entry of TYPE. If ENTRY-ID is non-nil trash that one."
  (interactive "P")
  (org2blog--ensure-login)
  (when (null entry-id)
    (setq entry-id (or (org2blog--bprop "POSTID")
                       (org2blog--bprop "POST_ID")
                       (progn (org-narrow-to-subtree)
                              (widen)
                              (or (org2blog--eprop "POSTID")
                                  (org2blog--eprop "POST_ID"))))))
  (catch 'return
    (let* ((safetrash (org2blog--blog-property-or :safe-trash org2blog/wp-safe-trash))
           (is-post (eq type 'post))
           (is-page (eq type 'page))
           (thing (symbol-name type))
           (doit (or (not safetrash)
                     (y-or-n-p
                      (format (concat "Would you like to trash "
                                      "your %s with ID: “%s”?")
                              thing entry-id)))))
      (when (not doit)
        (message (concat "You chose not to trash your post with ID: “%s”"
                         ", so I did not.") entry-id)
        (throw 'return nil))
      (condition-case-unless-debug err
          (progn
            (when is-post
              (metaweblog-delete-post org2blog-xmlrpc
                                      org2blog-username
                                      org2blog-password
                                      entry-id))
            (when is-page
              (metaweblog-wp-delete-page org2blog-xmlrpc
                                         org2blog-blogid
                                         org2blog-username
                                         org2blog-password
                                         entry-id)))
        (error
         (org2blog--error
          (format (concat "I’m sorry I ran into a problem "
                          "trying to trash your %s "
                          "ID “%s” "
                          "inside of ‘org2blog-entry-trash’.")
                  type entry-id)
          (format "%s" err))
         (throw 'return nil)))
      (message "I just trashed your %s with ID: “%s”." thing entry-id))))

;;;###autoload
(defun org2blog-complete ()
  "Complete categories, tags, or pages."
  (interactive)
  (catch 'return
    (let ((case-fold-search t)
          see-cat see-tag see-parent ls thing propend)
      (let ((pos (point)))
        (forward-line 0)
        (setq see-cat (or (re-search-forward "^#\\+category: "
                                             (line-end-position) t 1)
                          (re-search-forward "^:category: "
                                             (line-end-position) t 1)))
        (setq see-tag (or (re-search-forward "^#\\+tags: "
                                             (line-end-position) t 1)
                          (re-search-forward "^:post_tags: "
                                             (line-end-position) t 1)))
        (setq see-parent (or (re-search-forward "^#\\+parent: "
                                                (line-end-position) t 1)
                             (re-search-forward "^:parent: "
                                                (line-end-position) t 1)))
        (setq propend (or see-cat see-tag see-parent))
        (goto-char pos))
      (unless propend
        (message (concat "I’m sorry, "
                         "I only know how to complete "
                         "categories, tags, or pages. "
                         "Please position the cursor on "
                         "a category, tag, or page "
                         "property line and "
                         "try completing again. "
                         "If it already is but it isn’t "
                         "working then please make sure that "
                         "there is at least one space "
                         "between the closing colon and "
                         "the cursor: otherwise I "
                         "can’t be sure of what do "
                         "next."))
        (throw 'return nil))
      (cond (see-cat
             (setq ls org2blog-categories)
             (setq thing "Category"))
            (see-tag
             (setq ls org2blog-tags)
             (setq thing "Tag"))
            (see-parent
             (setq ls org2blog-pages)
             (setq thing "Parent"))
            (t (org2blog--error
                (format
                 (concat "I’m sorry but I ran into a "
                         "a problem trying to complete this "
                         "so I stopped. If you can, please "
                         "report that I didn’t "
                         "know how to complete a "
                         "“%s”.")
                 thing))
               (throw 'return nil)))
      (when (= (length ls) 0)
        (message (concat "I’m sorry but there don’t seem to be "
                         "any completions for a %s so I can’t "
                         "complete anything. If haven’t logged "
                         "in yet, then please log in and try again. "
                         "Otherwise create a %s and try again.")
                 thing
                 thing)
        (throw 'return nil))
      (when (< (point) propend) (end-of-line))
      (let ((prompt (format
                     (concat "Please select the %s and hit "
                             "enter to insert it "
                             "(or hit C-g to quit): ")
                     thing))
            (word-match (or (current-word t) ""))
            (completion-match nil))
        (when word-match
          (setq completion-match (completing-read prompt ls nil nil word-match))
          (when (stringp completion-match)
            (search-backward word-match nil t)
            (replace-match (concat completion-match ", ") nil t)))))))

;;;###autoload
(defun org2blog-insert-more ()
  "Insert WordPress “More” tag.

  “More” tags only work in posts, not Pages."
  (interactive)
  (let* ((pre "@@wp:<!--more")
         (post "-->@@")
         (msg (string-trim (read-string "“More” message? (hit return to leave blank): ")))
         (str (or (and (string-blank-p msg) (concat pre post))
                  (concat pre " " msg post))))
    (insert str)))

;;;###autoload
(defun org2blog-structure-template-add ()
  "Enable `BEGIN_EXPORT wp' blocks.

  Add them to `snippet-key org-structure-template-alist' unless
  already present."
  (interactive)
  (let* ((key "w")
         (block "WP")
         (template (cons key block)))
    (add-to-list 'org-structure-template-alist template)))

;;;###autoload
(defun org2blog-insert-mathjax ()
  "Insert the WordPress ‘MathJax’ shortcode."
  (interactive)
  (insert "[mathjax]"))

;;;###autoload
(defun org2blog-insert-latex ()
  "Insert WordPress ‘LaTeX’ string."
  (interactive)
  (insert "$\\LaTeX$"))

;;;###autoload
(defun org2blog-buffer-track ()
  "Track buffer."
  (interactive)
  (org2blog-entry-track 'buffer))

;;;###autoload
(defun org2blog-subtree-track ()
  "Track subtree."
  (interactive)
  (catch 'return
    (org2blog--in-subtree-check)
    (org2blog-entry-track 'subtree)))

;;;###autoload
(defun org2blog-entry-track (source &optional published?)
  "Track entry from SOURCE. Was it already PUBLISHED?"
  (interactive)
  (let ((is-buffer (eq source 'buffer))
        (is-subtree (eq source 'subtree)))
    (save-excursion
      (save-restriction
        (when is-buffer
          (widen)
          (org2blog--save-details (org2blog--export-as-post) "" published? nil))
        (when is-subtree
          (org-narrow-to-subtree)
          (org2blog--save-details (org2blog--export-as-post t) "" published? t)
          (widen))))))

;;;###autoload
(defun org2blog-buffer-post-or-page-view ()
  "Use either `org2blog-buffer-post-view' or `org2blog-buffer-page-view'.

  WordPress 6 differentiates between viewing a Page and a Post.
  Therefore this function must be retired. It is not a bug:
  WordPress just doesn't work that way with the API now.
  "
  (interactive)
  (error
   (concat
    "I’m sorry but due to factors outside my control "
    "this function has been removed. Please use either "
    "`org2blog-buffer-post-view' or "
    "`org2blog-buffer-page-view' instead.")))

;;;###autoload
(defun org2blog-buffer-post-view ()
  "View buffer post."
  (interactive)
  (org2blog-entry-view 'buffer 'post))

;;;###autoload
(defun org2blog-buffer-page-view ()
  "View buffer page."
  (interactive)
  (org2blog-entry-view 'buffer 'page))

;;;###autoload
(defun org2blog-subtree-post-or-page-view ()
  "Use either `org2blog-subtree-post-view' or `org2blog-subtree-page-view'.

  WordPress 6 differentiates between viewing a Page and a Post.
  Therefore this function must be retired. It is not a bug:
  WordPress just doesn't work that way with the API now.
  "
  (interactive)
  (error
   (concat
    "I’m sorry but due to factors outside my control "
    "this function has been removed. Please use either "
    "`org2blog-subtree-post-view' or "
    "`org2blog-subtree-page-view' instead.")))

;;;###autoload
(defun org2blog-subtree-post-view ()
  "View subtree post."
  (interactive)
  (org2blog-subtree-view 'post))

;;;###autoload
(defun org2blog-subtree-page-view ()
  "View subtree page."
  (interactive)
  (org2blog-subtree-view 'page))

;;;###autoload
(defun org2blog-subtree-view (dest)
  "View subtree post or page.

  DEST is either ’post or ’page."
  (interactive)
  (catch 'return
    (org2blog--in-subtree-check)
    (org2blog-entry-view 'subtree dest)))

;;;###autoload
(defun org2blog-entry-view (source dest)
  "View SOURCE's entry published to DEST.

  SOURCE is either ’buffer or ’subtree.

  DEST is either ’post or ’page.
  "
  (interactive)
  (let ((is-subtree (eq source 'subtree))
        (thing (symbol-name source)))
    (when is-subtree (org-narrow-to-subtree))
    (org2blog--ensure-login)
    (when is-subtree (widen))
    (let ((entry-id (or (org2blog--bprop "POSTID")
                        (org2blog--bprop "POST_ID")
                        (org2blog--eprop "POSTID")
                        (org2blog--eprop "POST_ID")))
          (url org2blog-xmlrpc))
      (if (not entry-id)
          (message (concat "I’m sorry, I can’t display this %s post because it "
                           "hasn’t been saved or published yet. Please do "
                           "either and try again.") thing)
        (let* ((base (substring url 0 -10))
               (preview "&preview=true")
               (resource (cond ((eq dest 'post)
                                (format "?p=%s" entry-id))
                               ((eq dest 'page)
                                (format "?page_id=%s" entry-id))
                               (org2blog--error
                                (format
                                 (concat
                                  "Not sure how to view source "
                                  "type “%s” and dest type “%s”.")
                                 source dest))))
               (url (concat base resource preview)))
          (browse-url url))))))

;;;###autoload
(defun org2blog-insert-link-to-post ()
  "Insert link to post."
  (interactive)
  (org2blog-insert-link nil))

;;;###autoload
(defun org2blog-insert-link-to-page ()
  "Insert link to page."
  (interactive)
  (org2blog-insert-link t))

;;;###autoload
(defun org2blog-insert-link (is-page)
  "Choose and insert link to entry using IS-PAGE if non-nil.

  When IS-PAGE is nil then chose from page IDs
  instead of posts."
  (interactive "P")
  (org2blog--ensure-login)
  (message "Loading %s…" (if is-page "page list"
                           (format "last %s posts"
                                   org2blog/link-selection-size)))
  (sit-for org2blog-step-time)
  (catch 'return
    (let* ((post-list
            (condition-case-unless-debug err
                (if is-page
                    (org2blog--load-pages)
                  (metaweblog-get-recent-posts org2blog-xmlrpc
                                               org2blog-blogid
                                               org2blog-username
                                               org2blog-password
                                               org2blog/link-selection-size))
              (error
               (org2blog--error
                (format (concat "I’m sorry I ran into a problem "
                                "trying to insert a link "
                                "inside of ‘org2blog-insert-link’."))
                (format "%s" err))
               (throw 'return nil))))
           post-title entryid url title-id-map)
      (dolist (post post-list)
        (setq title-id-map (cons
                            (if is-page
                                (cons (cdr (assoc "page_title" post)) (cdr (assoc "page_id" post)))
                              (cons (cdr (assoc "title" post)) (cdr (assoc "postid" post))))
                            title-id-map)))
      (setq post-title (completing-read
                        (if is-page "Please select a page: " "Please select a post: ")
                        title-id-map nil t)
            entryid (cdr (assoc post-title title-id-map)))
      (when post-title
        (setq url (concat
                   (replace-regexp-in-string "xmlrpc\\.php$" "?p=" org2blog-xmlrpc)
                   entryid))
        (insert (format "[[%s][%s]]" url post-title))))))

;;;###autoload
(defun org2blog-reload-entry-mode-map ()
  "Re-initialize `org2blog-mode-map'.

  Use the prefix key sequence defined by
  `org2blog/wp-keymap-prefix' and update `minor-mode-map-alist'
  accordingly."
  (interactive)
  (org2blog--init-entry-mode-map)
  (let ((keymap (assoc 'org2blog/wp-mode minor-mode-map-alist)))
    (setcdr keymap org2blog-mode-map)))

;;;###autoload
(defun org2blog-about ()
  "Display brief about page."
  (interactive)
  (switch-to-buffer "*Org2Blog About*")
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "╔═══════════════════════════════════════════════╗\n")
  (widget-insert "║ Emacs → Org mode → Org2Blog → WordPress → WWW ║\n")
  (widget-insert "╚═══════════════════════════════════════════════╝")
  (widget-insert "\n\n")
  (widget-insert "Welcome Org2Bloggers!")
  (widget-insert "\n\n")
  (widget-insert "Org2Blog publishes directly from Emacs’s 🐃 ")
  (widget-insert "Org mode 🦄 to WordPress ")
  (widget-insert "\nblogs 😄.")
  (widget-insert "\n\n")
  (widget-insert "The README explains how to do EVERYTHING. To access it locally go ")
  (widget-insert "\nback to the main menu and choose “README”. ")
  (widget-insert "\n\n")
  (widget-insert "Or read 🕮 about how to use ⛭ it (and more) here: ")
  (widget-create
   'url-link
   :tag "Org2Blog on GitHub"
   "https://github.com/org2blog/org2blog")
  (widget-insert ".")
  (widget-insert "\n\n")
  (widget-insert "Please submit ideas for improvements, requests for help, ")
  (widget-insert "you name it…")
  (widget-insert "\nwhenever you wish please ")
  (widget-create
   'url-link
   :tag "submit a request"
   "https://github.com/org2blog/org2blog/issues")
  (widget-insert ". ")
  (widget-create
   'url-link
   :tag "Maybe even contribute"
   "https://github.com/org2blog/org2blog/blob/master/docs/CONTRIBUTING.org")
  (widget-insert "\nto the project too! 😉")
  (widget-insert "\n\n")
  (widget-insert "Learn more about WordPress 🌐 here: ")
  (widget-create
   'url-link
   :tag "Wikipedia: WordPress"
   "https://en.wikipedia.org/wiki/WordPress")
  (widget-insert ".")
  (widget-insert "\n\n")
  (widget-insert "Learn more about ")
  (widget-create
   'url-link
   :tag "Emacs"
   "https://www.gnu.org/software/emacs/")
  (widget-insert " and ")
  (widget-create
   'url-link
   :tag "Org mode"
   "https://orgmode.org/")
  (widget-insert ".")
  (widget-insert "\n\n")
  (widget-insert "If you’ve never heard the terms “Free Software” or “Libre Software”")
  (widget-insert "\nbefore, or maybe just want to review them, then be sure to
  read all about them here: ")
  (widget-create
   'url-link
   :tag "What is free software?"
   "https://www.gnu.org/philosophy/free-sw.html")
  (widget-insert "\n\n")
  (widget-insert "Org2Blog and all of the libraries that it uses are ")
  (widget-insert "100% Libre Software. \n")
  (widget-insert "Org2Blog is licensed under the ")
  (widget-create
   'url-link
   :tag "GPLv3"
   "https://www.gnu.org/licenses/quick-guide-gplv3.en.html")
  (widget-insert ". (If you want to dig into the details,\n")
  (widget-insert "see the ")
  (widget-create
   'url-link
   :tag "LICENSE.TXT"
   "https://github.com/org2blog/org2blog/blob/master/LICENSE")
  (widget-insert " for this package).")
  (widget-insert "\n\n")
  (widget-insert "If you are brand new to blogging then be sure to read Rebecca Blood’s\n")
  (widget-insert "book:")
  (widget-insert "\n\n")
  (widget-create
   'url-link
   :tag "The Weblog Handbook: Practical Advice On Creating And Maintaining Your Blog"
   "http://www.rebeccablood.net/handbook/excerpts/weblog_ethics.html")
  (widget-insert ".")
  (widget-insert "\n\n")
  (widget-insert "It helps you discover your blogging goals and your values up front leaving you ")
  (widget-insert "\nwith more time for writing ✎.")
  (widget-insert "\n\n")
  (widget-insert "Be sure to check out ")
  (widget-insert "the links to other ")
  (widget-create
   'url-link
   :tag "Org2Bloggers"
   "https://github.com/org2blog/org2blog/blob/master/docs/Org2Bloggers.org")
  (widget-insert " out there.")
  (widget-insert "\n\n")
  (widget-insert "To all and soon to be Org2Bloggers:")
  (widget-insert "\n\n")
  (widget-insert "Hope you have fun blogging and have a great day!")
  (widget-insert "\n\n")
  (widget-create
   'url-link
   :tag "Copyright"
   "https://www.gnu.org/licenses/copyleft.en.html")
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "n" 'widget-forward)
    (define-key map "p" 'widget-backward)
    (define-key map "q" 'kill-this-buffer)
    (use-local-map map))
  (widget-setup)
  (goto-char (point-min)))

;;;###autoload
(defun org2blog-org2blog-keyword-check ()
  "Insert the ORG2BLOG keyword unless it exists.

  Inserts ‘#+ORG2BLOG’ on the first empty lines that it finds.

  If it doesn’t find one then it doesn’t insert it."
  (interactive)
  (catch 'return
    (when (org2blog--bprop "ORG2BLOG")
      (throw 'return nil))
    (save-excursion
      (goto-char (point-min))
      (if (not (re-search-forward "^[:blank:]*$" nil nil nil))
          (message (concat "I’m sorry I couldn’t find any "
                           "empty lines in which to insert "
                           "‘#+ORG2BLOG’. You can either insert "
                           "an empty line anywere in the document "
                           "and run me again or if you want to "
                           "you insert it yourself."))
        (insert "#+ORG2BLOG:\n")))))

;;; Function - Private

(defun org2blog--load-categories ()
  "Load categories from server.
  Caller must handle any errors."
  (let* ((raw (metaweblog-get-categories
               org2blog-xmlrpc
               org2blog-username
               org2blog-password
               org2blog-blogid))
         (cats (mapcar
                (lambda (category) (cdr (assoc "categoryName" category)))
                raw)))
    cats))

(defun org2blog--load-tags ()
  "Load tags from server.
  Caller must handle any errors."
  (let* ((raw (metaweblog-wp-get-tags
               org2blog-xmlrpc
               org2blog-username
               org2blog-password
               org2blog-blogid))
         (tags (mapcar
                (lambda (tag) (cdr (assoc "slug" tag)))
                raw)))
    tags))

(defun org2blog--load-pages (&optional summaries)
  "Load raw pages from server or SUMMARIES if non-nil.
  Caller must handle any errors."
  (let* ((pages (metaweblog-wp-get-pagelist
                 org2blog-xmlrpc
                 org2blog-username
                 org2blog-password
                 org2blog-blogid))
         (page-summaries
          (mapcar (lambda (pg)
                    (cons (cdr (assoc "page_title" pg))
                          (cdr (assoc "page_id" pg))))
                  pages))
         (result (if summaries
                     page-summaries
                   pages)))
    result))

(defun org2blog--in-subtree-check ()
  "Generate error unless cursor is within a subtree."
  (unless (org2blog--in-subtree-p)
    (org2blog--error
     (concat
      "I’m sorry but your cursor is not in "
      "subtree so I can’t do that. Please "
      "move your cursor to a subtree and try "
      "that again."))
    (throw 'return nil)))

(defun org2blog--in-subtree-p ()
  "Non-nil if cursor is below a subtree."
  (interactive)
  (not (org-before-first-heading-p)))

(defun org2blog--bprop (name)
  "Return buffer property for NAME.

  Hierarchy of properties:
  - Globally
  - Buffer
  - Entry

  This functions return buffer properties defined in the name
  line syntax “#+name: value”:

  #+FLOWERTYPE: flower

  See: URL ‘https://orgmode.org/manual/Property-syntax.html#Property-syntax’'."
  (let* ((r (org-make-options-regexp (list (upcase name) (downcase name)))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward r nil t 1)
        (match-string-no-properties 2)))))

(defun org2blog--eprop (name)
  "Return entry property for NAME.
  See ‘org2blog--bprop’ docstring for details."
  (org-entry-get (point) name))

(defun org2blog-blog-has (property)
  "Return non-nil if current blog PROPERTY exists."
  (plist-member (cdr org2blog-blog) property))

(defun org2blog-blog-get (property)
  "Return current blog PROPERTY."
  (plist-get (cdr org2blog-blog) property))

(defun org2blog--blog-property-or (property value)
  "Return current blog PROPERTY, else or VALUE."
  (if (org2blog-blog-has property)
      (org2blog-blog-get property)
    value))

(defun org2blog--login-status ()
  "User login status of current blog."
  (let ((msg (if (not org2blog-logged-in) "Logged Out."
               (format "Logged In To ‘%s’ as ‘%s’."
                       org2blog-blog-key org2blog-username))))
    msg))

(defun org2blog--define-key (map suffix function)
  "Helper to populate ‘org2blog-mode-map’ in MAP for FUNCTION with SUFFIX.

  Define a key sequence SUFFIX in MAP for FUNCTION.

  Uses the mode's key map with the prefix
  `org2blog/wp-keymap-prefix', and the given suffix."
  (let ((keyseq (read-kbd-macro (concat org2blog/wp-keymap-prefix " " suffix))))
    (define-key map keyseq function)))

(defun org2blog--init-entry-mode-map ()
  "Initialize `org2blog-mode-map'.

  Uses the prefix key sequence defined by
  `org2blog/wp-keymap-prefix'.

  Both sets the map and returns the map so that it can be used both
  at mode start time and after the user re-configures it."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (org2blog--define-key map "p" 'org2blog-buffer-post-publish)
    (org2blog--define-key map "P" 'org2blog-buffer-page-publish)
    (org2blog--define-key map "d" 'org2blog-buffer-post-save)
    (org2blog--define-key map "D" 'org2blog-buffer-page-save)
    (org2blog--define-key map "t" 'org2blog-complete)
    (org2blog--define-key map "g" 'org2blog-user-interface)
    (setq org2blog-mode-map map)
    map))

(defun org2blog--create-categories (new-categories)
  "Add NEW-CATEGORIES to ‘org2blog-categories'."
  (let ((result
         (mapcar
          (lambda (cat)
            (if (and (not (seq-contains-p org2blog-categories cat))
                     (y-or-n-p
                      (format
                       (concat "Would you like to "
                               "create the a new "
                               "category named: “%s”?")
                       cat)))
                (condition-case-unless-debug err
                    (metaweblog-wp-new-category
                     org2blog-xmlrpc
                     org2blog-username
                     org2blog-password
                     org2blog-blogid
                     cat)
                  (error
                   (org2blog--error
                    (format
                     (concat "I’m sorry I ran into a problem "
                             "trying to create categories "
                             "inside of ‘org2blog--create-categories’."))
                    (format "%s" err))
                   (throw 'return nil))))
            (add-to-list 'org2blog-categories cat))
          new-categories)))
    result))

(defun org2blog--entry-blog-name ()
  "Return the blog name for this entry (buffer or subtree)."
  (let ((blog-name
         (if (org-buffer-narrowed-p)
             (or (org2blog--eprop "BLOG") "")
           (or (org2blog--bprop "blog") ""))))
    (or (and (assoc blog-name org2blog/wp-blog-alist) blog-name) nil)))

(defun org2blog--ensure-login ()
  "Ensure user is logged in to current entry’s blog.

  This function handles two scenarios:

  Scenario #1

  User logs into BLOG-A and starts blogging. At some point
  User returns to an OLD-ENTRY to make changes. OLD-ENTRY belongs
  to BLOG-B. Upon completing the changes User chooses to save the
  entry. At this point User is logged into BLOG-A while attempting
  to post to BLOG-B. This won’t work, there will be an authentication
  failure.

  This function handles this scenario by logging the User out of
  BLOG-A and logging User into BLOG-B.

  Scenario #2:

  User is not logged in and attempts to save a post.

  This function prompts the user to login."
  (let ((blog-name (org2blog--entry-blog-name)))
    (when (and blog-name (not (equal blog-name org2blog-blog-key)))
      (org2blog-user-logout))
    (unless org2blog-logged-in
      (org2blog-user-login blog-name))))

(defun org2blog-entry-buffer-make (buffer-template)
  "Create new Buffer Entry populated using BUFFER-TEMPLATE.

  See ‘org2blog/wp-buffer-template’ for details about how it is used."
  (format buffer-template
          (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))
          (string-join
           (org2blog--blog-property-or :default-categories org2blog/wp-default-categories)
           ", ")
          (string-join
           (org2blog--blog-property-or :default-tags org2blog/wp-default-tags)
           ", ")
          (org2blog--blog-property-or :default-title org2blog/wp-default-title)))

(defun org2blog-entry-subtree-make (subtree-template)
  "Create new Subtree Entry populated using SUBTREE-TEMPLATE.

  See ‘org2blog/wp-buffer-subtree-template’ for details about how it is used."
  (format subtree-template
          (org2blog--blog-property-or :default-title-sub org2blog/wp-default-title-subtree)
          org2blog-blog-key
          (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))
          (string-join
           (org2blog--blog-property-or :default-categories-sub org2blog/wp-default-categories-subtree)
           ", ")
          (mapconcat
           (lambda (cat) cat)
           (org2blog--blog-property-or :default-tags-sub org2blog/wp-default-tags-subtree)
           ", ")))

(defun org2blog--upload-files-replace-urls (text)
  "Upload files and replace their links in TEXT."
  (catch 'return
    (unless (org2blog--blog-property-or :image-upload org2blog/wp-image-upload)
      (message (format "%s" (concat "No work to do: not uploading images.")))
      (throw 'return text))
    (message (format "%s" (concat "Work to do: uploading images.")))
    (let ((file-regexp "<img src=\"\\(.*?\\)\"")
          file-all-urls file-name file-web-url beg file-thumbnail-name upload-ret)
      (save-excursion
        (while (string-match file-regexp text beg)
          (setq file-name
                (if (match-beginning 1)
                    (substring text (match-beginning 1) (match-end 1))
                  (substring text (match-beginning 2) (match-end 2))))
          (setq file-name (save-match-data (if (string-match "^file:" file-name)
                                               (substring file-name 7)
                                             file-name)))
          (setq beg (match-end 0))
          (when (save-match-data (not (or
                                       (string-match org-link-plain-re file-name)
                                       (string-match "^.*#" file-name)
                                       (string-equal (file-name-nondirectory file-name) ""))))
            (goto-char (point-min))
            (if (re-search-forward (concat "^.*# "
                                           (regexp-quote file-name)
                                           " ") nil t 1)
                ;; Read immediately right after filename to get URL + thumbnail.
                ;; Split-string on space for the web-url and thumbnail.
                ;; If there is no third part, thumbnail will be nil.
                (let ((url-thumb-parts (split-string (buffer-substring-no-properties
                                                      (point)
                                                      (or (end-of-line) (point))) " ")))
                  (setq file-web-url (car url-thumb-parts))
                  ;; Get the name. The ‘cdr’ is either a list or nil so use
                  ;; ‘nth’: it will give the name or nil if there's nothing.
                  (setq file-thumbnail-name (nth 1 url-thumb-parts)))

              ;; Return alist with id, file, url, type
              (condition-case-unless-debug err
                  (setq upload-ret (metaweblog-upload-file
                                    org2blog-xmlrpc
                                    org2blog-username
                                    org2blog-password
                                    org2blog-blogid
                                    (metaweblog-get-file-properties file-name)))
                (error
                 (org2blog--error
                  (format (concat "I’m sorry I ran into a problem "
                                  "inside of ‘org2blog--upload-files-replace-urls’."))
                  (format "%s" err))
                 (throw 'return nil)))
              (setq file-web-url
                    (cdr (assoc "url" upload-ret)))

              ;; Link to the thumbnail image?
              (if (not org2blog/wp-image-thumbnails)
                  (setq file-thumbnail-name nil)
                ;; Use the attachment_id to find the thumbnail
                (let* ((attachment-id (cdr (assoc "id" upload-ret)))
                       ;; http://codex.wordpress.org/XML-RPC_WordPress_API/Media
                       ;; Get the name of thumbnail image, in this case medium at
                       ;; 300px.
                       (media-item-info
                        (xml-rpc-method-call org2blog-xmlrpc
                                             "wp.getMediaItem"
                                             org2blog-blogid
                                             org2blog-username
                                             org2blog-password
                                             attachment-id)))

                  ;; media-item-info -> metadata -> sizes -> medium -> file == basename-300x???.jpg
                  ;; Get the basename of the requested size thumb in ‘medium-file-name’.
                  (let ((media-metadata (cdr (assoc "metadata" media-item-info))))
                    (setq file-thumbnail-name
                          (cdr
                           (assoc "file"
                                  (cdr (assoc org2blog/wp-image-thumbnail-size
                                              (cdr (assoc "sizes" media-metadata))))))))))
              (goto-char (point-max))
              (org2blog--new-line-no-indent)
              (insert (concat "# " file-name " " file-web-url
                              (if file-thumbnail-name
                                  (concat  " " file-thumbnail-name)
                                ""))))

            ;; Retrieve ‘file-web-url’ either via the API or from the document.
            ;; Add to the list of replacements.
            ;; ‘(list (cons a b))’ => ‘((a . b))’ which can then be appended to
            ;; ‘file-all-urls’: a list of 3-element lists
            (setq file-all-urls
                  (append file-all-urls
                          (list (list file-name
                                      file-web-url
                                      file-thumbnail-name))))))

        (dolist (file file-all-urls)

          (if (not (and (nth 2 file) org2blog/wp-image-thumbnails))
              ;; The straight-forward “no-image-thumbnail way”:
              ;; replace “<a href="file://THEFILENAME">” or “<img src="file://THEFILENAME">”
              ;; with “<a href="url">” or “<img src="url">”.
              (setq text (replace-regexp-in-string
                          (concat "\\(<a href=\"\\|<img src=\"\\)\\(file://\\)*" (regexp-quote (car file)))
                          (concat "\\1" (nth 1 file)) text))
            ;; If thumbnail available and user said yes, then do
            ;; the “new image-thumbnail way”:
            ;; 1. Replace normal href as always.
            (setq text (replace-regexp-in-string
                        (concat "\\(<a href=\"\\)\\(file://\\)*" (regexp-quote (car file)))
                        (concat "\\1" (nth 1 file)) text))

            ;; 2. Replace “<img>” with “<a href="full"><img src="thumb">”.
            (let*
                ((file-web-url (nth 1 file))
                 (file-thumbnail-name (nth 2 file))
                 ;; Find the position of the last / measured from the end.
                 (idx (string-match-p (regexp-quote "/")
                                      (concat (reverse (string-to-list file-web-url)))))
                 ;; Chop off just the filename, replace with thumbnail name.
                 (thumbnail-url (concat (substring file-web-url 0 (- idx)) file-thumbnail-name)))

              ;; Replace: <img src="file://./img/blabla.png" alt="volume_cutting.png" />.
              ;; After "sample.png" we use non-greedy matching until “/>”.
              (setq text (replace-regexp-in-string
                          (concat "<img src=\"\\(file://\\)?"
                                  (regexp-quote (car file))
                                  "\"\\(.*?\\)/>")
                          (concat "<a href=\"" file-web-url "\">"
                                  "<img src=\"" thumbnail-url "\"\\2/></a>")
                          text))))))
      text)))

(defun org2blog--get-post-or-page (post-or-page-id)
  "Retrieve an entry for POST-OR-PAGE-ID.

  For information about its fields see
  URL`https://codex.wordpress.org/XML-RPC_MetaWeblog_API#metaWeblog.newPost'"
  (interactive)
  (catch 'return
    (condition-case-unless-debug err
        (let ((post-or-page (metaweblog-get-post org2blog-xmlrpc
                                                 org2blog-username
                                                 org2blog-password
                                                 post-or-page-id)))
          post-or-page)
      (error
       (org2blog--error
        (format (concat "I’m sorry I ran into a problem trying to retrieve "
                        "ID “%s” "
                        "inside of ‘org2blog--get-post-or-page’.")
                post-or-page-id)
        (format "%s" err))
       (throw 'return nil)))))

(defun org2blog--save-details (post pid pub subtree-p)
  "Save POST details of PID and PUB.

  If SUBTREE-P is non-nil, record that."
  (catch 'return
    (save-excursion
      (let ((the-file (if (org2blog-blog-has :track-posts)
                          (car (org2blog-blog-get :track-posts))
                        (car org2blog/wp-track-posts))))
        (when (not the-file)
          (message
           (concat "It looks like you are not tracking your posts "
                   "so I did not save its details. "
                   "If you want to start tracking "
                   "them then simply customize ‘org2blog/wp-track-posts’ "
                   "and save or post your entry again. "))
          (throw 'return nil))
        (when the-file
          (let* ((o2b-id (if subtree-p
                             (concat "id:" (org-id-get nil t))
                           (buffer-file-name)))
                 (log-file (cond
                            ((file-name-absolute-p the-file) the-file)
                            (org-directory
                             (expand-file-name the-file org-directory))
                            (t
                             (org2blog--error
                              (format
                               (concat
                                "I’m sorry, I had a problem creating your post "
                                "tracking file. The problem is that the "
                                "filename is ambiguous. The solution is to "
                                "either use an absolute path or to set "
                                "the variable ‘org-directory’, then try "
                                "tracking your entry again.")))
                             (throw 'return nil))))
                 (headline (if (org2blog-blog-has :track-posts)
                               (cadr (plist-get (cdr org2blog-blog) :track-posts))
                             (cadr org2blog/wp-track-posts)))
                 p)
            (make-directory (file-name-directory log-file) t)
            (when o2b-id
              (with-current-buffer (or (find-buffer-visiting log-file)
                                       (find-file-noselect log-file))
                (save-excursion
                  (save-restriction
                    (widen)
                    (goto-char (point-min))
                    (outline-show-all)
                    (setq p (org-find-exact-headline-in-buffer headline))
                    (if p
                        (progn (goto-char p) (org-narrow-to-subtree) (end-of-line))
                      (goto-char (point-max))
                      (org-insert-heading t)
                      (insert headline)
                      (org-narrow-to-subtree))
                    (if (not (search-forward o2b-id nil t 1))
                        (org-insert-subheading t)
                      (org-back-to-heading)
                      (forward-thing 'whitespace)
                      (kill-line)))
                  (org2blog--update-details post o2b-id pid pub))
                (save-buffer)))))))))

(defun org2blog--update-details (post o2b-id pid pub)
  "Store O2B-ID, PID, and PUB in POST."
  (insert (format "[[%s][%s]]"
                  o2b-id
                  (cdr (assoc "title" post))))
  (org-entry-put (point) "POSTID" (or pid ""))
  (org-entry-put (point) "POST_DATE" (cdr (assoc "date" post)))
  (org-entry-put (point) "PUBLISHED" (if pub "Yes" "No")))

(defun org2blog--new-line-no-indent ()
  "Insert a new line without indenting."
  (insert (if use-hard-newlines hard-newline "\n")))

(defun org2blog--collect-export-options ()
  "Return a plist of export options.

  This can be passed on to the export functions to configure the
  various export options."
  (let ((export-options org2blog-export-options))
    (plist-put export-options :wp-keep-new-lines
               (org2blog--blog-property-or :keep-new-lines org2blog/wp-keep-new-lines))
    (plist-put export-options :wp-latex
               (org2blog--blog-property-or :wp-latex org2blog/wp-use-wp-latex))
    (plist-put export-options :wp-shortcode
               (org2blog--blog-property-or :wp-shortcode org2blog/wp-use-sourcecode-shortcode))
    (plist-put export-options :tags-as-categories
               (org2blog--blog-property-or :tags-as-categories org2blog/wp-use-tags-as-categories))
    (plist-put export-options :wp-shortcode-langs-map
               org2blog/wp-shortcode-langs-map)
    export-options))

(defun org2blog--convert-timestamp-to-iso (timestamp)
  "Convert org TIMESTAMP to ISO."
  (let ((result (format-time-string
                 "%Y%m%dT%T%z"
                 (apply 'encode-time (org-parse-time-string timestamp))
                 t)))
    result))

(defun org2blog--export-as-html (subtree-p export-options)
  "Create entry HTML given EXPORT-OPTIONS and SUBTREE-P."
  (let ((result
         (save-excursion
           (org2blog--upload-files-replace-urls
            (org-no-properties (ox-wp-export-as-string nil subtree-p
                                                       export-options))))))
    result))

(defun org2blog--export-as-post (&optional subtree-p)
  "Parse a buffer entry.

  If SUBTREE-P is non nill then parse subtree."

  (let* ((export-options (org2blog--collect-export-options))
         (tags-as-categories (plist-get export-options :tags-as-categories)))
    (save-excursion
      (save-restriction
        (let ((post (if subtree-p
                        (org2blog--parse-subtree-entry)
                      (org2blog--parse-buffer-entry))))
          (when tags-as-categories
            (setcdr (assoc "categories" post) (cdr (assoc "tags" post)))
            (setcdr (assoc "tags" post) nil))

          (setcdr (assoc "date" post)
                  (org2blog--convert-timestamp-to-iso
                   (org2blog--insert-current-time subtree-p
                                                  (cdr (assoc "date" post)))))
          (setcdr (assoc "description" post)
                  (org2blog--export-as-html subtree-p export-options))
          post)))))

(defun org2blog--bprop-parent-id (parent)
  "Return ID of PARENT.

  If parent is the id of the parent page, the user need not be
  logged in.  Otherwise, the user is prompted to login."

  (when (and parent (equal 0 (string-to-number parent)))
    (org2blog--ensure-login))
  (if parent
      (or
       (cdr (assoc
             (car (split-string parent "\\( *, *\\)" t))
             org2blog-pages))
       (number-to-string (string-to-number parent))
       "0")
    "0"))

(defun org2blog--insert-current-time (subtree-p time)
  "Insert TIME into the entry. If non-nil SUBTREE-P do it there."
  (or time
      (let ((current-time
             (format-time-string (org-time-stamp-format t t)
                                 (org-current-time))))
        (save-excursion
          (if subtree-p
              (org-entry-put (point) "POST_DATE" current-time)
            (goto-char (point-min))
            (insert (concat "#+DATE: " current-time "\n"))))
        current-time)))

(defun org2blog--parse-buffer-entry ()
  "Parse an org2blog buffer entry.

  The entry object returned does not contain the exported html.
  This entry needs to be further processed by the
  `org2blog--export-as-post' function, to add the export html
  and munge it a little to make it suitable to use with the
  `metaweblog' functions."
  (let*
      ((_ (org-export-with-buffer-copy (org-export-get-environment)))
       (parsed-entry
        (list
         (cons "point" (point))
         (cons "date" (org2blog--bprop "DATE"))
         (cons "title" (org2blog--bprop "TITLE"))
         (cons "description" nil)
         (cons "tags"
               (split-string (or (org2blog--bprop "TAGS") "")
                             "\\( *, *\\)" t))
         (cons "categories"
               (split-string (or (org2blog--bprop "CATEGORY") "")
                             "\\( *, *\\)" t))
         (cons "post-id" (org2blog--bprop "POSTID"))
         (cons "parent" (org2blog--bprop-parent-id
                         (org2blog--bprop "PARENT")))
         (cons "excerpt" (org2blog--bprop "DESCRIPTION"))
         (cons "permalink" (or (org2blog--bprop "PERMALINK") "")))))
    parsed-entry))

(defun org2blog--parse-subtree-entry ()
  "Parse an org2blog subtree entry.

  The entry object returned does not contain the exported html.
  This entry needs to be further processed by the
  `org2blog--export-as-post' function, to add the export html
  and munge it a little to make it suitable to use with the
  `metaweblog' functions."
  (let*
      ((parsed-entry
        (list
         (cons "point" (point))
         (cons "date" (or (org2blog--eprop "POST_DATE")
                          (org2blog--eprop "SCHEDULED")
                          (org2blog--eprop "DEADLINE")
                          (org2blog--eprop "TIMESTAMP_IA")
                          (org2blog--eprop "TIMESTAMP")))
         (cons "title" (or (org2blog--eprop "TITLE")
                           (nth 4 (org-heading-components))))
         (cons "description" nil)
         (cons "tags" (or
                       (split-string (or (org2blog--eprop "POST_TAGS") "") "\\( *, *\\)" t)
                       (mapcar 'org-no-properties (org-get-tags (point) nil))))
         (cons "categories"
               (split-string (or (org2blog--eprop "CATEGORY") "")
                             "\\( *, *\\)" t))
         (cons "post-id" (or (org2blog--eprop "POSTID")
                             (org2blog--eprop "POST_ID")))
         (cons "parent" (org2blog--bprop-parent-id
                         (org2blog--eprop "PARENT")))
         (cons "excerpt" (org2blog--eprop "DESCRIPTION"))
         (cons "permalink" (org2blog--eprop "PERMALINK")))))
    parsed-entry))

(defun org2blog--startup-library-check (library-name current-version min-version)
  "Warn when LIBRARY-NAME CURRENT-VERSION is less than the MIN-VERSION version."
  (when (version< current-version min-version)
    (display-warning
     'org2blog/wp
     (format
      (concat "I’m sorry, I might have problems running right now. It looks like "
              "version %s of %s is installed, but I need "
              "at least version %s of %s to run. You might not run "
              "into problems, but please install at "
              "least version %s of %s and run me again. "
              "I’m sorry for the trouble, but running a newer version "
              "will make your whole Org2Blog experience "
              "faster, easier, and more fun. See you soon!")
      current-version library-name
      min-version library-name
      min-version library-name)
     :error)))

(defun org2blog--startup-asserts ()
  "Verify startup assertions."
  (org2blog--startup-library-check "Org mode" org-version
                                   org2blog/wp-required-org-version)
  (org2blog--startup-library-check "Emacs" emacs-version org2blog--minimal-emacs))

(defun org2blog--error (amessage &optional report-details)
  "Display error AMESSAGE and non-nil REPORT-DETAILS for a human."
  (display-warning 'org2blog/wp amessage :error)
  (when report-details
    (display-warning
     'org2blog/wp
     (concat
      (org2blog--get-timestamp) "\n"
      (format "Please report that along with the following details:\n%s\n%s"
              report-details
              (org2blog-version-info t)))
     :error))
  (display-buffer "*Warnings*")
  (message
   (concat
    "I’m sorry, I ran into an error and couldn’t do that. "
    "Please view the “*Warnings*” buffer for details.")))

(defun org2blog--get-timestamp ()
  "Create a full ISO 8601 format timestamp."
  (let* ((timestamp-without-timezone (format-time-string "%Y-%m-%dT%T"))
         (timezone-name-in-numeric-form (format-time-string "%z"))
         (timezone-utf-offset
          (concat (substring timezone-name-in-numeric-form 0 3)
                  ":"
                  (substring timezone-name-in-numeric-form 3 5)))
         (timestamp (concat timestamp-without-timezone
                            timezone-utf-offset)))
    timestamp))

;;; Mode

;;;###autoload
(define-minor-mode org2blog/wp-mode
  "Toggle org2blog/wp minor mode.

  With no argument, the mode is toggled on/off.

  Non-nil argument turns mode on.

  Nil argument turns mode off.

  Commands:
  \\{org2blog-mode-map}

  Entry to this mode calls the value of `org2blog-mode-hook'."

  :init-value nil
  :lighter " o2b"
  :group 'org2blog/wp
  :keymap (org2blog--init-entry-mode-map)

  (when org2blog/wp-mode
    (org2blog-structure-template-add)
    (run-mode-hooks 'org2blog-mode-hook)))

;;; Initialization

(cond (after-init-time
       (org2blog--startup-asserts)
       (org2blog-version-info))
      ((not after-init-time)
       (add-hook 'after-init-hook #'org2blog--startup-asserts t)
       (add-hook 'after-init-hook #'org2blog-version-info t)))

(provide 'org2blog)

;;; org2blog.el ends here
