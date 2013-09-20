;;; org2blog.el --- blog from Org mode to wordpress

;; Copyright (C) 2010 Benjamin Beckwith <bnbeckwith@gmail.com>
;; Copyright (C) 2010 Marcel van der Boom <marcel@hsdev.com>
;; Copyright (C) 2010,2011 Puneeth Chaganti <punchagan+org2blog@gmail.com>
;; Copyright (C) 2010 Sacha Chua <sacha@sachachua.com>
;; Copyright (C) 2010 Giovanni Moretti <Giovanni@reflections.co.nz>
;; Copyright (C) 2011 Mykola Nikishov <mn@mn.com.ua>
;; Copyright (C) 2010 Matt Price <matt@roke.mercey.dyndns.org>
;; Copyright (C) 2013 Peter Vasil <mail@petervasil.net>

;; Author: Puneeth Chaganti <punchagan+org2blog@gmail.com>
;; Version: 0.8.1
;; Keywords: orgmode, wordpress, blog

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; A portion of the code in this file is based on blog.el posted to
;; http://www.mail-archive.com/gnu-emacs-sources@gnu.org/msg01576.html
;; copyrighted by Ashish Shukla. The Copyright notice from that file is
;; given below.

;; blog.el -- a wordpress posting client
;; Copyright (C) 2008 Ashish Shukla

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.


(require 'org)
(require 'xml-rpc)
(require 'metaweblog)
(require 'ox-wp)

(defgroup org2blog/wp nil
  "Post to weblogs from Emacs"
  :group 'org2blog/wp)

(defcustom org2blog/wp-blog-alist nil
  "Association list to set information for each blog.
Each element of the alist is a blog name.  The CAR of each
element is a string, uniquely identifying the project.  The CDR
of each element is a well-formed property list with an even
number of elements, alternating keys and values, specifying
parameters for the blog.

     (:property value :property value ... )

When a property is given a value in org2blog/wp-blog-alist, its
setting overrides the value of the corresponding user
variable (if any) during publishing.

Most properties are optional, but some should always be set:

  :url                     xmlrpc url of the blog.
  :username                username to be used.

All the other properties are optional. They over-ride the global variables.

  :password                password to be used
  :default-title           `org2blog/wp-default-title'
  :default-categories      `org2blog/wp-default-categories'
                           Use a list of categories.
                           (\"category1\" \"category2\" ...)
  :tags-as-categories      `org2blog/wp-use-tags-as-categories'
  :confirm                 `org2blog/wp-confirm-post'
  :show                    `org2blog/wp-show-post-in-browser'
  :keep-new-lines          `org2blog/wp-keep-new-lines'
  :wp-latex                `org2blog/wp-use-wp-latex'
  :wp-code                 `org2blog/wp-use-sourcecode-shortcode'
  :track-posts             `org2blog/wp-track-posts'
"
  :group 'org2blog/wp
  :type '(alist :value-type plist))

(defcustom org2blog/wp-default-categories '("Uncategorized" "Hello")
  "Default list of categories"
  :group 'org2blog/wp
  :type '(repeat string))

(defcustom org2blog/wp-buffer-template
  "#+DATE: %s
#+OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil
#+CATEGORY: %s
#+TAGS:
#+DESCRIPTION:
#+TITLE: %s
\n"
  "The default template to be inserted in a new post buffer."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-buffer-template-prefix nil
  "A prefix to the default template used for a new post buffer."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-buffer-format-function 'org2blog/wp-format-buffer
  "Function formatting a buffer according to `org2blog/wp-buffer-template'."
  :group 'org2blog/wp
  :type 'function)

(defcustom org2blog/wp-default-title "Hello, World"
  "Title of the new post"
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-use-tags-as-categories nil
  "Non-nil means assign :tags: to Wordpress categories instead."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-confirm-post nil
  "Non-nil means confirm before Publishing a post."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-show-post-in-browser 'ask
  "A variable to configure if you want to view your post/draft in
the browser.  Setting it to 'ask will prompt you before opening
it in the browser.  Setting it to 'show will show it without
prompting.  Set it to nil, to turn off viewing posts in the
browser."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-keep-new-lines nil
  "Non-nil means do not strip newlines."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-use-sourcecode-shortcode nil
  "Non-nil means convert <pre> tags to WP sourcecode blocks.
NOTE: htmlize.el available in org-mode's contrib directory should
be on your emacs load-path for this to work."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-use-wp-latex t
  "Non-nil means convert LaTeX to WP latex blocks."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-sourcecode-default-params "light=\"true\""
  "Default arguments to pass to WP syntaxhighlighter."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-sourcecode-langs
  (list "actionscript3" "bash" "coldfusion" "cpp" "csharp" "css" "delphi"
        "erlang" "fsharp" "diff" "groovy" "javascript" "java" "javafx" "matlab"
        "objc" "perl" "php" "text" "powershell" "python" "ruby" "scala" "sql"
        "vb" "xml")
  "List of languages supported by sourcecode shortcode of WP."
  :group 'org2blog/wp
  :type 'list)

(defcustom org2blog/wp-shortcode-langs-map nil
  "Association list for source code languages supported by Org
and by SyntaxHighlighter.  Each element of the list maps the
orgmode source code language (key) to the language spec that
should be used for syntax highlighting in shortcode blocks. The
list of target languages complements the list of languages in
ineed not be in 'org2blog/wp-sourcecode-langs and they already
need NOT be present in 'org2blog/wp-sourcecode-langs."
  :group 'org2blog/wp
  :type '(alist :key-type string :value-type string))

(defcustom org2blog/wp-track-posts
  (list ".org2blog.org" "Posts")
  "File where to save logs about posts.
Set to nil if you don't wish to track posts."
  :group 'org2blog/wp
  :type 'list)

(defcustom org2blog/wp-keymap-prefix
  "C-c M-p"
  "Key sequence which forms the common prefix for key-bindings in
this mode.  If this is changed,
`org2blog/wp-reload-entry-mode-map' must be called before it
takes effect."
  :group 'org2blog/wp
  :type 'string)

(defvar org2blog/wp-blog nil
  "Parameters of the currently selected blog.")

(defvar org2blog/wp-blog-name nil
  "Name of the blog, to pick from `org2blog/wp-blog-alist'")

(defvar org2blog/wp-categories-list nil
  "List of weblog categories")

(defvar org2blog/wp-tags-list nil
  "List of weblog tags")

(defvar org2blog/wp-pages-list nil
  "List of WP pages.")

(defvar org2blog/wp-server-xmlrpc-url nil
  "Weblog server XML-RPC URL")

(defvar org2blog/wp-server-userid nil
  "Weblog server user id")

(defvar org2blog/wp-server-blogid nil
  "Weblog ID")

(defvar org2blog/wp-entry-mode-map nil
  "Keymap for blog entry buffer")

(defvar org2blog/wp-logged-in nil
  "Flag whether user is logged-in or not")

(defvar org2blog/wp-buffer-name "*org2blog/wp-%s*"
  "Name of the blog buffer")

(defvar org2blog/wp-buffer-kill-prompt t
  "Ask before killing buffer")
(make-variable-buffer-local 'org2blog/wp-buffer-kill-prompt)

(defconst org2blog/wp-version "0.5"
  "Current version of blog.el")

(defvar org2blog/wp-mode-hook nil
  "Hook to run upon entry into mode.")

(defvar org2blog/wp-export-options
  '(
    :section-numbers nil
    :with-priority nil
    :with-sub-superscript nil
    :with-toc nil
    :with-tags nil
    :with-todo-keywords nil
    )
  "Export options to be used when exporting buffers and subtrees.
Look at `org-export-options-alist' for the available options.
Also, note that these options are over-ridden by in-file
options.")

(defun org2blog/wp-is-narrow-p nil
  "Return t if a buffer is narrowed"
  (not (equal (- (point-max) (point-min)) (buffer-size))))

(defun org2blog/wp-kill-buffer-hook ()
  "Prompt before killing buffer."
  (if (and org2blog/wp-buffer-kill-prompt
	   (not (buffer-file-name)))
    (if (y-or-n-p "Save entry?")
        (progn
          (save-buffer)
          (org2blog/wp-save-details (org2blog/wp-parse-entry) nil
                                 (y-or-n-p "Published?"))))))

(defun org2blog/wp-define-key (suffix function)
  "Define a key sequence in the mode's key map with the prefix
given by `org2blog/wp-keymap-prefix', and the given suffix."
  (let ((keyseq (read-kbd-macro (concat org2blog/wp-keymap-prefix " " suffix))))
    (define-key org2blog/wp-map keyseq function)))

(defun org2blog/wp-init-entry-mode-map ()
  "Initialize `org2blog/wp-entry-mode-map' based on the prefix
key sequence defined by `org2blog/wp-keymap-prefix'."
  (setq org2blog/wp-entry-mode-map
	(let ((org2blog/wp-map (make-sparse-keymap)))
	  (set-keymap-parent org2blog/wp-map org-mode-map)
	  (org2blog/wp-define-key "p" 'org2blog/wp-post-buffer-and-publish)
	  (org2blog/wp-define-key "P" 'org2blog/wp-post-buffer-as-page-and-publish)
	  (org2blog/wp-define-key "d" 'org2blog/wp-post-buffer)
	  (org2blog/wp-define-key "D" 'org2blog/wp-post-buffer-as-page)
	  (org2blog/wp-define-key "t" 'org2blog/wp-complete-category)
	  org2blog/wp-map)))

(defun org2blog/wp-reload-entry-mode-map ()
  "Re-initialize `org2blog/wp-entry-mode-map' based on the prefix
key sequence defined by `org2blog/wp-keymap-prefix' and update
`minor-mode-map-alist' accordingly."
  (interactive)
  (org2blog/wp-init-entry-mode-map)
  (let ((keymap (assoc 'org2blog/wp-mode minor-mode-map-alist)))
    (setcdr keymap org2blog/wp-entry-mode-map)))

;; Set the mode map for org2blog.
(unless org2blog/wp-entry-mode-map org2blog/wp-init-entry-mode-map)

;;;###autoload
(define-minor-mode org2blog/wp-mode
  "Toggle org2blog/wp mode.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{org2blog/wp-entry-mode-map}

Entry to this mode calls the value of `org2blog/wp-mode-hook'."

  :init-value nil
  :lighter " o2b"
  :group 'org2blog/wp
  :keymap org2blog/wp-entry-mode-map

  (if org2blog/wp-mode
      (run-mode-hooks 'org2blog/wp-mode-hook)))

(defun org2blog/wp-create-categories (categories)
  "Prompt and create new categories on WordPress."
  (mapcar
   (lambda (cat)
     (if (and (not (member cat org2blog/wp-categories-list))
              (y-or-n-p (format "Create '%s' category? " cat)))
         (wp-new-category org2blog/wp-server-xmlrpc-url
                          org2blog/wp-server-userid
                          org2blog/wp-server-pass
                          org2blog/wp-server-blogid
                          cat))
     (add-to-list 'org2blog/wp-categories-list cat))
   categories))

(defun org2blog/wp-password ()
  "Prompt for, and set password."
  (interactive)
  (setq org2blog/wp-server-pass (read-passwd "Weblog password? ")))

(defun org2blog/wp-get-blog-name ()
  "Get the blog name from a post -- buffer or subtree.
NOTE: Checks for subtree only when buffer is narrowed."
  (let ((blog-name
         (if (org2blog/wp-is-narrow-p)
             (or (org-entry-get (point) "BLOG") "")
           (or (org2blog/wp-get-option "blog") ""))))
    (or (and (assoc blog-name org2blog/wp-blog-alist) blog-name) nil)))

(defun org2blog/wp-correctly-login ()
  "Relogin to correct blog, if blog-name is found and different
from currently logged in."
  (let ((blog-name (org2blog/wp-get-blog-name)))
    (when (and blog-name (not (equal blog-name org2blog/wp-blog-name)))
      (org2blog/wp-logout))
    (unless org2blog/wp-logged-in
      (org2blog/wp-login blog-name))))

;;;###autoload
(defun org2blog/wp-login (&optional blog-name)
  "Logs into the blog. Initializes the internal data structures."
  (interactive)
  (if (not org2blog/wp-blog-alist)
      (error "Set `org2blog/wp-blog-alist' to be able to use org2blog."))
  (let ()
    (setq org2blog/wp-blog-name
          (or
           ;; Use the provided name
           blog-name
           ;; OR Use the only entry in alist
           (and (equal (length org2blog/wp-blog-alist) 1)
                (car (car org2blog/wp-blog-alist)))
           ;; OR Prompt user
           (completing-read
            "Blog to login into? ([Tab] to see list): "
            (mapcar 'car org2blog/wp-blog-alist) nil t)))
    (unless (> (length org2blog/wp-blog-name) 1)
      (error "Invalid blog name"))
    (setq org2blog/wp-blog (assoc org2blog/wp-blog-name org2blog/wp-blog-alist)
          org2blog/wp-server-xmlrpc-url (plist-get (cdr org2blog/wp-blog) :url)
          org2blog/wp-server-userid (eval (plist-get (cdr org2blog/wp-blog) :username))
          org2blog/wp-server-blogid (or (plist-get (cdr org2blog/wp-blog) :id) "1")
          org2blog/wp-server-pass
          (or
           (eval (plist-get (cdr org2blog/wp-blog) :password))
           (read-passwd (format "%s Weblog password? " org2blog/wp-blog-name)))
          ;; Fetch and save category list
          org2blog/wp-categories-list
	  (mapcar (lambda (category) (cdr (assoc "categoryName" category)))
		  (metaweblog-get-categories org2blog/wp-server-xmlrpc-url
					     org2blog/wp-server-userid
                                             org2blog/wp-server-pass
					     org2blog/wp-server-blogid))
          ;; Fetch and save tag list
          org2blog/wp-tags-list
	  (mapcar (lambda (tag) (cdr (assoc "slug" tag)))
		  (wp-get-tags org2blog/wp-server-xmlrpc-url
			       org2blog/wp-server-userid
                               org2blog/wp-server-pass
			       org2blog/wp-server-blogid))
          ;; Fetch and save page list
          org2blog/wp-pages-list
	  (mapcar (lambda (pg)
                    (cons (cdr (assoc "page_title" pg))
                          (cdr (assoc "page_id" pg))))
		  (wp-get-pagelist org2blog/wp-server-xmlrpc-url
				   org2blog/wp-server-userid
				   org2blog/wp-server-pass
				   org2blog/wp-server-blogid)))
    (setq org2blog/wp-logged-in t)
    (message "Logged in")))

(defun org2blog/wp-logout()
  "Logs out from the blog and clears. Clears the internal data structures."
  (interactive)
  (setq org2blog/wp-server-xmlrpc-url nil
	org2blog/wp-server-userid nil
	org2blog/wp-server-blogid nil
	org2blog/wp-server-pass nil
	org2blog/wp-categories-list nil
	org2blog/wp-tags-list nil
	org2blog/wp-pages-list nil
	org2blog/wp-logged-in nil)
  (message "Logged out"))

;;;###autoload
(defun org2blog/wp-new-entry ()
  "Creates a new buffer for a blog entry."
  (interactive)
  ;; Prompt for login
  (if (and (not org2blog/wp-logged-in)
           (y-or-n-p "You are not logged in. Login?"))
      (org2blog/wp-login))

  ;; Generate new buffer
  (let ((org2blog/wp-buffer (generate-new-buffer
                          (format org2blog/wp-buffer-name org2blog/wp-blog-name))))
    (switch-to-buffer org2blog/wp-buffer)
    (add-hook 'kill-buffer-hook 'org2blog/wp-kill-buffer-hook nil 'local)
    (org-mode)
    ;; Insert the post template
    (insert
     (or org2blog/wp-buffer-template-prefix "")
     (funcall org2blog/wp-buffer-format-function
              org2blog/wp-buffer-template))
    (org2blog/wp-mode t)))

(defun org2blog/wp-format-buffer (buffer-template)
  "Default buffer formatting function."
  (format buffer-template
     (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))
     (mapconcat
        (lambda (cat) cat)
          (or (plist-get (cdr org2blog/wp-blog) :default-categories)
              org2blog/wp-default-categories)
        ", ")
     (or (plist-get (cdr org2blog/wp-blog) :default-title)
          org2blog/wp-default-title)))

(defun org2blog/wp-upload-files-replace-urls (text)
  "Uploads files, if any in the html, and changes their links"
  (let ((file-all-urls nil)
        file-name file-web-url beg
        (file-regexp "<a href=\"\\(.?*\\)\"\\|<img src=\"\\(.*?\\)\""))
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
        (if (save-match-data (not (or
                                   (string-match org-plain-link-re file-name)
                                   (string-match "^#" file-name)
                                   (string-equal (file-name-nondirectory file-name) ""))))

            (progn
              (goto-char (point-min))
              (if (re-search-forward (concat "^# "
                                             (regexp-quote file-name)
                                             " ") nil t 1)
                  (setq file-web-url (buffer-substring-no-properties
                                      (point)
                                      (or (end-of-line) (point))))
                (setq file-web-url
                      (cdr (assoc "url"
                                  (metaweblog-upload-file
                                   org2blog/wp-server-xmlrpc-url
                                   org2blog/wp-server-userid
                                   org2blog/wp-server-pass
                                   org2blog/wp-server-blogid
                                   (get-file-properties file-name)))))
                (goto-char (point-max))
                (newline)
                (insert (concat "# " file-name " " file-web-url)))
              (setq file-all-urls
                    (append file-all-urls (list (cons
                                                 file-name file-web-url)))))))
      (dolist (file file-all-urls)
        (setq text (replace-regexp-in-string
                    (concat "\\(<a href=\"\\|<img src=\"\\)\\(file://\\)*" (regexp-quote (car file)))
                    (concat "\\1" (cdr file)) text))))
    text))

(defun org2blog/wp-get-option (opt)
  "Gets an the value of the option OP from a buffer."
  (let* ((r (org-make-options-regexp (list (upcase opt) (downcase opt)))))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward r nil t 1)
          (match-string-no-properties 2)))))

(defun org2blog/wp-get-post-parent (post-par)
  "Gets the post's parent from a buffer."
  (if post-par
      (or (cdr (assoc
            (car (split-string post-par "\\( *, *\\)" t))
            org2blog/wp-pages-list))
          "0")
    "0"))

(defun org2blog/wp-parse-entry (&optional subtree-p)
  "Parse an org2blog/wp buffer."
  (interactive "P")
  (let* ((keep-new-lines (if (plist-member (cdr org2blog/wp-blog) :keep-new-lines)
                             (plist-get (cdr org2blog/wp-blog) :keep-new-lines)
                           org2blog/wp-keep-new-lines))
         (wp-latex (if (plist-member (cdr org2blog/wp-blog) :wp-latex)
                       (plist-get (cdr org2blog/wp-blog) :wp-latex)
                     org2blog/wp-use-wp-latex))
         (sourcecode-shortcode (if (plist-member (cdr org2blog/wp-blog) :wp-code)
                                   (plist-get (cdr org2blog/wp-blog) :wp-code)
                                 org2blog/wp-use-sourcecode-shortcode))
         (option-plist org2blog/wp-export-options)
         html-text post-title post-id post-date tags categories
         cur-time post-par)
    (plist-put option-plist :wp-keep-new-lines keep-new-lines)
    (plist-put option-plist :wp-latex wp-latex)
    (plist-put option-plist :wp-shortcode sourcecode-shortcode)
    (plist-put option-plist :wp-shortcode-langs org2blog/wp-sourcecode-langs)
    (plist-put option-plist :wp-shortcode-lang-map org2blog/wp-shortcode-langs-map)
    (save-restriction
      (save-excursion
        ;; Get the required parameters for posting the blog-post
        (if subtree-p
            (progn
              (setq post-title (or (org-entry-get (point) "TITLE")
                                   (nth 4 (org-heading-components))))
              (setq excerpt (org-entry-get (point) "DESCRIPTION"))
              (setq permalink (org-entry-get (point) "PERMALINK"))
              (setq post-id (or (org-entry-get (point) "POSTID")
                                (org-entry-get (point) "POST_ID")))
              (setq post-par (org2blog/wp-get-post-parent
                              (org-entry-get (point) "PARENT")))
              ;; Set post-date to the Post Date property or look for timestamp
              (setq post-date (or (org-entry-get (point) "POST_DATE")
                                  (org-entry-get (point) "SCHEDULED")
                                  (org-entry-get (point) "DEADLINE")
                                  (org-entry-get (point) "TIMESTAMP_IA")
                                  (org-entry-get (point) "TIMESTAMP")))
              (setq tags (mapcar 'org-no-properties (org-get-tags-at (point) nil)))
              (setq categories (org-entry-get (point) "CATEGORY"))
              (setq categories (if categories
                                   (split-string categories "\\( *, *\\)" t)
                                 "")))
          (setq post-title (or (plist-get
                                ;; In org 8 this has been replaced by
                                ;; org-export-get-enviroment
                                (if (version-list-< (version-to-list (org-version)) '(8 0 0))
                                    (org-infile-export-plist)
                                  (org-export-get-environment))
                                :title)
                               "No Title"))
          (setq excerpt (plist-get (if (version-list-< (version-to-list (org-version)) '(8 0 0))
                                       (org-infile-export-plist)
                                     (org-export-get-environment))
                                   :description))
          (setq permalink (org2blog/wp-get-option "PERMALINK"))
          (setq post-id (org2blog/wp-get-option "POSTID"))
          (setq post-par (org2blog/wp-get-post-parent
                          (org2blog/wp-get-option "PARENT")))
          (setq post-date (org2blog/wp-get-option "DATE"))
          (setq tags (org2blog/wp-get-option "TAGS"))
          (setq tags (if tags (split-string tags "\\( *, *\\)" t) ""))

          (setq categories (org2blog/wp-get-option "CATEGORY"))
          (setq categories (if categories
                               (split-string categories "\\( *, *\\)" t)
                             "")))

        ;; Convert post date to ISO timestamp
        ;;add the date of posting to the post. otherwise edits will change it
        (setq cur-time (format-time-string (org-time-stamp-format t t) (org-current-time)))
        (setq post-date
              (format-time-string "%Y%m%dT%T%z"
                                  (if post-date
                                      (apply 'encode-time (org-parse-time-string post-date))
                                    (current-time)
                                    (if subtree-p
                                        (org-entry-put (point) "POST_DATE" cur-time)
                                      (save-excursion
                                        (goto-char (point-min))
                                        (insert (concat "#+DATE: " cur-time "\n")))))
                                  t))

        (if
            (if (plist-member (cdr org2blog/wp-blog) :tags-as-categories)
                (plist-get (cdr org2blog/wp-blog) :tags-as-categories)
              org2blog/wp-use-tags-as-categories)
            (setq categories tags
                  tags nil))

        ;; Get the exported html
        (save-excursion
          (setq html-text (org-wp-export-as-string nil subtree-p option-plist))
          (setq html-text (org-no-properties html-text)))

        ;; Post-process as required.
        (setq html-text (org2blog/wp-upload-files-replace-urls html-text))))

    ;; Return value
    (list
     (cons "point" (point))
     (cons "date" post-date)
     (cons "title" (if (version-list-< (version-to-list (org-version)) '(8 0 0))
                       (org-html-do-expand post-title)
                     (org-element-interpret-data post-title)))
     (cons "tags" tags)
     (cons "categories" categories)
     (cons "post-id" post-id)
     (cons "parent" post-par)
     (cons "excerpt" (if (version-list-< (version-to-list (org-version)) '(8 0 0))
                         (org-html-do-expand (or excerpt ""))
                       (org-element-interpret-data (or excerpt ""))))
     (cons "permalink" (or permalink ""))
     (cons "description" html-text))))


(defun org2blog/wp-post-buffer-and-publish ()
  "Post buffer and mark it as published"
  (interactive)
  (org2blog/wp-post-buffer t))

(defun org2blog/wp-post-buffer (&optional publish subtree-p)
  "Posts new blog entry to the blog or edits an existing entry."
  (interactive "P")
  (org2blog/wp-mode t) ;; turn on org2blog-wp-mode
  (org2blog/wp-correctly-login)
  (save-excursion
    (save-restriction
      (let ((post (org2blog/wp-parse-entry subtree-p))
            (confirm (and
                      (if (plist-member (cdr org2blog/wp-blog) :confirm)
                          (plist-member (cdr org2blog/wp-blog) :confirm)
                        org2blog/wp-confirm-post)
                      publish))
            (show (or (plist-member (cdr org2blog/wp-blog) :show)
                      org2blog/wp-show-post-in-browser))
            post-id)
        (org2blog/wp-create-categories (cdr (assoc "categories" post)))
        (setq post-id (cdr (assoc "post-id" post)))
        (when confirm
          (if (not (y-or-n-p (format "Publish %s ?"
                                     (cdr (assoc "title" post)))))
              (error "Post cancelled.")))
        (if post-id
            (metaweblog-edit-post org2blog/wp-server-xmlrpc-url
                                  org2blog/wp-server-userid
                                  org2blog/wp-server-pass
                                  post-id
                                  post
                                  publish)
          (setq post-id (metaweblog-new-post org2blog/wp-server-xmlrpc-url
                                             org2blog/wp-server-userid
                                             org2blog/wp-server-pass
                                             org2blog/wp-server-blogid
                                             post
                                             publish))
          (if subtree-p
              (progn
                (org-entry-put (point) "POSTID" post-id)
                (org-entry-put (point) "BLOG" org2blog/wp-blog-name))
            (goto-char (point-min))
            (insert (concat "#+BLOG: " org2blog/wp-blog-name "\n"))
            (insert (concat "#+POSTID: " post-id "\n"))))
        (org2blog/wp-save-details post post-id publish subtree-p)
        (message (if publish
                     "Published (%s): %s"
                   "Draft (%s): %s")
                 post-id
                 (cdr (assoc "title" post)))
        (when (or (equal show 'show)
                  (and
                   (equal show 'ask)
                   (y-or-n-p
                    "[For drafts, ensure you login] View in browser? y/n")))
          (if subtree-p
              (org2blog/wp-preview-subtree-post)
            (org2blog/wp-preview-buffer-post)))))))


(defun org2blog/wp-post-buffer-as-page-and-publish ()
  "Alias to post buffer and mark it as published"
  (interactive)
  (org2blog/wp-post-buffer-as-page t))

(defun org2blog/wp-post-buffer-as-page (&optional publish subtree-p)
  "Posts new page to the blog or edits an existing page."
  (interactive "P")
  (org2blog/wp-correctly-login)
  (save-excursion
    (save-restriction
      (widen)
      (let ((post (org2blog/wp-parse-entry subtree-p))
            (confirm (and
                     (if (plist-member (cdr org2blog/wp-blog) :confirm)
                        (plist-member (cdr org2blog/wp-blog) :confirm)
                      org2blog/wp-confirm-post)
                     publish))
            (show (or (plist-member (cdr org2blog/wp-blog) :show)
                      org2blog/wp-show-post-in-browser))
            post-id)
        (org2blog/wp-create-categories (cdr (assoc "categories" post)))
        (setq post-id (cdr (assoc "post-id" post)))
        (when confirm
          (if (not (y-or-n-p (format "Publish %s ?"
                                     (cdr (assoc "title" post)))))
              (error "Post cancelled.")))
        (if post-id
            (wp-edit-page org2blog/wp-server-xmlrpc-url
                          org2blog/wp-server-userid
                          org2blog/wp-server-pass
                          org2blog/wp-server-blogid
                          post-id
                          post
                          publish)
          (setq post-id (wp-new-page org2blog/wp-server-xmlrpc-url
                                     org2blog/wp-server-userid
                                     org2blog/wp-server-pass
                                     org2blog/wp-server-blogid
                                     post
                                     publish))
          (setq org2blog/wp-pages-list
                (mapcar (lambda (pg)
                          (cons (cdr (assoc "title" pg))
                                (cdr (assoc "page_id" pg))))
                        (wp-get-pagelist org2blog/wp-server-xmlrpc-url
					 org2blog/wp-server-userid
					 org2blog/wp-server-pass
					 org2blog/wp-server-blogid)))
          (if subtree-p
              (org-entry-put (point) "POSTID" post-id)
            (goto-char (point-min))
            (insert (concat "#+POSTID: " post-id "\n"))))
        (org2blog/wp-save-details post post-id publish subtree-p)
        (message (if publish
                     "Published (%s): %s"
                   "Draft (%s): %s")
                 post-id
                 (cdr (assoc "title" post)))
        (when (or (equal show 'show)
                  (and
                   (equal show 'ask)
                   (y-or-n-p
                    "[For drafts, ensure you login] View in browser? y/n")))
          (if subtree-p
              (org2blog/wp-preview-subtree-post)
            (org2blog/wp-preview-buffer-post)))))))

(defun org2blog/wp-delete-entry (&optional post-id)
  (interactive "P")
  (org2blog/wp-correctly-login)
  (if (null post-id)
      (setq post-id (org2blog/wp-get-option "POSTID")))
  (metaweblog-delete-post org2blog/wp-server-xmlrpc-url
                                org2blog/wp-server-userid
                                org2blog/wp-server-pass
                                post-id)
  (message "Post Deleted"))

(defun org2blog/wp-delete-page (&optional page-id)
  (interactive "P")
  (org2blog/wp-correctly-login)
  (if (null page-id)
      (setq page-id (org2blog/wp-get-option "POSTID")))
  (wp-delete-page org2blog/wp-server-xmlrpc-url
                  org2blog/wp-server-blogid
                  org2blog/wp-server-userid
                  org2blog/wp-server-pass
                  page-id)
   (message "Page Deleted"))

(defun org2blog/wp-save-details (post pid pub subtree-p)
  "Save the details of posting, to a file."
  (save-excursion
    (when (if (plist-member (cdr org2blog/wp-blog) :track-posts)
              (car (plist-get (cdr org2blog/wp-blog) :track-posts))
            (car org2blog/wp-track-posts))
      (let* ((o2b-id (if subtree-p
                         (concat "id:" (org-id-get nil t))
                       (buffer-file-name)))
             (log-file (if (plist-member (cdr org2blog/wp-blog) :track-posts)
                           (car (plist-get (cdr org2blog/wp-blog) :track-posts))
                         (car org2blog/wp-track-posts)))
             (log-file (if (file-name-absolute-p log-file)
                           log-file
                         (if org-directory
                             (expand-file-name log-file org-directory)
                           (message "org-track-posts: filename is ambiguous
use absolute path or set org-directory")
                           log-file)))
             (headline (if (plist-member (cdr org2blog/wp-blog) :track-posts)
                           (cadr (plist-get (cdr org2blog/wp-blog) :track-posts))
                         (cadr org2blog/wp-track-posts)))
             p)
        (when o2b-id
          (with-current-buffer (or (find-buffer-visiting log-file)
                                   (find-file-noselect log-file))
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (show-all)
                (setq p (org-find-exact-headline-in-buffer headline))
                (if p
                    (progn (goto-char p) (org-narrow-to-subtree) (end-of-line))
                  (goto-char (point-max))
                  (if (y-or-n-p (format "Heading '%s' not in '%s'; Create?"
                                        headline log-file))
                      (progn (org-insert-heading t) (insert headline)
                             (org-narrow-to-subtree))))
                (if (search-forward o2b-id nil t 1)
                    (progn
                      (org-back-to-heading)
                      (forward-thing 'whitespace)
                      (kill-line))
                  (org-insert-subheading t)))
              (org2blog/wp-update-details post o2b-id pid pub))
            (save-buffer)))))))

(defun org2blog/wp-update-details (post o2b-id pid pub)
  "Inserts details of a new post or updates details."
  (insert (format "[[%s][%s]]"
                  (if (cdr (assoc "subtree" post))
                      o2b-id
                    (concat "file:" o2b-id))
                  (cdr (assoc "title" post))))
  (org-entry-put (point) "POSTID" (or pid ""))
  (org-entry-put (point) "POST_DATE" (cdr (assoc "date" post)))
  (org-entry-put (point) "Published" (if pub "Yes" "No")))

(defun org2blog/wp-complete-category()
  "Provides completion for categories and tags."
  (interactive)
  (let* (current-pos tag-or-category-list)
    (setq current-pos (point))
    (forward-line 0)
    (forward-char 2)
    (if (or (looking-at "CATEGORY: ") (looking-at "TAGS: ")
            (looking-at "PARENT: "))
        (progn
          (cond
           ((looking-at "TAGS: ")
            (setq tag-or-cat-list org2blog/wp-tags-list)
	    (setq tag-or-cat-prompt "Tag ?"))
           ((looking-at "CATEGORY: ")
            (setq tag-or-cat-list org2blog/wp-categories-list)
	    (setq tag-or-cat-prompt "Category ?"))
           ((looking-at "PARENT: ")
            (setq tag-or-cat-list org2blog/wp-pages-list)
	    (setq tag-or-cat-prompt "Parent ?")))
          (goto-char current-pos)
      	  (let ((word-match (or (current-word t) ""))
      		(completion-match nil))
      	    (when word-match
      	      (setq completion-match (completing-read tag-or-cat-prompt tag-or-cat-list nil nil word-match))
      	      (when (stringp completion-match)
      		(search-backward word-match nil t)
                (replace-match (concat completion-match ", ") nil t)))))
      (progn
      	(goto-char current-pos)
      	(command-execute (lookup-key org-mode-map (kbd "C-c t")))))))

;;;###autoload
(defun org2blog/wp-post-subtree (&optional publish)
  "Post the current entry as a draft. Publish if PUBLISH is non-nil."
  (interactive "P")
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (org-id-get nil t "o2b")
      (org2blog/wp-post-buffer publish t)
      (widen)
      (when (buffer-file-name) (save-buffer)))))

;;;###autoload
(defun org2blog/wp-post-subtree-as-page (&optional publish)
  "Post the current entry as a draft. Publish if PUBLISH is non-nil."
  (interactive "P")
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (org-id-get nil t "o2b")
      (org2blog/wp-post-buffer-as-page publish t)
      (widen)
      (when (buffer-file-name) (save-buffer)))))

;;;###autoload
(defun org2blog/wp-post-subtree-as-page-and-publish ()
  "Publish the current subtree as a page."
  (interactive)
  (org2blog/wp-post-subtree-as-page t))


;;;###autoload
(defun org2blog/wp-track-buffer ()
  "Save details of current buffer in the tracking file."
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (org2blog/wp-save-details (org2blog/wp-parse-entry) "" nil))))

;;;###autoload
(defun org2blog/wp-track-subtree ()
  "Save details of current subtree in the tracking file."
  (interactive)
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (org2blog/wp-save-details (org2blog/wp-parse-entry t) "" nil t)
      (widen))))

;;;###autoload
(defun org2blog/wp-preview-buffer-post ()
  "Preview the present buffer in browser, if posted."
  (interactive)
  (org2blog/wp-correctly-login)
  (let* ((postid (org2blog/wp-get-option "POSTID"))
         (url org2blog/wp-server-xmlrpc-url))
    (if (not postid)
        (message "This buffer hasn't been posted, yet.")
      (setq url (substring url 0 -10))
      (setq url (concat url "?p=" postid "&preview=true"))
      (browse-url url))))

;;;###autoload
(defun org2blog/wp-preview-subtree-post ()
  "Preview the present subtree in browser, if posted."
  (interactive)
  (org-narrow-to-subtree)
  (org2blog/wp-correctly-login)
  (widen)
  (let* ((postid (or (org-entry-get (point) "POSTID")
                     (org-entry-get (point) "POST_ID")))
         (url org2blog/wp-server-xmlrpc-url))
    (if (not postid)
        (message "This subtree hasn't been posted, yet.")
      (setq url (substring url 0 -10))
      (setq url (concat url "?p=" postid "&preview=true"))
      (browse-url url))))

(defun org2blog/wp-insert-post-or-page-link (&optional is-page)
  "Insert a link to the post (or page) with the given id, with
the title of the post (or page) as description."
  (interactive "P")
  (org2blog/wp-correctly-login)
  (let* ((post-list (if is-page
                        (wp-get-pagelist org2blog/wp-server-xmlrpc-url
                                         org2blog/wp-server-userid
                                         org2blog/wp-server-pass
                                         org2blog/wp-server-blogid)
                      (metaweblog-get-recent-posts org2blog/wp-server-xmlrpc-url
                                                   org2blog/wp-server-blogid
                                                   org2blog/wp-server-userid
                                                   org2blog/wp-server-pass
                                                   1000)))
         post-title url title-id-map)
    (dolist (post post-list)
        (setq title-id-map (cons
                            (cons (cdr (assoc "title" post)) (cdr (assoc "postid" post)))
                            title-id-map)))
    ;; Ask user to select the title
    (setq post-title (completing-read
                      (if is-page "Select page: " "Select post: ")
                      title-id-map nil t)
          post-id (cdr (assoc post-title title-id-map)))
    (if post-title
        ;; "Generate" the actual url of the post
        (setq url (concat
                   (replace-regexp-in-string "xmlrpc\\.php$" "?p=" org2blog/wp-server-xmlrpc-url)
                   post-id))
      ;; Insert!
      (insert (format "[[%s][%s]]" url post-title)))))

(provide 'org2blog)
