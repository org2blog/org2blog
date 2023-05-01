;;; metaweblog.el --- An XML-RPC MetaWeblog and WordPress API client -*- lexical-binding: t; -*-

;; Copyright (C) 2008 Ashish Shukla
;; Copyright (C) 2010 Puneeth Chaganti
;; Copyright (C) 2019-2022 Grant Rettke <grant@wisdomandwonder.com>

;; Author: Puneeth Chaganti <punchagan+org2blog@gmail.com>
;; Maintainer: Grant Rettke <grant@wisdomandwonder.com>
;; Version: 1.1.16
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm
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

;; This API operates against WordPress using both the
;; XML-RPC MetaWeblog API specification
;; https://codex.wordpress.org/XML-RPC_MetaWeblog_API#metaWeblog.getRecentPosts
;; and the XML-RPC WordPress API
;; https://codex.wordpress.org/XML-RPC_WordPress_API

;; This API operates against a blog using its RPC endpoint URL ‘BLOG-XMLRPC',
;; a user name `USER-NAME', a password `PASSWORD', and a blog ID ‘BLOG-ID'.
;; These parameters are used for every API call. When their intention is
;; obvious and as expected they are excluded from documentation by
;; including them in line at the end of the docstring only to satisfy
;; the byte-compiler.

;; A portion of the code in this file is based on blog.el posted to
;; http://www.mail-archive.com/gnu-emacs-sources@gnu.org/msg01576.html
;; copyrighted by Ashish Shukla.

;;; Code:

;;; Require

(require 'xml-rpc)

;;; Constants

(defconst metaweblog-version "1.1.16"
  "Current version of metaweblog.el.")

;;; Deprecations

(defconst metaweblog-deprecation "2.0.0"
  "Release in which obselete objects will be removed.")

(mapc (lambda (ls)
        (define-obsolete-function-alias (car ls) (cadr ls) metaweblog-deprecation))
      '((wp-new-category metaweblog-wp-new-category)
        (wp-get-tags metaweblog-wp-get-tags)
        (wp-get-pages metaweblog-wp-get-pages)
        (wp-get-pagelist metaweblog-wp-get-pagelist)
        (wp-new-page metaweblog-wp-new-page)
        (wp-edit-page metaweblog-wp-edit-page)
        (wp-delete-page metaweblog-wp-delete-page)
        (get-file-properties metaweblog-get-file-properties)))

;;; Function - Public

(defun metaweblog-get-categories (blog-xmlrpc user-name password blog-id)
  "Retrieve list of Categories.

BLOG-XMLRPC USER-NAME PASSWORD BLOG-ID"
  (xml-rpc-method-call blog-xmlrpc
                       "metaWeblog.getCategories"
                       blog-id
                       user-name
                       password))

(defun metaweblog-wp-new-category (blog-xmlrpc user-name password blog-id category)
  "Create Category CATEGORY.

BLOG-XMLRPC USER-NAME PASSWORD BLOG-ID"
  (xml-rpc-method-call blog-xmlrpc
                       "wp.newCategory"
                       blog-id
                       user-name
                       password
                       `(("name" . ,category))))

(defun metaweblog-wp-get-tags (blog-xmlrpc user-name password blog-id)
  "Retrieve list of Tags.

BLOG-XMLRPC USER-NAME PASSWORD BLOG-ID"
  (xml-rpc-method-call blog-xmlrpc
                       "wp.getTags"
                       blog-id
                       user-name
                       password))

(defun metaweblog-wp-get-pages (blog-xmlrpc user-name password blog-id)
  "Retrieve list of Pages with entire page content.

BLOG-XMLRPC USER-NAME PASSWORD BLOG-ID"
  (xml-rpc-method-call blog-xmlrpc
                       "wp.getPages"
                       blog-id
                       user-name
                       password))

(defun metaweblog-wp-get-pagelist (blog-xmlrpc user-name password blog-id)
  "Retrieve list of Pages with minimal page content.

BLOG-XMLRPC USER-NAME PASSWORD BLOG-ID"
  (xml-rpc-method-call blog-xmlrpc
                       "wp.getPageList"
                       blog-id
                       user-name
                       password))

(defun metaweblog-new-post
    (blog-xmlrpc user-name password blog-id content publish)
  "Create a new Post.

If PUBLISH is non-nil, the post is published, otherwise it is
saved as draft. CONTENT will be an alist title, description,
categories, and date as keys (string-ified) mapped to the title
of the post, post contents, list of categories, and date
respectively.

Since `xml-rpc-method-call' entity-fies the HTML text in the post
we've to use raw.

BLOG-XMLRPC USER-NAME PASSWORD BLOG-ID"
  (let ((post-title (cdr (assoc "title" content)))
        (post-description (cdr (assoc "description" content)))
        (post-categories (cdr (assoc "categories" content)))
        (post-tags (cdr (assoc "tags" content)))
        (post-excerpt (cdr (assoc "excerpt" content)))
        (post-permalink (cdr (assoc "permalink" content)))
        (post-date (cdr (assoc "date" content))))
    (xml-rpc-xml-to-response
     (xml-rpc-request
      blog-xmlrpc
      `((methodCall
         nil
         (methodName nil "metaWeblog.newPost")
         (params nil
                 (param nil (value nil (string nil ,blog-id)))
                 (param nil (value nil (string nil ,user-name)))
                 (param nil (value nil (string nil ,password)))
                 (param nil (value nil
                                   (struct
                                    nil
                                    (member nil
                                            (name nil "title")
                                            (value nil ,post-title))
                                    (member nil
                                            (name nil "description")
                                            (value nil ,post-description))
                                    (member nil
                                            (name nil "mt_excerpt")
                                            (value nil ,post-excerpt))
                                    (member nil
                                            (name nil "wp_slug")
                                            (value nil ,post-permalink))
                                    (member nil
                                            (name nil "dateCreated")
                                            (dateTime.iso8601 nil ,post-date))
                                    ,(when post-tags
                                       `(member nil
                                                (name nil "mt_keywords")
                                                (value nil
                                                       (array
                                                        nil
                                                        ,(append
                                                          '(data nil)
                                                          (mapcar
                                                           (lambda(f)
                                                             `(value nil (string nil ,f)))
                                                           post-tags))))))
                                    ,(when post-categories
                                       `(member nil
                                                (name nil "categories")
                                                (value nil
                                                       (array
                                                        nil
                                                        ,(append
                                                          '(data nil)
                                                          (mapcar
                                                           (lambda(f)
                                                             `(value nil (string nil ,f)))
                                                           post-categories)))))))))
                 (param nil (value nil (boolean nil ,(if publish "1" "0")))))))))))

(defun metaweblog-wp-new-page
    (blog-xmlrpc user-name password blog-id content publish)
  "Sends a new page to the blog.

If PUBLISH is non-nil, the post is published, otherwise it is
saved as draft. CONTENT will be an alist title, description,
categories, and date as keys (string-ified) mapped to the title
of the post, post contents, list of categories, and date
respectively.

Since `xml-rpc-method-call' entity-fies the HTML text in the post
we've to use raw.

BLOG-XMLRPC USER-NAME PASSWORD BLOG-ID"
  (let ((post-title (cdr (assoc "title" content)))
        (post-description (cdr (assoc "description" content)))
        (post-categories (cdr (assoc "categories" content)))
        (post-tags (cdr (assoc "tags" content)))
        (post-excerpt (cdr (assoc "excerpt" content)))
        (post-permalink (cdr (assoc "permalink" content)))
        (post-parent (cdr (assoc "parent" content)))
        (post-date (cdr (assoc "date" content))))
    (xml-rpc-xml-to-response
     (xml-rpc-request
      blog-xmlrpc
      `((methodCall
         nil
         (methodName nil "wp.newPage")
         (params nil
                 (param nil (value nil (string nil ,blog-id)))
                 (param nil (value nil (string nil ,user-name)))
                 (param nil (value nil (string nil ,password)))
                 (param nil (value nil
                                   (struct
                                    nil
                                    (member nil
                                            (name nil "title")
                                            (value nil ,post-title))
                                    (member nil
                                            (name nil "description")
                                            (value nil ,post-description))
                                    (member nil
                                            (name nil "mt_excerpt")
                                            (value nil ,post-excerpt))
                                    (member nil
                                            (name nil "wp_slug")
                                            (value nil ,post-permalink))
                                    (member nil
                                            (name nil "wp_page_parent_id")
                                            (value nil ,post-parent))
                                    (member nil
                                            (name nil "dateCreated")
                                            (dateTime.iso8601 nil ,post-date))
                                    ,(when post-tags
                                       `(member nil
                                                (name nil "mt_keywords")
                                                (value nil
                                                       (array
                                                        nil
                                                        ,(append
                                                          '(data nil)
                                                          (mapcar
                                                           (lambda(f)
                                                             `(value nil (string nil ,f)))
                                                           post-tags))))))
                                    ,(when post-categories
                                       `(member nil
                                                (name nil "categories")
                                                (value nil
                                                       (array
                                                        nil
                                                        ,(append
                                                          '(data nil)
                                                          (mapcar
                                                           (lambda(f)
                                                             `(value nil (string nil ,f)))
                                                           post-categories)))))))))
                 (param nil (value nil (boolean nil ,(if publish "1" "0")))))))))))

(defun metaweblog-wp-edit-page
    (blog-xmlrpc user-name password blog-id page-id content publish)
  "Edits an existing page PAGE-ID on the blog.

If PUBLISH is non-nil, the post is published, otherwise it is
saved as draft. CONTENT will be an alist title, description,
categories, and date as keys (string-ified) mapped to the title
of the post, post contents, list of categories, and date
respectively.

Since `xml-rpc-method-call' entity-fies the HTML text in the post
we've to use raw

BLOG-XMLRPC USER-NAME PASSWORD BLOG-ID"
  (let ((post-title (cdr (assoc "title" content)))
        (post-description (cdr (assoc "description" content)))
        (post-categories (cdr (assoc "categories" content)))
        (post-tags (cdr (assoc "tags" content)))
        (post-excerpt (cdr (assoc "excerpt" content)))
        (post-permalink (cdr (assoc "permalink" content)))
        (post-parent (cdr (assoc "parent" content)))
        (post-date (cdr (assoc "date" content))))
    (message post-date)
    (xml-rpc-xml-to-response
     (xml-rpc-request
      blog-xmlrpc
      `((methodCall
         nil
         (methodName nil "wp.editPage")
         (params nil
                 (param nil (value nil (string nil ,blog-id)))
                 (param nil (value nil (string nil ,page-id)))
                 (param nil (value nil (string nil ,user-name)))
                 (param nil (value nil (string nil ,password)))
                 (param nil (value nil
                                   (struct
                                    nil
                                    (member nil
                                            (name nil "title")
                                            (value nil ,post-title))
                                    (member nil
                                            (name nil "description")
                                            (value nil ,post-description))
                                    (member nil
                                            (name nil "mt_excerpt")
                                            (value nil ,post-excerpt))
                                    (member nil
                                            (name nil "wp_slug")
                                            (value nil ,post-permalink))
                                    (member nil
                                            (name nil "wp_page_parent_id")
                                            (value nil ,post-parent))
                                    (member nil
                                            (name nil "dateCreated")
                                            (dateTime.iso8601 nil ,post-date))
                                    ,(when post-tags
                                       `(member nil
                                                (name nil "mt_keywords")
                                                (value nil
                                                       (array
                                                        nil
                                                        ,(append
                                                          '(data nil)
                                                          (mapcar
                                                           (lambda(f)
                                                             `(value nil (string nil ,f)))
                                                           post-tags))))))
                                    ,(when post-categories
                                       `(member nil
                                                (name nil "categories")
                                                (value nil
                                                       (array
                                                        nil
                                                        ,(append
                                                          '(data nil)
                                                          (mapcar
                                                           (lambda(f)
                                                             `(value nil (string nil ,f)))
                                                           post-categories)))))))))
                 (param nil (value nil (boolean nil ,(if publish "1" "0")))))))))))

(defun metaweblog-edit-post
    (blog-xmlrpc user-name password post-id content publish)
  "Edit an exiting Post POST-ID.

If PUBLISH is non-nil, the post is published, otherwise it is
saved as draft. CONTENT will be an alist title, description,
categories, and date as keys (string-ified) mapped to the title
of the post, post contents, list of categories, and date
respectively.

Since `xml-rpc-method-call' entity-fies the HTML text in the post
we've to use raw.

BLOG-XMLRPC USER-NAME PASSWORD BLOG-ID"
  (let ((post-title (cdr (assoc "title" content)))
        (post-description (cdr (assoc "description" content)))
        (post-categories (cdr (assoc "categories" content)))
        (post-tags (cdr (assoc "tags" content)))
        (post-excerpt (cdr (assoc "excerpt" content)))
        (post-permalink (cdr (assoc "permalink" content)))
        (post-date (cdr (assoc "date" content))))
    (message post-date)
    (xml-rpc-xml-to-response
     (xml-rpc-request
      blog-xmlrpc
      `((methodCall
         nil
         (methodName nil "metaWeblog.editPost")
         (params nil
                 (param nil (value nil (string nil ,post-id)))
                 (param nil (value nil (string nil ,user-name)))
                 (param nil (value nil (string nil ,password)))
                 (param nil (value nil
                                   (struct
                                    nil
                                    (member nil
                                            (name nil "title")
                                            (value nil ,post-title))
                                    (member nil
                                            (name nil "description")
                                            (value nil ,post-description))
                                    (member nil
                                            (name nil "mt_excerpt")
                                            (value nil ,post-excerpt))
                                    (member nil
                                            (name nil "wp_slug")
                                            (value nil ,post-permalink))
                                    (member nil
                                            (name nil "dateCreated")
                                            (dateTime.iso8601 nil ,post-date))
                                    ,(when post-tags
                                       `(member nil
                                                (name nil "mt_keywords")
                                                (value nil
                                                       (array
                                                        nil
                                                        ,(append
                                                          '(data nil)
                                                          (mapcar
                                                           (lambda(f)
                                                             `(value nil (string nil ,f)))
                                                           post-tags))))))
                                    ,(when post-categories
                                       `(member nil
                                                (name nil "categories")
                                                (value nil
                                                       (array
                                                        nil
                                                        ,(append
                                                          '(data nil)
                                                          (mapcar
                                                           (lambda(f)
                                                             `(value nil (string nil ,f)))
                                                           post-categories)))))))))
                 (param nil (value nil (boolean nil ,(if publish "1" "0")))))))))))

(defun metaweblog-get-post (blog-xmlrpc user-name password post-id)
  "Retrieve a post POST-ID.

Also works for Pages.

BLOG-XMLRPC USER-NAME PASSWORD BLOG-ID"
  (xml-rpc-method-call blog-xmlrpc
                       "metaWeblog.getPost"
                       post-id
                       user-name
                       password))

(defun metaweblog-delete-post (blog-xmlrpc app-key user-name password post-id)
  "Delete an existing Post POST-ID.

BLOG-XMLRPC APP-KEY USER-NAME PASSWORD BLOG-ID"
  (xml-rpc-method-call blog-xmlrpc
                       "metaWeblog.deletePost"
                       app-key
                       post-id
                       user-name
                       password
                       t))

(defun metaweblog-wp-delete-page (blog-xmlrpc blog-id user-name password page-id)
  "Delete a page PAGE-ID from the weblog system.

BLOG-XMLRPC USER-NAME PASSWORD BLOG-ID"
  (xml-rpc-method-call blog-xmlrpc
                       "wp.deletePage"
                       blog-id
                       user-name
                       password
                       page-id))

(defun metaweblog-get-recent-posts(blog-xmlrpc blog-id user-name password number-of-posts)
  "Retrieve a list of recent Posts.

NUMBER-OF-POSTS is the no. of posts that should be returned.

BLOG-XMLRPC USER-NAME PASSWORD BLOG-ID"
  (xml-rpc-method-call blog-xmlrpc
                       "metaWeblog.getRecentPosts"
                       blog-id
                       user-name
                       password
                       number-of-posts))

(defun metaweblog-get-file-properties (file)
  "Gets the properties of a file FILE.

Returns an assoc list with
name - file name
bits - data of the file as a base64 encoded string
type - mimetype of file deduced from extension.

BLOG-XMLRPC USER-NAME PASSWORD BLOG-ID"
  (let* (base64-str type name file-props)
    (save-excursion
      (save-restriction
        (with-current-buffer (find-file-noselect file nil t)
          (fundamental-mode)
          (setq name (file-name-nondirectory file))
          (setq base64-str (base64-encode-string (encode-coding-string (buffer-string) 'binary)))
          (setq type (mailcap-extension-to-mime (or (file-name-extension file) "")))
          (kill-buffer)
          (setq file-props `(("name" . ,name)
                             ("bits" . ,base64-str)
                             ("type" . ,type))))))
    file-props))

(defun metaweblog-upload-file (blog-xmlrpc user-name password blog-id file)
  "Upload a media file FILE.

FILE will be an alist name, type, bits, as keys mapped to name of
the file, mime type and the data.

BLOG-XMLRPC USER-NAME PASSWORD BLOG-ID"
  (let ((file-name (cdr (assoc "name" file)))
        (file-type (cdr (assoc "type" file)))
        (file-bits (cdr (assoc "bits" file))))

    (xml-rpc-xml-to-response
     (xml-rpc-request
      blog-xmlrpc
      `((methodCall
         nil
         (methodName nil "metaWeblog.newMediaObject")
         (params nil
                 (param nil (value nil (string nil ,blog-id)))
                 (param nil (value nil (string nil ,user-name)))
                 (param nil (value nil (string nil ,password)))
                 (param nil (value nil
                                   (struct
                                    nil
                                    (member nil
                                            (name nil "name")
                                            (value nil ,file-name))
                                    (member nil
                                            (name nil "bits")
                                            (base64 nil ,file-bits))
                                    (member nil
                                            (name nil "type")
                                            (value nil ,file-type))
                                    (member nil
                                            (name nil "overwrite")
                                            (value nil "t"))))))))))))

(defun metaweblog-get-users-blogs (blog-xmlrpc app-key user-name password)
  "Retrieve list of blogs to which USER-NAME can post.

BLOG-XMLRPC APP-KEY USER-NAME PASSWORD"
  (xml-rpc-method-call blog-xmlrpc
                       "metaWeblog.getUsersBlogs"
                       app-key
                       user-name
                       password))

(provide 'metaweblog)

;;; metaweblog.el ends here
