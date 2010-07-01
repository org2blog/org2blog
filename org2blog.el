;;; org2blog.el --- blog from Org mode to wordpress
;; Copyright (C) 2010 Puneeth Chaganti

;; Author: Puneeth Chaganti <punchagan at gmail dot com>

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

(defgroup org2blog nil 
  "Post to weblogs from Emacs" 
  :group 'org2blog)

(defcustom org2blog-server-url nil 
  "Weblog XML-RPC URL" 
  :group 'org2blog
  :type 'string)

(defcustom org2blog-server-user nil 
  "Weblog server username" 
  :group 'org2blog 
  :type 'string)

(defcustom org2blog-server-pass nil 
  "Weblog server password. If this is nil you'll be prompted." 
  :group 'org2blog 
  :type 'string) 

(defcustom org2blog-server-weblog-id nil 
  "Weblog ID" 
  :group 'org2blog 
  :type 'string)

(defcustom org2blog-default-categories '("Uncategorized") 
  "Default list of categories" 
  :group 'org2blog 
  :type '(repeat string))

(defcustom org2blog-default-title "Hello, World" 
  "Title of the new post" 
  :group 'org2blog 
  :type 'string)

(defvar org2blog-categories-list nil 
  "List of weblog categories")

(defvar org2blog-tags-list nil 
  "List of weblog tags")

(defvar org2blog-server-xmlrpc-url nil 
  "Weblog server XML-RPC URL")

(defvar org2blog-server-userid nil 
  "Weblog server user id")

(defvar org2blog-server-blogid nil 
  "Weblog ID")

(defvar org2blog-entry-mode-map nil 
  "Keymap for blog entry buffer")

(defvar org2blog-logged-in nil 
  "Flag whether user is logged-in or not")

(defvar org2blog-buffer-name "*org2blog*" 
  "Name of the blog buffer")

(defvar org2blog-buffer-kill-prompt t
  "Ask before killing buffer")

(make-variable-buffer-local 'org2blog-buffer-kill-prompt)

(defconst org2blog-version "0.2" 
  "Current version of blog.el")

(defun org2blog-kill-buffer-hook ()
  "Prompt before killing buffer."
  (if (and org2blog-buffer-kill-prompt
	   (not (buffer-file-name)))
    (if (y-or-n-p "Save entry?")
	(save-buffer))))

(unless org2blog-entry-mode-map
  (setq org2blog-entry-mode-map
	(let ((org2blog-map (make-sparse-keymap)))
	  (set-keymap-parent org2blog-map org-mode-map)
	  (define-key org2blog-map (kbd "C-c p") (lambda() (interactive) (org2blog-post-entry t)))
	  (define-key org2blog-map (kbd "C-c d") 'org2blog-post-entry)
	  (define-key org2blog-map (kbd "C-c t") 'org2blog-complete-category)
	  org2blog-map)))

(defun org2blog-login()
  "Logs into the blog. Initializes the internal data structures."
  (interactive)
  (let ((password))
    (setq org2blog-server-xmlrpc-url (or org2blog-server-url
					 (read-no-blanks-input 
					  "Weblog XML-RPC URL ? ")))
    (setq org2blog-server-userid (or org2blog-server-user
				     (read-no-blanks-input 
				      "Weblog User ID ? ")))
    (setq org2blog-server-blogid (or org2blog-server-weblog-id
				     (read-no-blanks-input "Weblog ID ? ")))
    (setq org2blog-categories-list
	  (mapcar (lambda (category) (cdr (assoc "categoryName" category)))
		  (metaweblog-get-categories org2blog-server-xmlrpc-url
					     org2blog-server-userid
					     (or org2blog-server-pass
						 (setq password (read-passwd "Weblog Password ? ")))
					     org2blog-server-weblog-id)))
    (setq org2blog-tags-list
	  (mapcar (lambda (tag) (cdr (assoc "slug" tag)))
		  (wp-get-tags org2blog-server-xmlrpc-url
			       org2blog-server-userid
			       (or org2blog-server-pass
				   password)
			       org2blog-server-weblog-id)))
    (setq org2blog-logged-in t)))

(defun org2blog-logout()
  "Logs out from the blog and clears. Clears the internal data structures."
  (interactive)
  (setq org2blog-server-xmlrpc-url nil
	org2blog-server-userid nil
	org2blog-server-blogid nil
	org2blog-categories-list nil
	org2blog-logged-in nil))

(defun org2blog-new-entry()
  "Creates a new blog entry. Use DESCRIPTION option for categories and KEYWORDS for tags."
  (interactive)
  (unless org2blog-logged-in
    (error "Please log-in to the blog first"))
  (let ((org2blog-buffer (generate-new-buffer org2blog-buffer-name)))
    (switch-to-buffer org2blog-buffer)
    (add-hook 'kill-buffer-hook 'org2blog-kill-buffer-hook nil 'local)
    (org-mode)
    (insert "#+OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil\n")
    (insert "#+DESCRIPTION: \n")
    (insert "#+KEYWORDS: \n")
    (insert "#+TITLE: <Enter Title Here>")
    (newline)
    (use-local-map org2blog-entry-mode-map)))

(defun upload-images-insert-links ()
  "Uploads images if any in the html, and changes their links"
  (let ((re 
	 (concat "\\[\\[\\(/.*\\)" 
		 (substring (org-image-file-name-regexp) 0 -2)
		 "\\]\\]"))
	(file-all-urls nil)
	file-name file-web-url blog-pass)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re  nil t 1)
	(setq file-name (concat 
			 (match-string-no-properties 1)
			 "."
			 (match-string-no-properties 2)))
	(setq file-web-url
	      (cdr (assoc "url" 
			  (metaweblog-upload-image org2blog-server-xmlrpc-url
						   org2blog-server-userid
						   (or org2blog-server-pass
						       blog-pass
						       (setq blog-pass (read-passwd "Img Upload - blog password ? ")))
						   org2blog-server-weblog-id
						   (get-image-properties file-name)))))
	(setq file-all-urls (append file-all-urls (list (cons 
							 file-name file-web-url)))))
      (goto-char (point-min))
      (dolist (image file-all-urls)
	(replace-string (car image) (cdr image))))))

(defun org2blog-post-entry(&optional publish)
  "Posts blog entry to the blog."
  (interactive "P")
  (unless org2blog-logged-in (error "Please log-in to the blog first"))
  (let* (html-text post-title categories post-id post-buffer)
    (setq post-buffer (buffer-name))
    (save-excursion 
      (goto-char (point-min))
      (if (re-search-forward "^#\\+POSTID: \\(.*\\)" nil t 1)
	  (setq post-id (match-string-no-properties 1))))
    (setq post-title (plist-get (org-infile-export-plist) :title))
    (setq tags (split-string 
		      (plist-get (org-infile-export-plist) :keywords) ", " t))
    (setq categories (split-string 
		      (plist-get (org-infile-export-plist) :description) ", " t))
    (upload-images-insert-links)
    (setq html-text (org-export-as-html 2 nil nil 'string t nil))
    (setq html-text (replace-regexp-in-string "\\\n" "" html-text))
    (if post-id
	(metaweblog-edit-post org2blog-server-xmlrpc-url
			      org2blog-server-userid
			      (or org2blog-server-pass
				  (read-passwd "Weblog Password ? "))
			      post-id
			      `(("description" . ,html-text)
				("title" . ,post-title)
				("categories" . ,categories)
				("tags" . ,tags))
			      publish)
      (setq post-id (metaweblog-new-post org2blog-server-xmlrpc-url
					 org2blog-server-userid
					 (or org2blog-server-pass
					     (read-passwd "Weblog Password ? "))
					 org2blog-server-blogid
					 `(("description" . ,html-text)
					   ("title" . ,post-title)
					   ("categories" . ,categories)
					   ("tags" . ,tags))
					 publish))
      (switch-to-buffer post-buffer)
      (goto-char (point-min))
      (insert (concat "#+POSTID: " post-id "\n")))))

(defun org2blog-complete-category()
  "Provides completion for categories and tags. DESCRIPTION for categories and KEYWORDS for tags."
  (interactive)
  (let* (current-pos tag-or-category-list)
    (setq current-pos (point))
    (forward-line 0)
    (forward-char 2)
    (if (or (looking-at "DESCRIPTION: ") (looking-at "KEYWORDS: "))
      	(progn 
	  (if (looking-at "KEYWORDS: ")
	      (setq tag-or-cat-list org2blog-tags-list)
	    (setq tag-or-cat-list org2blog-categories-list))
	  (goto-char current-pos)
      	  (let ((word-match (or (current-word t) ""))
      		(completion-match nil))
      	    (when word-match
      	      (setq completion-match (completing-read "Category ? " tag-or-cat-list nil nil word-match))
      	      (when (stringp completion-match)
      		(search-backward word-match nil t)
      	      (replace-match (concat completion-match ", ") nil t)))))
      (progn
      	(goto-char current-pos)
      	(command-execute (lookup-key org-mode-map (kbd "C-c t")))))))

(provide 'org2blog)