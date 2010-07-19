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

(defcustom org2blog-use-tags-as-categories nil
  "Non-nil means assign :tags: to Wordpress categories instead."
  :group 'org2blog
  :type 'boolean)

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

(defvar org2blog-mode nil
  "Mode for org2blog")
(make-variable-buffer-local 'org2blog-mode)

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

(defun org2blog-mode (&optional arg)
  "org2blog mode for providing mode-map."
  (interactive "P")
  (setq org2blog-mode
	(if (null arg)
	    (not org2blog-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if org2blog-mode
      (use-local-map org2blog-entry-mode-map)))

(unless org2blog-entry-mode-map
  (setq org2blog-entry-mode-map
	(let ((org2blog-map (make-sparse-keymap)))
	  (set-keymap-parent org2blog-map org-mode-map)
	  (define-key org2blog-map (kbd "C-c p") (lambda() (interactive) (org2blog-post-as-entry t)))
	  (define-key org2blog-map (kbd "C-c P") (lambda() (interactive) (org2blog-post-as-page t)))
	  (define-key org2blog-map (kbd "C-c d") 'org2blog-post-as-entry)
	  (define-key org2blog-map (kbd "C-c D") 'org2blog-post-as-page)
	  (define-key org2blog-map (kbd "C-c t") 'org2blog-complete-category)
	  org2blog-map)))

(defun org2blog-create-categories (categories)
  "Create unknown CATEGORIES."
  (mapcar
   (lambda (cat)
     (if (and (not (member cat org2blog-categories-list))
              (y-or-n-p (format "Create %s category? " cat)))
         (wp-new-category org2blog-server-xmlrpc-url
                          org2blog-server-userid
                          (org2blog-password)
                          org2blog-server-blogid
                          cat)))
   categories))

(defun org2blog-password ()
  "Get password or prompt if needed."
  (or org2blog-server-pass
      (setq org2blog-server-pass (read-passwd "Weblog password? "))))

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
                                             (org2blog-password)
					     org2blog-server-weblog-id)))
    (setq org2blog-tags-list
	  (mapcar (lambda (tag) (cdr (assoc "slug" tag)))
		  (wp-get-tags org2blog-server-xmlrpc-url
			       org2blog-server-userid
                               (org2blog-password)
			       org2blog-server-weblog-id)))
    (setq org2blog-logged-in t)
    (message "Logged in")))

(defun org2blog-logout()
  "Logs out from the blog and clears. Clears the internal data structures."
  (interactive)
  (setq org2blog-server-xmlrpc-url nil
	org2blog-server-userid nil
	org2blog-server-blogid nil
	org2blog-categories-list nil
	org2blog-logged-in nil)
  (message "Logged out"))

(defun org2blog-new-entry()
  "Creates a new blog entry. Use DESCRIPTION option for categories and KEYWORDS for tags."
  (interactive)
  (unless org2blog-logged-in
    (org2blog-login))
  (let ((org2blog-buffer (generate-new-buffer org2blog-buffer-name)))
    (switch-to-buffer org2blog-buffer)
    (add-hook 'kill-buffer-hook 'org2blog-kill-buffer-hook nil 'local)
    (org-mode)
    (insert "#+DATE: ")
    (insert (format-time-string "[%Y-%m-%d %a %H:%M]\n" (current-time)))
    (insert "#+OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:{}\n")
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
						       (setq blog-pass (read-passwd 
									(format 
									 "%s image upload - blog password ? " 
									 file-name))))
						   org2blog-server-weblog-id
						   (get-image-properties file-name)))))
	(setq file-all-urls (append file-all-urls (list (cons 
							 file-name file-web-url)))))
      (goto-char (point-min))
      (dolist (image file-all-urls)
	(replace-string (car image) (cdr image))))))

(defun org2blog-get-post-id ()
  "Gets the post-id from a buffer."
  (let (post-id)
    (save-excursion 
      (goto-char (point-min))
      (if (re-search-forward "^#\\+POSTID: \\(.*\\)" nil t 1)
          (setq post-id (match-string-no-properties 1))))
    post-id))

(defun org2blog-parse-entry(&optional publish)
  "Parse an org2blog buffer."
  (interactive "P")
  (let* (html-text post-title post-id post-buffer post-date tags categories)
    (setq post-buffer (buffer-name))
    (setq post-id (org2blog-get-post-id))
    (setq post-title (plist-get (org-infile-export-plist) :title))
    (setq post-date (plist-get (org-infile-export-plist) :date))
    (setq tags (plist-get (org-infile-export-plist) :keywords))
    (setq categories (plist-get (org-infile-export-plist) :description))
    (if post-date
	(progn
	  (setq post-date (replace-regexp-in-string "[-[:alpha:][:space:]]" "" 
						    post-date))
	  (setq post-date (concat (substring post-date 1 9) 
				  "T"  
				  (substring post-date 9 14) 
				  ":00"))))
    
    (if tags
	(if (setq tags (split-string tags "[ ,]+" t))
	    ()
	  (setq tags ""))
      (setq tags ""))

    (if categories
	(if (setq categories (split-string categories "[ ,]+" t))
            (org2blog-create-categories org2blog-categories-list)
	  (setq categories ""))
      (setq categories ""))

    (upload-images-insert-links)
    (setq html-text (org-export-as-html nil nil nil 'string t nil))
    (setq html-text 
	  (save-excursion
	    (with-temp-buffer
	      (let* (start-pos end-pos)
	  	(insert html-text)
	  	(setq start-pos (point-min))
		(goto-char start-pos)
                (while (re-search-forward "<\\(pre\\|blockquote\\).*?>" nil t 1)
                  (setq end-pos (match-beginning 0))
                  (replace-regexp "\\\n" " " nil start-pos end-pos)
                  (re-search-forward (concat "</" (match-string-no-properties 1) ">") nil t 1)
                  (setq start-pos (match-end 0))
                  (goto-char start-pos))
	  	(setq end-pos (point-max))
	  	(replace-regexp "\\\n" " " nil start-pos end-pos)
	      (buffer-substring-no-properties (point-min) (point-max))))))
    (list html-text post-title post-id post-buffer post-date tags categories)))

(defun org2blog-post-as-entry(&optional publish)
  "Posts new blog entry to the blog or edits an existing entry."
  (interactive "P")
  (unless org2blog-logged-in 
    (org2blog-login))
  (let (post html-text post-title post-id post-buffer post-date tags categories)
    (setq post (org2blog-parse-entry))
    (setq html-text (nth 0 post)
          post-title (nth 1 post)
          post-id (nth 2 post)
          post-buffer (nth 3 post)
          post-date (nth 4 post)
          tags (nth 5 post)
          categories (nth 6 post))
    (if post-id
	(metaweblog-edit-post org2blog-server-xmlrpc-url
			      org2blog-server-userid
                              (org2blog-password)
			      post-id
			      `(("description" . ,html-text)
				("title" . ,post-title)
				("date" . ,post-date)
				("categories" . ,categories)
				("tags" . ,tags))
			      publish)
      (setq post-id (metaweblog-new-post org2blog-server-xmlrpc-url
					 org2blog-server-userid
                                         (org2blog-password)
					 org2blog-server-blogid
					 `(("description" . ,html-text)
					   ("title" . ,post-title)
					   ("date" . ,post-date)
					   ("categories" . ,categories)
					   ("tags" . ,tags))
					 publish))
      (switch-to-buffer post-buffer)
      (goto-char (point-min))
      (insert (concat "#+POSTID: " post-id "\n")))
      (if publish
	  (message "Post \" %s \" Published" post-title)
	(message "Post \" %s \" saved as Draft" post-title))))

(defun org2blog-post-as-page(&optional publish)
  "Posts new page to the blog or edits an existing page."
  (interactive "P")
  (unless org2blog-logged-in 
    (org2blog-login))
  (let (post html-text post-title post-id post-buffer post-date tags categories)
    (setq post (org2blog-parse-entry))
    (setq html-text (nth 0 post)
          post-title (nth 1 post)
          post-id (nth 2 post)
          post-buffer (nth 3 post)
          post-date (nth 4 post)
          categories (nth 5 post)
          tags (nth 6 post))
    (if post-id
	(metaweblog-edit-post org2blog-server-xmlrpc-url
			      org2blog-server-userid
                              (org2blog-password)
			      post-id
			      `(("description" . ,html-text)
				("title" . ,post-title)
				("date" . ,post-date)
				("categories" . ,categories)
				("tags" . ,tags))
			      publish)
      (setq post-id (wp-new-page org2blog-server-xmlrpc-url
                                 org2blog-server-userid
                                 (org2blog-password)
                                 org2blog-server-blogid
                                 `(("description" . ,html-text)
                                   ("title" . ,post-title)
                                   ("date" . ,post-date)
                                   ("categories" . ,categories)
                                   ("tags" . ,tags))
                                 publish))
      (switch-to-buffer post-buffer)
      (goto-char (point-min))
      (insert (concat "#+POSTID: " post-id "\n")))
      (if publish
	  (message "Post \" %s \" Published" post-title)
	(message "Post \" %s \" saved as Draft" post-title))))

(defun org2blog-delete-entry (&optional post-id)
  (interactive "P")
  (if (null post-id)
      (setq post-id (org2blog-get-post-id)))
  (metaweblog-delete-post org2blog-server-xmlrpc-url
                                org2blog-server-userid
                                (org2blog-password)
                                post-id)
  (message "Post Deleted"))

(defun org2blog-delete-page (&optional page-id)
  (interactive "P")
  (if (null page-id)
      (setq page-id (org2blog-get-post-id)))
  (wp-delete-page org2blog-server-xmlrpc-url
                  org2blog-server-blogid
                  org2blog-server-userid
                  (org2blog-password)
                  page-id)
   (message "Page Deleted"))

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


(defun org2blog-upload-images-insert-links (&optional beg end)
  "Upload images and replace with links in the region specified by BEG to END."
  (interactive "r")
  (let ((re 
	 (concat "\\[\\[\\(.*\\)" 
		 (substring (org-image-file-name-regexp) 0 -2)
		 "\\]\\]"))
	file-all-urls file-name file-web-url blog-pass)
    (save-excursion
      (save-restriction
        (narrow-to-region (or beg (point-min))
                          (or end (point-max)))
        (goto-char (point-min))
        (while (re-search-forward re nil t 1)
          (setq file-name (concat 
                           (match-string-no-properties 1)
                           "."
                           (match-string-no-properties 2)))
          (unless (save-match-data (string-match org-link-types-re file-name))
            (save-match-data 
              (if (assoc file-name file-all-urls)
                  (setq file-web-url (cdr (assoc file-name file-all-urls)))
                (setq file-web-url
                      (cdr (assoc "url" 
                                  (metaweblog-upload-image org2blog-server-xmlrpc-url
                                                           org2blog-server-userid
                                                           (org2blog-password)
                                                           org2blog-server-weblog-id
                                                           (get-image-properties file-name))))
                      file-all-urls (append file-all-urls (list (cons 
                                                                 file-name file-web-url))))))
            (replace-match (concat "[[" file-web-url "]]") t t nil 0)))))
    file-all-urls))

(defun org2blog-post-subtree (&optional publish)
  "Post the current entry as a draft. Publish if PUBLISH is non-nil."
  (interactive "P")
  (let ((post (org2blog-parse-subtree))
        post-id)
    (org2blog-create-categories (cdr (assoc "categories" post)))
    (setq post-id (cdr (assoc "post-id" post)))
    (save-excursion 
      (org2blog-upload-images-insert-links (org-back-to-heading) (org-end-of-subtree)))
    (if post-id
        (metaweblog-edit-post org2blog-server-xmlrpc-url
			      org2blog-server-userid
                              (org2blog-password)
			      post-id
                              post
			      publish)
      (setq post-id
            (metaweblog-new-post
             org2blog-server-xmlrpc-url
             org2blog-server-userid
             (org2blog-password)
             org2blog-server-blogid
             post
             publish))
      (org-entry-put (point) "Post ID" post-id)
      (message (if publish
                   "Published (%s): %s"
                 "Draft (%s): %s")
               post-id
               (cdr (assoc "title" post))))))

(defun org2blog-parse-subtree ()
  "Parse the current subtree as a blog entry."
  (let (html-text
        (post-title (or (org-entry-get (point) "Title")
                        (org-get-heading t)))
        (post-id (org-entry-get (point) "Post ID"))
        ;; Set post-date to the Post Date property or look for timestamp
        (post-date (or (org-entry-get (point) "Post Date")
                       (org-entry-get (point) "SCHEDULED")
                       (org-entry-get (point) "DEADLINE")                       
                       (org-entry-get (point) "TIMESTAMP_IA")
                       (org-entry-get (point) "TIMESTAMP")))
        (tags (org-get-tags-at (point) nil))
        (categories (org-split-string (or (org-entry-get (point) "CATEGORIES") "") ":")))
    ;; Convert post date to ISO timestamp
    (setq post-date
          (format-time-string "%Y%m%dT%T"
                              (if post-date
                                  (apply 'encode-time (org-parse-time-string post-date))
                                (current-time))
                              t))
    (if org2blog-use-tags-as-categories
        (setq categories tags
              tags nil))
    (save-excursion
      (setq html-text
            (org-export-region-as-html
             (and (org-back-to-heading) (line-end-position))
             (org-end-of-subtree)
             t 'string))
      (setq html-text
            (with-temp-buffer
              (insert html-text)
              (goto-char (point-min))
              ;; Fix newlines
	      (let (start-pos end-pos)
                (setq start-pos (point-min))
		(goto-char start-pos)
                (while (re-search-forward "<\\(pre\\|blockquote\\).*?>" nil t 1)
                  (setq end-pos (match-beginning 0))
                  (replace-string "\n" " " nil start-pos end-pos)
                  (re-search-forward (concat "</" (match-string-no-properties 1) ">") nil t 1)
                  (setq start-pos (match-end 0))
                  (goto-char start-pos))
	  	(setq end-pos (point-max))
	  	(replace-string "\n" " " nil start-pos end-pos))
              ;; Copy the text
              (buffer-substring-no-properties (point-min) (point-max)))))
    (list
     (cons "date" post-date)
     (cons "title" post-title)
     (cons "tags" tags)
     (cons "categories" categories)
     (cons "post-id" post-id)
     (cons "description" html-text))))

(provide 'org2blog)
