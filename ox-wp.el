;;; ox-wp.el --- WordPress Back-End for Org Export Engine

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-html)



;;; User-Configurable Variables

(defgroup org-export-wp nil
  "Options specific to Wordpress export back-end."
  :tag "Org Wordpress"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))


;;; Define Back-End
(org-export-define-derived-backend 'wp 'html
  :translate-alist '((src-block . org-wp-src-block)
                     (example-block . org-wp-src-block)
                     (latex-environment . org-wp-latex-environment)
                     (latex-fragment . org-wp-latex-fragment))
  :filters-alist '(
                   (:filter-paragraph . org-wp-filter-paragraph)
                   ))



;;; Filters
(defun org-wp-filter-paragraph (paragraph backend info)
  "Function to filter out the new lines from PARAGRAPH unless
user explicitly configures otherwise."
  (let ((keep-new-lines (plist-get info :wp-keep-new-lines)))
    (if keep-new-lines paragraph
      (format "%s\n\n"
              (org-trim (replace-regexp-in-string "\s*\n" " " paragraph))))))

(defun org-wp-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to WP HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((lang (org-element-property :language src-block))
        (caption (org-export-get-caption src-block))
        ;; Set language to nil, to avoid htmlizing the code
        (raw-code (org-html-format-code (org-element-put-property src-block :language nil) info))
        (label (let ((lbl (org-element-property :name src-block)))
                 (if (not lbl) ""
                   (format " id=\"%s\""
                           (org-export-solidify-link-text lbl)))))
        (sc (plist-get info :wp-sourcecode-shortcode))
        (syntaxhl (org-element-property :syntaxhl src-block)))

    ;; Set back the language that we reset
    (org-element-put-property src-block :language lang)

    ;; FIXME: Change language name based on what wp-shortcode can handle.
    (if sc
        (format "[sourcecode language=\"%s\" title=\"%s\" %s]\n%s[/sourcecode]"
                (or lang "text") (or caption "") (or syntaxhl "") raw-code)
      (org-html-src-block src-block contents info))))

(defun org-wp-latex-environment (latex-environment contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to WP HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (not (plist-get info :wp-latex))
      (org-html-latex-environment latex-environment contents info)
    (let ((latex-env (org-element-property :value latex-environment)))
      (org-wp-latex-to-wp latex-env))))

(defun org-wp-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT element from Org to WP HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (not (plist-get info :wp-latex))
      (org-html-latex-fragment latex-fragment contents info)
    (let ((latex-frag (org-element-property :value latex-fragment)))
      (org-wp-latex-to-wp latex-frag))))

;; Misc helpers
(defun org-wp-latex-to-wp (text)
  "Helper to convert latex fragments or environments to WP LaTeX
blocks."
  (let* ((matchers (plist-get org-format-latex-options :matchers))
         (re-list org-latex-regexps)
         beg end re e m n block off)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (setq e (pop re-list))
        (setq m (car e) re (nth 1 e) n (nth 2 e)
              block (if (nth 3 e) "\n\n" ""))
        (when (member m matchers)
          (save-match-data
            (when (re-search-forward re nil t)
              (cond
               ((equal m "$")
                (replace-match (concat (match-string 1) "$latex "
                                       (match-string 4) "$"
                                       (match-string 6))
                               nil t))
               ((equal m "$1")
                (replace-match (concat (match-string 1) "$latex "
                                       (substring (match-string 2) 1 -1)
                                       "$" (match-string 3))
                               nil t))
               ((equal m "\\(")
                (replace-match (concat "$latex "
                                       (substring (match-string 0) 2 -2)
                                       "$") nil t))
               ((equal m "\\[")
                (replace-match (concat "<p style=\"text-align:center\"> $latex "
                                       (substring (match-string 0) 2 -2)
                                       "$ </p>") nil t))
               ((equal m "$$")
                (replace-match (concat "<p style=\"text-align:center\"> $latex "
                                       (substring (match-string 0) 2 -2)
                                       "$ </p>") nil t))
               ((equal m "begin")
                (if (equal (match-string 2) "equation")
                    (replace-match (concat "<p style=\"text-align:center\"> $latex "
                                           (substring (match-string 1) 16 -14)
                                           "$ </p>") nil t))))))))
      (replace-regexp-in-string "\s*\n" " " (buffer-string)))))


;;; Interactive function

;;;###autoload
(defun org-wp-export-as-wordpress (&optional async subtreep ext-plist)
  "Export current buffer to a text buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

Export is done in a buffer named \"*Org WP Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (if async
      (org-export-async-start
	  (lambda (output)
	    (with-current-buffer (get-buffer-create "*Org WP Export*")
	      (erase-buffer)
	      (insert output)
	      (goto-char (point-min))
	      (text-mode)
	      (org-export-add-to-stack (current-buffer) 'wp)))
	`(org-export-as 'wp ,subtreep t t ext-plist))
    (let ((outbuf (org-export-to-buffer
		   'wp "*Org WP Export*" subtreep t t ext-plist)))
      (with-current-buffer outbuf (text-mode))
      (when org-export-show-temporary-export-buffer
	(switch-to-buffer-other-window outbuf)))))


(defun org-wp-export-as-string (&optional async subtreep ext-plist)
  "Just calls the `org-wp-export-as-wordpress' function and
  returns the exported buffer text as a string"
  (interactive)
  (with-current-buffer (org-wp-export-as-wordpress async subtreep ext-plist)
    (let ((text (buffer-string)))
      (kill-buffer)
      text)))

(provide 'ox-wp)

;;; ox-wp.el ends here
