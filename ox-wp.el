;;; ox-wp.el --- Org mode exporter for WordPress -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Puneeth Chaganti <punchagan@muse-amuse.in>
;; Copyright (C) 2019-2022 Grant Rettke <grant@wisdomandwonder.com>

;; Author: Puneeth Chaganti <punchagan+org2blog@gmail.com>
;; Maintainer: Grant Rettke <grant@wisdomandwonder.com>
;; Version: 1.1.16
;; Package-Requires: ((emacs "27.1"))
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

;; Read about how this exporter works here URL ‘https://orgmode.org/manual/Adding-export-back_002dends.html/’

;;;; Code:

;;; Require

(require 'ox-html)

;;; Constants

(defconst ox-wp-version "1.1.16"
  "Current version of ox-wp.el.")

;;; Functions

;;;###autoload
(defun ox-wp-export-as-wordpress (&optional async subtreep ext-plist)
  "Export current buffer to a text buffer by delegation.

Delegating: ASYNC, SUTREEP, and EXT-PLIST.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When `org-export-show-temporary-export-buffer' is non-nil
display a buffer with the export value."
  (interactive)
  (org-export-to-buffer 'wp "*Org WordPress Export*"
    async subtreep nil t ext-plist (lambda () (html-mode))))

(defun ox-wp-export-as-string (&optional async subtreep ext-plist)
  "Get exported buffer text as a string by delegation.

delegating: ASYNC, SUBTREEP, and EXT-PLIST.

Delegates work to `ox-wp-export-as-wordpress'."
  (interactive)
  (with-current-buffer (ox-wp-export-as-wordpress async subtreep ext-plist)
    (let ((text (buffer-string)))
      (kill-buffer)
      text)))

;; Back-End

(org-export-define-derived-backend 'wp 'html
  :translate-alist '((src-block . ox-wp-src-block)
                     (example-block . ox-wp-src-block)
                     (latex-environment . ox-wp-latex-environment)
                     (latex-fragment . ox-wp-latex-fragment)
                     (export-block . ox-wp-export-block)
                     (export-snippet . ox-wp-export-snippet))
  :filters-alist '((:filter-paragraph . ox-wp-filter-paragraph)))


;; Filters

(defun ox-wp-filter-paragraph (paragraph _backend info)
  "When INFO, filter newlines from PARAGRAPH."
  (let* ((keep-new-lines (plist-get info :wp-keep-new-lines))
         (result (if keep-new-lines paragraph
                   (format "%s\n\n"
                           (org-trim (replace-regexp-in-string "\s*\n" " "
                                                               paragraph))))))
    result))

(defun ox-wp-src-block (src-block contents info)
  "Delegate transcoding of SRC-BLOCK, CONTENTS, and INFO."
  (let* ((sc (plist-get info :wp-shortcode))
         (result (if sc
                     (ox-wp-src-block-shortcode src-block contents info)
                   (ox-wp-src-block-html src-block contents info))))
    result))

(defun ox-wp-src-block-shortcode (src-block _contents info)
  "Create the SyntaxHighlighter Evolved sourceblock with SRC-BLOCK, CONTENTS, and INFO."
  (let* ((langval (org-element-property :language src-block))
         (langs (plist-get info :wp-shortcode-langs-map))
         (lang (or (cdr (assoc langval langs))
                   (when langval (downcase langval))
                   "text"))
         (name (org-element-property :name src-block))
         (cap (and (org-export-get-caption src-block)
                   (org-trim (org-export-data
                              (org-export-get-caption src-block)
                              info))))
         (title (concat (when name (format "Name: %s." name))
                        (when cap (format "%s." (concat (when name " ") cap)))))
         (syntaxhl (or (org-export-read-attribute :attr_wp src-block :syntaxhl)
                       ""))
         (srccode (org-export-format-code-default src-block info))
         (result
          (format
           "[sourcecode language=\"%s\" title=\"%s\" %s]\n%s[/sourcecode]"
           lang
           title
           syntaxhl
           srccode)))
    result))

(defun ox-wp-src-block-html (src-block _contents info)
  "Create the HTML sourceblock with SRC-BLOCK, CONTENTS, and INFO."
  (catch 'return
    (when (org-export-read-attribute :attr_html src-block :textarea)
      (let ((result (org-html--textarea-block src-block)))
        (throw 'return result)))
    (let* ((name (org-element-property :name src-block))
           (caption (or (org-export-data
                         (org-export-get-caption src-block)
                         info)))
           (lang (org-element-property :language src-block))
           (code (org-html-format-code src-block info))
           (name-and-caption
            (concat (when name (format " Name: %s." name))
                    (unless (string-blank-p caption) (format " %s." caption)))))
      (unless lang
        (let ((result
               (format "<em>%s</em>\n<pre class=\"example\" id=\"%s\">\n%s</pre>"
                       name-and-caption name code)))
          (throw 'return result)))
      (let* ((classlabel
              (format "<label class=\"org-src-name\"><em>%s</em></label>"
                      name-and-caption))
             (body (format "<pre class=\"src src-%s\" id=\"%s\">%s</pre>"
                           lang name code))
             (div (format "<div class=\"org-src-container\">\n%s\n%s\n</div>"
                          classlabel
                          body))
             (result div))
        result))))

(defun ox-wp-latex-environment (latex-environment contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to WP HTML.

CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (not (plist-get info :wp-latex))
      (org-html-latex-environment latex-environment contents info)
    (let ((latex-env (org-element-property :value latex-environment)))
      (ox-wp-latex-to-wp latex-env))))

(defun ox-wp-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT element from Org to WP HTML.

CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (not (plist-get info :wp-latex))
      (org-html-latex-fragment latex-fragment contents info)
    (let ((latex-frag (org-element-property :value latex-fragment)))
      (ox-wp-latex-to-wp latex-frag))))

(defun ox-wp-latex-to-wp (text)
  "Convert latex fragments or environments in TEXT to WP LaTeX blocks."
  (let ((matchers (plist-get org-format-latex-options :matchers))
        (regular-expressions org-latex-regexps))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (nth 0 regular-expressions)
        (let* ((regular-expression (pop regular-expressions))
               (re-matcher (nth 0 regular-expression))
               (re-pattern (nth 1 regular-expression)))
          (when (member re-matcher matchers)
            (save-match-data
              (when (re-search-forward re-pattern nil t)
                (cond
                 ((equal re-matcher "$")
                  (replace-match (concat (match-string 1) "$latex "
                                         (match-string 4) "$"
                                         (match-string 6))
                                 nil t))
                 ((equal re-matcher "$1")
                  (replace-match (concat (match-string 1) "$latex "
                                         (substring (match-string 2) 1 -1)
                                         "$" (match-string 3))
                                 nil t))
                 ((equal re-matcher "\\(")
                  (replace-match (concat "$latex "
                                         (substring (match-string 0) 2 -2)
                                         "$") nil t))
                 ((equal re-matcher "\\[")
                  (replace-match (concat "<p style=\"text-align:center\"> $latex "
                                         (substring (match-string 0) 2 -2)
                                         "$ </p>") nil t))
                 ((equal re-matcher "$$")
                  (replace-match (concat "<p style=\"text-align:center\"> $latex "
                                         (substring (match-string 0) 2 -2)
                                         "$ </p>") nil t))
                 ((equal re-matcher "begin")
                  (cond ((equal (match-string 2) "equation")
                         (replace-match (concat "<p style=\"text-align:center\"> $latex "
                                                (substring (match-string 1) 16 -14)
                                                "$ </p>") nil t))))))))))
      (let ((result
             (replace-regexp-in-string "\s*\n" " " (buffer-string))))
        result))))

(defun ox-wp-export-snippet (export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object from Org to WordPress.
CONTENTS is nil.  INFO is a plist holding contextual
information.

`ox-wp' initially relied upon `ox-html' to satisfy this
function's responsibility. Consequently, it used the
`html' tag. When `ox-wp' implemented its export function,
it switched over to using the `wp' tag instead. However,
it needs to continue supporting both tags."
  (when (member (org-export-snippet-backend export-snippet) '(wp html))
    (org-element-property :value export-snippet)))

(defun ox-wp-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to WordPress.
CONTENTS is nil.  INFO is a plist holding contextual information.

`ox-wp' initially relied upon `ox-html' to satisfy this
function's responsibility. Consequently, it used the
`html' tag. When `ox-wp' implemented its export function,
it switched over to using the `wp' tag instead. However,
it needs to continue supporting both tags."
  (when (or (string= (org-element-property :type export-block) "WP")
            (string= (org-element-property :type export-block) "HTML"))
    (org-remove-indentation (org-element-property :value export-block))))

;;;; Underline

(defun org-html-underline (_underline contents _info)
  "Transcode UNDERLINE from Org to HTML.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information.

Underline implementation:

- WordPress friendly plain HTML
- W3C HTML and WHATWG HTML standard compliant HTML
- No CSS
- Zero Org configuration changes. For example ignore
  `org-html-htmlize-output-type' and
  `org-html-text-markup-alist'."
  (format "<u>%s</u>" contents))

(provide 'ox-wp)
;;; ox-wp.el ends here

