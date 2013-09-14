;; Tests for org2blog live here

;; FIXME: Define a separate load file, that sets up the environment to
;; run in batch mode.

(require 'ert)
(require 'org)
(require 'org2blog)

;; Utility functions

;;; Directory containing tests
(defconst o2b-test-dir
  (expand-file-name (file-name-directory (or load-file-name buffer-file-name))))

;;; File with the test inputs and outputs
(defconst o2b-test-case-file
  (expand-file-name "tests.org" o2b-test-dir))


;;; Fetch the SRC block given the name
(defun o2b-test-fetch-src-block (name)
  (let ((src ""))
    (save-excursion
      (save-restriction
        (with-current-buffer (find-file-read-only o2b-test-case-file)
          (show-all)
          (goto-char (point-min))
          (unless (org-babel-goto-named-src-block name)
            (setq src (nth 1 (org-babel-get-src-block-info t))))
          (kill-buffer))))
    src))

(defmacro o2b-test-buffer-with-block (src-name &rest body)
  "Create a temporary buffer, insert the code with the given
SRC-NAME and evaluage BODY there."
  (declare (indent 0) (debug t))
  `(let ()
     (with-temp-buffer
       (org-mode)
       (insert (o2b-test-fetch-src-block ,src-name))
       (goto-char (point-min))
       (progn ,@body))))


;; Tests
(ert-deftest o2b-test-post-subtree ()
  "Test whether posting a subtree works correctly"

  ;; Test if the test subtree is parsed as expected
  (should (string-equal
           (o2b-test-fetch-src-block "o2b-test-post-subtree-output")
           (o2b-test-buffer-with-block
             "o2b-test-post-subtree-input"
             (cdr (assoc "description" (org2blog/wp-parse-entry t))))))

  ;; Test if demoting the subtree by one level still produces the same
  ;; output
  (should (string-equal
           ;; Expected output
           (o2b-test-fetch-src-block "o2b-test-post-subtree-output")
           ;; Output from parsing entry
           (o2b-test-buffer-with-block
             "o2b-test-post-subtree-input"
             (org-demote-subtree)
             (cdr (assoc "description" (org2blog/wp-parse-entry t)))))))

(ert-deftest o2b-test-parse-latex ()
  "Test if a LaTeX source block is parsed correctly, with and
  without Wordpress's plugin enabled."

  (should (string-equal
           (o2b-test-fetch-src-block "o2b-test-parse-latex-output-plain")
           (o2b-test-buffer-with-block
             "o2b-test-parse-latex-input"
             (let ((org2blog/wp-use-wp-latex nil))
               (cdr (assoc "description" (org2blog/wp-parse-entry t)))))))

  (should (string-equal
           (o2b-test-fetch-src-block "o2b-test-parse-latex-output-wp")
           (o2b-test-buffer-with-block
             "o2b-test-parse-latex-input"
             (let ((org2blog/wp-use-wp-latex t))
               (cdr (assoc "description" (org2blog/wp-parse-entry t))))))))

(ert-deftest o2b-test-post-with-toc ()
  "Test if posting with toc works."
  (should (string-equal
           (o2b-test-fetch-src-block "o2b-test-post-subtree-toc")
           (o2b-test-buffer-with-block
             "o2b-test-post-subtree-input"
             (let ((org2blog/wp-export-options (copy-sequence org2blog/wp-export-options)))
               (plist-put org2blog/wp-export-options :with-toc t)
               (cdr (assoc "description" (org2blog/wp-parse-entry t))))))))


(ert-deftest o2b-test-post-buffer-non-visible ()
  "Testing if posting buffer posts non visible content."
  (should (string-equal
           (o2b-test-fetch-src-block "o2b-test-post-buffer-non-visible")
           (o2b-test-buffer-with-block
             "o2b-test-post-buffer-non-visible-input"
             (let ()
               (org-shifttab 4)
               (goto-char (point-max))
               (cdr (assoc "description" (org2blog/wp-parse-entry nil))))))))

(ert-deftest o2b-test-post-buffer-hangs ()
  "Testing if posting a specific source hangs emacs."
  (should (string-equal
           (o2b-test-fetch-src-block "o2b-test-post-buffer-hangs")
           (o2b-test-buffer-with-block
             "o2b-test-post-buffer-hangs-input"
             (let ()
               (cdr (assoc "description" (org2blog/wp-parse-entry nil))))))))


(ert-deftest o2b-test-post-regexp-latex ()
  "Testing if a source block with \[ is treated as LaTeX."
  (should (string-equal
           (o2b-test-fetch-src-block "o2b-test-regexp-source-becomes-latex")
           (o2b-test-buffer-with-block
             "o2b-test-regexp-source-becomes-latex-input"
             (let ()
               (cdr (assoc "description" (org2blog/wp-parse-entry nil))))))))

(ert-deftest o2b-test-post-source-subtree ()
  "Testing if posting a subtree with a source block works."
  (should (string-equal
           (o2b-test-fetch-src-block "o2b-test-source-subtree-error")
           (o2b-test-buffer-with-block
             "o2b-test-source-subtree-error-input"
             (let ()
               (cdr (assoc "description" (org2blog/wp-parse-entry t))))))))
