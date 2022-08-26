;;; org2blog-test-system.el --- Org2Blog WordPress System Testing  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022 Grant Rettke <grant@wisdomandwonder.com>

;; Author: Grant Rettke <grant@wisdomandonder.com>

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

;; Provides interactive system integration testing. Get your blog ready for
;; use and choose the test you want to run. At each step of the test you are
;; prompted whether to continue or not. Visually very the results of each step
;; in Messages and on your blog.

;;; Code:

;;;;
;;;; Setup
;;;;

(when nil ;; Default to running using package instead of source
  (add-to-list 'load-path "~/src/htmlize")
  (require 'htmlize)
  (add-to-list 'load-path "~/src/hydra")
  (require 'hydra)
  (add-to-list 'load-path "~/src/xml-rpc")
  (require 'xml-rpc)
  (add-to-list 'load-path "~/src/org2blog")
  (require 'org2blog))

(setq org2blog/wp-blog-alist
      `(("test"
         :url ,(getenv "O2BXMLRPC")
         :username ,(getenv "O2BUSER")
         :password ,(getenv "O2BPASSWORD")
         :default-title "Hello World"
         :default-categories ("org2blog"))
        ))

;;;;
;;;; Utility
;;;;

(defvar org2blog--buffer-content nil
  "Stores test buffer contents as a string.

Every test runs within the context of a buffer. There are two
scenarios. The first is creating a new buffer and populating it
with an entry. The second is performing server operations on an
existing entry already in a buffer possibly resulting in changes
to the buffer. This variable stores that content both before and
after the test.")

(defun org2blog--test-confirm-step (&optional msg)
  "Confirm progression of the test. Prompt with MSG if non-nil."
  (setq msg (or msg "Continue?"))
  (message "%s" msg)
  (unless (y-or-n-p msg)
    (throw 'return nil)))

;;;;
;;;; Create -> View -> Modify -> View -> Publish -> View -> Trash -> View
;;;;
;;;; Keep this one simple don't need macros here.
;;;;

;;; Buffer Post

(defun org2blog--test-buffer-post ()
  (interactive)
  (catch 'return
    (setq org2blog--buffer-content
          "
#+ORG2BLOG:
#+BLOG: test
#+DATE: [2019-10-09 Wed 19:12]
#+OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil
#+CATEGORY: Test
#+TAGS: Test
#+TITLE: TEST: Buffer Post

TEST: Buffer Post
")
    ;; Log In
    (org2blog--test-confirm-step "Log in?")
    (org2blog-user-login "test")
    ;; Create
    (org2blog--test-confirm-step "Create buffer post?")
    (let* ((body
            (with-temp-buffer
              (org-mode)
              (insert org2blog--buffer-content)
              (org2blog-buffer-post-save)
              (buffer-string))))
      (setq org2blog--buffer-content body))
    ;; View
    (org2blog--test-confirm-step "View it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-buffer-post-view))
    ;; Modify
    (setq org2blog--buffer-content
          (concat org2blog--buffer-content
                  "\nModified here."))
    (org2blog--test-confirm-step "Update it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-buffer-post-save))
    ;; View
    (org2blog--test-confirm-step "View it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-buffer-post-view))
    ;; Publish
    (org2blog--test-confirm-step "Publish it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-buffer-post-publish))
    ;; View
    (org2blog--test-confirm-step "View it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-buffer-post-view))
    ;; Trash
    (org2blog--test-confirm-step "Trash it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-buffer-post-trash))
    ;; View
    (org2blog--test-confirm-step "View it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-buffer-post-view))))

;;; Buffer Page

(defun org2blog--test-buffer-page ()
  (interactive)
  (catch 'return
    (setq org2blog--buffer-content
          "
#+ORG2BLOG:
#+BLOG: test
#+DATE: [2019-10-09 Wed 19:12]
#+OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil
#+CATEGORY: Test
#+TAGS: Test
#+TITLE: TEST: Buffer Page

TEST: Buffer Page
")
    ;; Log In
    (org2blog--test-confirm-step "Log in?")
    (org2blog-user-login "test")
    ;; Create
    (org2blog--test-confirm-step "Create buffer page?")
    (let* ((body
            (with-temp-buffer
              (org-mode)
              (insert org2blog--buffer-content)
              (org2blog-buffer-page-save)
              (buffer-string))))
      (setq org2blog--buffer-content body))
    ;; View
    (org2blog--test-confirm-step "View it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-buffer-page-view))
    ;; Modify
    (setq org2blog--buffer-content
          (concat org2blog--buffer-content
                  "\nModified here."))
    (org2blog--test-confirm-step "Update it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-buffer-page-save))
    ;; View
    (org2blog--test-confirm-step "View it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-buffer-page-view))
    ;; Publish
    (org2blog--test-confirm-step "Publish it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-buffer-page-publish))
    ;; View
    (org2blog--test-confirm-step "View it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-buffer-page-view))
    ;; Trash
    (org2blog--test-confirm-step "Trash it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-buffer-page-trash))
    ;; View
    (org2blog--test-confirm-step "View it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-buffer-page-view))))

;;; Subtree Post

(defun org2blog--test-subtree-post ()
  (interactive)
  (catch 'return
    (setq org2blog--buffer-content
          "
#+ORG2BLOG

* Subtree Post Test
:PROPERTIES:
:BLOG: test
:DATE: [2019-10-09 Wed 19:27]
:OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil
:CATEGORY: Test
:POST_TAGS: Test
:END:

TEST: Subtree Post
")
    ;; Log In
    (org2blog--test-confirm-step "Log in?")
    (org2blog-user-login "test")
    ;; Create
    (org2blog--test-confirm-step "Create subtree post?")
    (let* ((body
            (with-temp-buffer
              (org-mode)
              (insert org2blog--buffer-content)
              (org2blog-subtree-post-save)
              (buffer-string))))
      (setq org2blog--buffer-content body))
    ;; View
    (org2blog--test-confirm-step "View it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-subtree-post-view))
    ;; Modify
    (setq org2blog--buffer-content
          (concat org2blog--buffer-content
                  "\nModified here."))
    (org2blog--test-confirm-step "Update it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-subtree-post-save))
    ;; View
    (org2blog--test-confirm-step "View it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-subtree-post-view))
    ;; Publish
    (org2blog--test-confirm-step "Publish it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-subtree-post-publish))
    ;; View
    (org2blog--test-confirm-step "View it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-subtree-post-view))
    ;; Trash
    (org2blog--test-confirm-step "Trash it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-subtree-post-trash))
    ;; View
    (org2blog--test-confirm-step "View it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-subtree-post-view))))

;;; Subtree Page

(defun org2blog--test-subtree-page ()
  (interactive)
  (catch 'return
    (setq org2blog--buffer-content
          "
#+ORG2BLOG

* Subtree Page Test
:PROPERTIES:
:BLOG: test
:DATE: [2019-10-09 Wed 19:27]
:OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil
:CATEGORY: Test
:PAGE_TAGS: Test
:END:

TEST: Subtree Page
")
    ;; Log In
    (org2blog--test-confirm-step "Log in?")
    (org2blog-user-login "test")
    ;; Create
    (org2blog--test-confirm-step "Create subtree page?")
    (let* ((body
            (with-temp-buffer
              (org-mode)
              (insert org2blog--buffer-content)
              (org2blog-subtree-page-save)
              (buffer-string))))
      (setq org2blog--buffer-content body))
    ;; View
    (org2blog--test-confirm-step "View it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-subtree-page-view))
    ;; Modify
    (setq org2blog--buffer-content
          (concat org2blog--buffer-content
                  "\nModified here."))
    (org2blog--test-confirm-step "Update it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-subtree-page-save))
    ;; View
    (org2blog--test-confirm-step "View it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-subtree-page-view))
    ;; Publish
    (org2blog--test-confirm-step "Publish it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-subtree-page-publish))
    ;; View
    (org2blog--test-confirm-step "View it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-subtree-page-view))
    ;; Trash
    (org2blog--test-confirm-step "Trash it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-subtree-page-trash))
    ;; View
    (org2blog--test-confirm-step "View it?")
    (with-temp-buffer
      (org-mode)
      (insert org2blog--buffer-content)
      (org2blog-subtree-page-view))))
