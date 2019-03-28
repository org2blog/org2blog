;;; org2blog.el --- Blog from Org mode to WordPress. -*- lexical-binding: t; -*-

;; Copyright (C) 2008 Ashish Shukla
;; Copyright (C) 2010 Benjamin Beckwith <bnbeckwith@gmail.com>
;; Copyright (C) 2010 Marcel van der Boom <marcel@hsdev.com>
;; Copyright (C) 2010-2014 Puneeth Chaganti <punchagan+org2blog@muse-amuse.in>
;; Copyright (C) 2010 Sacha Chua <sacha@sachachua.com>
;; Copyright (C) 2010 Giovanni Moretti <Giovanni@reflections.co.nz>
;; Copyright (C) 2010 Matt Price <matt@roke.mercey.dyndns.org>
;; Copyright (C) 2011 Mykola Nikishov <mn@mn.com.ua>
;; Copyright (C) 2013 Peter Vasil <mail@petervasil.net>
;; Copyright (C) 2015-2017, 2019 Grant Rettke <grant@wisdomandwonder.com>

;; Author: Puneeth Chaganti <punchagan+org2blog@gmail.com>
;; Maintainer: Grant Rettke <grant@wisdomandwonder.com>
;; Version: 1.1.0
;; Package-Requires: ((emacs "26.1") (dash "2.15.0") (dash-functional "2.15.0") (f "0.20.0") (helpful "0.15") (htmlize "1.55") (hydra "0.14.0") (metaweblog "1.0.1") (org "9.2.1") (s "1.12.0") (xml-rpc "1.6.12"))
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

;;; Code:



;;; Require

;; External

(require 'dash)
(require 'dash-functional)
(require 'f)
(require 'helpful)
(require 'ht)
(require 'htmlize)
(require 'hydra)
(require 'metaweblog)
(require 'org)
(require 's)
(require 'xml-rpc)

;; Internal

(require 'ox-wp)
(require 'org2blog-def)



;;; Constant

(defconst org2blog/wp-version (owp--pkg "version")
  "Current version of org2blog.el.")

(defconst org2blog/wp-required-org-version (cadr (assoc 'org (owp--pkg "requirements")))
  "Minimum variable ‘org-version’ required to run this package.")

(defconst owp--minimal-emacs (owp--pkg "emacs")
  "Minimum variable ‘emacs-version’ required to run this package.")

(defconst owp--default-blogid "1"
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
“1”.")

(defconst owp--deprecation "2.0.0"
  "Release in which obselete objects will be removed.")




;;; Deprecation

(-map (lambda (ls)
        (define-obsolete-function-alias (car ls) (cadr ls) owp--deprecation))
      '((org2blog/wp-buffer-kill-prompt owp-on-new-entry-kill)
        (org2blog/wp-complete-category owp-complete)
        (org2blog/wp-delete-entry owp-post-trash)
        (org2blog/wp-delete-page owp-page-trash)
        (org2blog/wp-format-buffer owp-entry-buffer-make)
        (org2blog/wp-insert-post-or-page-link owp-insert-link)
        (org2blog/wp-login owp-user-login)
        (org2blog/wp-logout owp-user-logout)
        (org2blog/wp-new-entry owp-buffer-new)
        (org2blog/wp-org-mode-hook-fn owp-maybe-start)
        (org2blog/wp-password owp-user-set-password)
        (org2blog/wp-post-buffer owp-buffer-post-save)
        (org2blog/wp-post-buffer-and-publish owp-buffer-post-publish)
        (org2blog/wp-post-buffer-as-page owp-buffer-page-save)
        (org2blog/wp-post-subtree owp-subtree-post-save)
        (org2blog/wp-post-subtree-and-publish owp-subtree-post-publish)
        (org2blog/wp-post-subtree-as-page owp-subtree-page-save)
        (org2blog/wp-post-subtree-as-page-and-publish owp-subtree-page-publish)
        (org2blog/wp-preview-buffer-post owp-buffer-post-or-page-view)
        (org2blog/wp-preview-subtree-post owp-subtree-post-or-page-view)
        (org2blog/wp-reload-entry-mode-map owp-reload-entry-mode-map)
        (org2blog/wp-track-buffer owp-buffer-track)
        (org2blog/wp-track-subtree owp-subtree-track)))

(-map (lambda (ls)
        (define-obsolete-variable-alias (car ls) (cadr ls) owp--deprecation))
      '((org2blog/wp-after-new-post-or-page-functions owp-buffer-entry-save-hook)
        (org2blog/wp-blog owp-blog)
        (org2blog/wp-blog-name owp-blog-key)
        (org2blog/wp-buffer-name owp-buffer-name)
        (org2blog/wp-categories-list owp-categories)
        (org2blog/wp-entry-mode-map owp-mode-map)
        (org2blog/wp-export-options owp-export-options)
        (org2blog/wp-logged-in owp-logged-in)
        (org2blog/wp-mode-hook owp-mode-hook)
        (org2blog/wp-pages-list owp-pages)
        (org2blog/wp-server-blogid owp-blogid)
        (org2blog/wp-server-pass owp-password)
        (org2blog/wp-server-userid owp-username)
        (org2blog/wp-server-xmlrpc-url owp-xmlrpc)
        (org2blog/wp-tags-list owp-tags)))



;;; Variable

(defvar owp-blog nil
  "Parameters of the currently selected blog.")

(defvar owp-blog-key nil
  "Name of the blog, to pick from `org2blog/wp-blog-alist'.")

(defvar owp-categories nil
  "List of weblog categories.")

(defvar owp-tags nil
  "List of weblog tags.")

(defvar owp-pages nil
  "List of WP pages.")

(defvar owp-xmlrpc nil
  "WordPress server XML-RPC URL.")

(defvar owp-username nil
  "WordPress server user id.")

(defvar owp-blogid nil
  "WordPress Blog ID.")

(defvar owp-mode-map nil
  "Keymap for blog entry buffer.")

(defvar owp-logged-in nil
  "Flag whether user is logged-in or not.")

(defvar owp-buffer-name "*Org2Blog (%s): %s*"
  "Name of the blog buffer.")

(defvar owp-mode-hook nil
  "Hook to run upon entry into mode.
Here is an example of creating keybindings:

(defun ahook ()
  (local-set-key (kbd \"M-9\") #'owp-user-interface)
  (local-set-key (kbd \"M-0\") #'owp-complete))
(add-hook 'org2blog/wp-mode-hook #'ahook).")

(defvar owp-buffer-entry-save-hook nil
  "Hooks run after a new post or page save.

Each function is called with one argument, the object
representing the aforementioned post or page.

Here is an example that outputs the entire object to the *Messages* buffer:

(defun ahook (entry)
  (pp entry))

(add-hook 'owp-buffer-entry-save-hook #'ahook).")

(defvar owp-export-options
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

(defvar owp-password nil)

(defvar owp-step-time 0.2 "Number of seconds to sleep between actions.

Must be greater than or equal to 0.2 seconds.")



;;; Group

(defgroup org2blog/wp nil
  "Blog from Org mode to WordPress"
  :group 'org2blog/wp)



;;; Custom

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

Most properties are optional, but some should always be set:

  :url                     xmlrpc url of the blog.
  :username                blog username

All the other properties are optional. They over-ride the global variables.

  :password                password to be used
  :default-title           `org2blog/wp-default-title'
  :default-categories      `org2blog/wp-default-categories'
                           Use a list of categories.
                           (\"category1\" \"category2\" ...)
  :default-title-sub       `org2blog/wp-default-title-subtree'
  :default-categories-sub  `org2blog/wp-default-categories-subtree'
                           Use a list of categories.
                           (\"category1\" \"category2\" ...)
  :tags-as-categories      `org2blog/wp-use-tags-as-categories'
  :confirm                 `org2blog/wp-confirm-post'
  :safe-trash              `org2blog/wp-safe-trash'
  :safe-new-entry-buf-kill `org2blog/wp-safe-new-entry-buffer-kill'
  :show                    `org2blog/wp-show-post-in-browser'
  :keep-new-lines          `org2blog/wp-keep-new-lines'
  :wp-latex                `org2blog/wp-use-wp-latex'
  :wp-code                 `org2blog/wp-use-sourcecode-shortcode'
  :track-posts             `org2blog/wp-track-posts'
                           Use a two item list.
                           (list \".org2blog.org\")
  :id                      ‘owp-blogid’"
  :group 'org2blog/wp
  :type '(alist :value-type plist))

(defcustom org2blog/wp-default-categories '("Uncategorized" "Hello")
  "Default list of categories for a new buffer entry."
  :group 'org2blog/wp
  :type '(repeat string))

(defcustom org2blog/wp-default-categories-subtree '("Uncategorized" "Hello")
  "Default list of categories for a new subtree entry."
  :group 'org2blog/wp
  :type '(repeat string))

(defcustom org2blog/wp-buffer-template
  "#+ORG2BLOG:
#+DATE: %s
#+OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil
#+CATEGORY: %s
#+TAGS:
#+DESCRIPTION:
#+TITLE: %s
\n"
  "The default template to be inserted in a new entry buffer.

It is passed to ‘format’ with 3 string arguments:
- Today’s date and time
- Your configuration of default categories
- Your configuration of default title."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-buffer-template-prefix nil
  "A prefix to the default template used for a new post buffer."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-buffer-subtree-template-prefix nil
  "A prefix to the default template used for a new subtree entry."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-buffer-format-function 'owp-entry-buffer-make
  "Function formatting a buffer according to `org2blog/wp-buffer-template'."
  :group 'org2blog/wp
  :type 'function)

(defcustom org2blog/wp-default-title "Hello, Buffer"
  "Title of a newly generated buffer entry."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-default-title-subtree "Hello, Subtree"
  "Title of a newly generated subtree entry."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-use-tags-as-categories nil
  "Non-nil means assign :tags: to Wordpress categories instead."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-confirm-post nil
  "Non-nil means confirm before Publishing a post or page."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-safe-trash t
  "Non-nil means confirm before Trashing a post or page."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-safe-new-entry-buffer-kill t
  "Non-nil means confirm before killing a new entry buffer."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-show-post-in-browser 'dont
  "How to automatically display an entry after save or publish.

Eventually you will have a lot of examples of how you prefer
to blog. This is your “personal workflow” and option should
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

If you want to configure this value per-blog then use the option :SHOW."
  :group 'org2blog/wp
  :type 'symbol)

(defcustom org2blog/wp-keep-new-lines nil
  "Non-nil means do not strip newlines.

When Org mode exports to HTML it removed line endings so
the web page “looks right”. If for some reason you don’t
what that typical behavior set this to program t."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-use-sourcecode-shortcode nil
  "Non-nil means convert <pre> tags to WP sourcecode blocks."
  :group 'org2blog/wp
  :type 'boolean)

(defcustom org2blog/wp-use-wp-latex t
  "Non-nil means convert LaTeX to WP LaTeX blocks."
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
be added."
  :group 'org2blog/wp
  :type '(list string string))

(defcustom org2blog/wp-keymap-prefix
  "C-c M-p"
  "Mode keymap prefix.

Call `owp-reload-entry-mode-map' after making
change for them to takes effect."
  :group 'org2blog/wp
  :type 'string)

(defcustom org2blog/wp-image-thumbnails nil
  "Non-nil means WordPress thumbnail links to full-size image."
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

function ‘owp-insert-link’ inserts an
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



;;; Hydra

(defun owp--hlpf (name)
  "Apply ‘helpful-callable' to symbol NAME."
  (helpful-callable name))

(defun owp--hlpv (name)
  "Apply ‘helpful-variable' to symbol NAME."
  (helpful-variable name))

(defhydra owp--hydra-main (:color blue :hint nil)
  "
╔═^═════════╗
║ ^Org2Blog ║^ (Main Menu) %(owp--login-status)^
╠═^═════════╩^═══════╦═════════════^═══^╦════════^════════════^╦═^═══════^═══════════^═╗
║ ^Admin^            ║ ^Use^            ║ ^Buffer^             ║ ^Subtree^           ^ ║
╚═^═════^════════════╩═^═══^════════════╩═^══════^═════════════╩═^═══════^═══════════^═╝
 [_4_] Login          [_e_] New Buffer   [_j_] Save Post Draft  [_u_] Save Post Draft
 [_3_] Reporting On   [_r_] New Subtree  [_k_] View Post        [_i_] View Post
 [_2_] Reporting Off  [_c_] Completion   [_l_] Publish Post     [_o_] Publish Post
 [_1_] Logout         [_f_] Trash ID     [_;_] Trash Post       [_p_] Trash Post
 [_a_] About          [_d_] “Insert A”   [_J_] Save Page Draft  [_U_] Save Page Draft
 [_w_] Version        [_v_] README       [_K_] View Page        [_I_] View Page
 [_h_] Help           [_V_] Variables    [_L_] Publish Page     [_O_] Publish Page
 [_q_] Quit            ^ ^               [_:_] Trash Page       [_P_] Trash Page
"
  ("4" owp-user-login :exit nil)
  ("3" owp-user-report-on :exit nil)
  ("2" owp-user-report-off :exit nil)
  ("1" owp-user-logout :exit nil)
  ("a" owp-about)
  ("w" owp-version-info)
  ("h" (owp--hydra-main-help/body))
  ("q" nil)

  ("e" owp-buffer-new)
  ("r" owp-subtree-new)
  ("c" owp-complete)
  ("f" owp-entry-trash-prompt)
  ("d" owp--hydra-main-inserts/body)
  ("v" owp-readme)
  ("V" owp--hydra-main-variables/body)

  ("j" owp-buffer-post-save)
  ("k" owp-buffer-post-view)
  ("l" owp-buffer-post-publish)
  (";" owp-buffer-post-trash)

  ("J" owp-buffer-page-save)
  ("K" owp-buffer-page-view)
  ("L" owp-buffer-page-publish)
  (":" owp-buffer-page-trash)

  ("u" owp-subtree-post-save)
  ("i" owp-subtree-post-view)
  ("o" owp-subtree-post-publish)
  ("p" owp-subtree-post-trash)

  ("U" owp-subtree-page-save)
  ("I" owp-subtree-page-view)
  ("O" owp-subtree-page-publish)
  ("P" owp-subtree-page-trash))

(defhydra owp--hydra-main-help (:color blue :hint nil)
  "
╔═^═════════╗
║ ^Org2Blog ║^ (Main Menu → Help) Select any item for more detail
╠═^═════════╩^═══════╦═════════════^═══^╦════════^════════════^╦═^═══════^═══════════^═╗
║ ^Admin^            ║ ^Use^            ║ ^Buffer^             ║ ^Subtree^           ^ ║
╚═^═════^════════════╩═^═══^════════════╩═^══════^═════════════╩═^═══════^═══════════^═╝
 [_4_] Login          [_e_] New Buffer   [_j_] Save Post Draft  [_u_] Save Post Draft
 [_3_] Reporting On   [_r_] New Subtree  [_k_] View Post        [_i_] View Post
 [_2_] Reporting Off  [_c_] Completion   [_l_] Publish Post     [_o_] Publish Post
 [_1_] Logout         [_f_] Trash ID     [_;_] Trash Post       [_p_] Trash Post
 [_a_] About          [_d_] “Insert A”   [_J_] Save Page Draft  [_U_] Save Page Draft
 [_w_] Version        [_v_] README       [_K_] View Page        [_I_] View Page
  ^ ^                 [_V_] Variables    [_L_] Publish Page     [_O_] Publish Page
 [_q_] Back            ^ ^               [_:_] Trash Page       [_P_] Trash Page
"
  ("4" (owp--hlpf 'owp-user-login) :exit nil)
  ("3" (owp--hlpf 'owp-user-report-on) :exit nil)
  ("2" (owp--hlpf 'owp-user-report-off) :exit nil)
  ("1" (owp--hlpf 'owp-user-logout) :exit nil)
  ("a" (owp--hlpf 'owp-about) :exit nil)
  ("w" (owp--hlpf 'owp-version-info) :exit nil)
  ("q" owp--hydra-main/body)

  ("e" (owp--hlpf 'owp-buffer-new) :exit nil)
  ("r" (owp--hlpf 'owp-subtree-new) :exit nil)
  ("c" (owp--hlpf 'owp-complete) :exit nil)
  ("f" (owp--hlpf 'owp-entry-trash-prompt) :exit nil)
  ("d" (owp--hlpf 'owp--main-inserts) :exit nil)
  ("v" (owp--hlpf 'owp-readme) :exit nil)
  ("V" (owp--hlpf 'owp--main-variables) :exit nil)

  ("j" (owp--hlpf 'owp-buffer-post-save) :exit nil)
  ("k" (owp--hlpf 'owp-buffer-post-view) :exit nil)
  ("l" (owp--hlpf 'owp-buffer-post-publish) :exit nil)
  (";" (owp--hlpf 'owp-buffer-post-trash) :exit nil)

  ("J" (owp--hlpf 'owp-buffer-page-save) :exit nil)
  ("K" (owp--hlpf 'owp-buffer-page-view) :exit nil)
  ("L" (owp--hlpf 'owp-buffer-page-publish) :exit nil)
  (":" (owp--hlpf 'owp-buffer-page-trash) :exit nil)

  ("u" (owp--hlpf 'owp-subtree-post-save) :exit nil)
  ("i" (owp--hlpf 'owp-subtree-post-view) :exit nil)
  ("o" (owp--hlpf 'owp-subtree-post-publish) :exit nil)
  ("p" (owp--hlpf 'owp-subtree-post-trash) :exit nil)

  ("U" (owp--hlpf 'owp-subtree-page-save) :exit nil)
  ("I" (owp--hlpf 'owp-subtree-page-view) :exit nil)
  ("O" (owp--hlpf 'owp-subtree-page-publish) :exit nil)
  ("P" (owp--hlpf 'owp-subtree-page-trash) :exit nil))

(defun owp--main-inserts ()
  "Open the “Insert A” menu."
  (owp--hydra-main-inserts/body))
(defhydra owp--hydra-main-inserts (:color blue :hint nil)
  "
╔══════════╗
║ Org2Blog ║ (Main Menu → Insert)
╚══════════╩═══════╦═════════════^════════^╗
                   ║ ^Insert A^            ║
                   ╚═^═══════════^═════════╝
                    [_m_] More Tag
                    [_t_] MathJax Shortcode
                    [_x_] “LaTeX” Name
                    [_r_] Link To Post
                    [_g_] Link To Page
                    [_o_] #+ORG2BLOG
[_h_] Help           ^ ^
[_q_] Back           ^ ^
"
  ("m" owp-insert-more)
  ("t" owp-insert-mathjax)
  ("x" owp-insert-latex)
  ("r" owp-insert-link-to-post)
  ("g" owp-insert-link-to-page)
  ("o" owp-org2blog-keyword-check)

  ("h" owp--hydra-main-inserts-help/body)
  ("q" owp--hydra-main/body))

(defhydra owp--hydra-main-inserts-help (:color blue :hint nil)
  "
╔══════════╗
║ Org2Blog ║ (Main Menu → Insert → Help) Select any item for more detail
╚══════════╩═══════╦═════════════^════════^╗
                   ║ ^Insert A^            ║
                   ╚═^═══════════^═════════╝
                    [_m_] More Tag
                    [_t_] MathJax Shortcode
                    [_x_] “LaTeX” Name
                    [_r_] Link To Post
                    [_g_] Link To Page
                    [_o_] #+ORG2BLOG
                     ^ ^
[_q_] Back           ^ ^
"
  ("m" (owp--hlpf 'owp-insert-more) :exit nil)
  ("t" (owp--hlpf 'owp-insert-mathjax) :exit nil)
  ("x" (owp--hlpf 'owp-insert-latex) :exit nil)
  ("r" (owp--hlpf 'owp-insert-link-to-post) :exit nil)
  ("g" (owp--hlpf 'owp-insert-link-to-page) :exit nil)
  ("o" (owp--hlpf 'owp-org2blog-keyword-check) :exit nil)
  ("q" owp--hydra-main-inserts/body))

(defun owp--main-variables ()
  "Open the Variables menu."
  (owp--hydra-main-variables/body))
(defhydra owp--hydra-main-variables (:color blue :columns 2)
  "
╔══════════╗
║ Org2Blog ║ (Main Menu → Variables) Select any item for more detail
╚══════════╩═════════════════════════════════════════════════════════╝
"
  ("aa" (owp--hlpv 'org2blog/link-selection-size)
   "org2blog/link-selection-size" :exit nil)
  ("ab" (owp--hlpv 'org2blog/wp-blog-alist)
   "org2blog/wp-blog-alist" :exit nil)
  ("ac" (owp--hlpv 'org2blog/wp-buffer-format-function)
   "org2blog/wp-buffer-format-function" :exit nil)
  ("ad" (owp--hlpv 'org2blog/wp-buffer-subtree-template-prefix)
   "org2blog/wp-buffer-subtree-template-prefix" :exit nil)
  ("ae" (owp--hlpv 'org2blog/wp-buffer-template)
   "org2blog/wp-buffer-template" :exit nil)
  ("af" (owp--hlpv 'org2blog/wp-buffer-template-prefix)
   "org2blog/wp-buffer-template-prefix" :exit nil)
  ("ag" (owp--hlpv 'org2blog/wp-confirm-post)
   "org2blog/wp-confirm-post" :exit nil)
  ("ah" (owp--hlpv 'org2blog/wp-default-categories)
   "org2blog/wp-default-categories" :exit nil)
  ("ai" (owp--hlpv 'org2blog/wp-default-categories-subtree)
   "org2blog/wp-default-categories-subtree" :exit nil)
  ("aj" (owp--hlpv 'org2blog/wp-default-title)
   "org2blog/wp-default-title" :exit nil)
  ("ak" (owp--hlpv 'org2blog/wp-default-title-subtree)
   "org2blog/wp-default-title-subtree" :exit nil)
  ("al" (owp--hlpv 'org2blog/wp-image-thumbnail-size)
   "org2blog/wp-image-thumbnail-size" :exit nil)
  ("am" (owp--hlpv 'org2blog/wp-image-thumbnails)
   "org2blog/wp-image-thumbnails" :exit nil)
  ("an" (owp--hlpv 'org2blog/wp-keep-new-lines)
   "org2blog/wp-keep-new-lines" :exit nil)
  ("ao" (owp--hlpv 'org2blog/wp-keymap-prefix)
   "org2blog/wp-keymap-prefix" :exit nil)
  ("ap" (owp--hlpv 'org2blog/wp-safe-new-entry-buffer-kill)
   "org2blog/wp-safe-new-entry-buffer-kill" :exit nil)
  ("aq" (owp--hlpv 'org2blog/wp-safe-trash)
   "org2blog/wp-safe-trash" :exit nil)
  ("ar" (owp--hlpv 'org2blog/wp-shortcode-langs-map)
   "org2blog/wp-shortcode-langs-map" :exit nil)
  ("as" (owp--hlpv 'org2blog/wp-show-post-in-browser)
   "org2blog/wp-show-post-in-browser" :exit nil)
  ("at" (owp--hlpv 'org2blog/wp-track-posts)
   "org2blog/wp-track-posts" :exit nil)
  ("au" (owp--hlpv 'org2blog/wp-use-sourcecode-shortcode)
   "org2blog/wp-use-sourcecode-shortcode" :exit nil)
  ("av" (owp--hlpv 'org2blog/wp-use-tags-as-categories)
   "org2blog/wp-use-tags-as-categories" :exit nil)
  ("aw" (owp--hlpv 'org2blog/wp-use-wp-latex)
   "org2blog/wp-use-wp-latex" :exit nil)

  ("q" owp--hydra-main/body "Back"))



;;; Function - Public

(defun owp-readme ()
  "Display project README.org.

Load the project's readme file into a buffer,
start Org mode, and make buffer readonly.

This is the real project readme displayed
on the project host site (GitHub at the moment)."
  (interactive)
  (catch 'return
    (condition-case-unless-debug err
        (let* ((match (find-function-noselect 'owp-readme t))
               (_ (unless (and (consp match) (cdr match))
                    (owp--error (concat "I’m sorry but I can’t show you the "
                                        "README using ‘owp-readme’."))
                    (throw 'return nil)))
               (srcbuf (car match))
               (srcfile (with-current-buffer srcbuf
                          (buffer-file-name)))
               (_ (unless (f-exists? srcfile)
                    (message
                     (concat "I’m sorry I ran into a problem trying "
                             "to find the readme file"
                             "for ‘owp-readme’. Please "
                             "report this as an error."))
                    (owp--error "Couldn’t open README")
                    (throw 'return nil)))
               (title "*Org2Blog README (COPY)*")
               (destbuf (get-buffer-create title))
               (readme (f-join (f-dirname srcfile) "README.org")))
          (switch-to-buffer destbuf)
          (condition-case-unless-debug err
              (let ((content (f-read readme)))
                (insert content))
            (error
             (owp--error
              (format (concat "I’m sorry I ran into a problem trying load "
                              "and the contents %s in ‘owp-readme’.")
                      readme)
              (format "%s" err))
             (throw 'return nil)))
          (goto-char (point-min))
          (org-mode)
          (message (concat "I just made a copy of README.org and inserted "
                           "it into this buffer. You can read it, and "
                           "even add notes and make changes as you go "
                           "along. When you are finished you can save it to "
                           "your own file or just kill the buffer.")))
      (error
       (owp--error
        (format (concat "I’m sorry I ran into a problem trying to display "
                        "the readme somewhere in ‘owp-readme’."))
        (format "%s" err))
       (throw 'return nil)))))

;;;###autoload
(defun owp-user-interface ()
  "Invoke the graphical user interface."
  (interactive)
  (owp--hydra-main/body))

;;;###autoload
(defun owp-on-new-entry-kill (kind)
  "Handler for a new KIND of entry buffer closing.

KIND must be either ’buffer or ’subtree.

Use like this:

  (add-hook 'kill-buffer-hook
             (apply-partially #'owp-on-new-entry-kill ’buffer)
                              nil 'local)
."
  (catch 'return
    (let* ((save-buffer? (and (owp--blog-property-or
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
        (owp-entry-track kind published?)))))

;;;###autoload
(defun owp-maybe-start ()
  "Enable `org2blog/wp-mode' when `#+ORG2BLOG:' is present.

Use it like this:

(add-hook 'org-mode-hook #'owp-maybe-start)"
  (with-current-buffer (current-buffer)
    (when (owp--bprop "ORG2BLOG")
      (org2blog/wp-mode t))))

;;;###autoload
(defun owp-user-report (on)
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
Call function ‘owp-version-info’ to display runtime version numbers

You usually only need to keep track of what is happening between
two of them because if it is doing what you expect then you
can move on.

Consider print messages where you need them and also using edebug.
With virtually no setup, Edebug lets you walk through a function
and evaluate local variable to see precisely what is happening.

After studying the request body, messages, and control flow and
things still don't work then the best thing to do is to test the
call using another tool. Once you've got a copy of the xml request
body you can test it using cURL. By this point you'll have a
better sense of where things are happening, or not, and now
might be the time to move on to the transfer layer.

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
           (if on "Hold onto your seat 🎢!" "Enjoy the silence 🧘.")))

;;;###autoload
(defun owp-user-report-on ()
  "Enable ‘owp-user-report’ ’ing."
  (interactive)
  (owp-user-report t))

;;;###autoload
(defun owp-user-report-off ()
  "Disable ‘owp-user-report’ ’ing."
  (interactive)
  (owp-user-report nil))

;;;###autoload
(defun owp-version-info (&optional value)
  "Display critical library information or return as a VALUE if non-nil."
  (interactive)
  (let ((msg (format
              "Org2Blog Runtime: Org2Blog %s, Emacs %s, Org Mode %s, MetaWeblog %s, XML-RPC %s"
              org2blog/wp-version
              emacs-version
              org-version
              metaweblog-version
              xml-rpc-version)))
    (if value msg
      (message msg))))

;;;###autoload
(defun owp-user-set-password ()
  "Set password “in memory”.

This does not change your password on the blog.

This does not change your password in your configuration file.

It does change your pass in memory during this session.

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
      (setq owp-password new)
      (message (concat
                "I just set your password “on your computer in memory. "
                "I mean that, your password remains the same on "
                "your blog and I’ll use the new password you just told me "
                "from now on to do things there.  "
                "If you already changed your password on your blog, or will "
                "do it soon, then you would probably use this function.")))))

;;;###autoload
(defun owp-user-login (&optional blog-name)
  "Log in to BLOG-NAME if non-nil, otherwise choose from a list."
  (interactive)
  (catch 'return
    (when (not org2blog/wp-blog-alist)
      (message "%s" (concat "Sorry, I can’t find any blogs for you to "
                            "login to. Please add your blog to "
                            "‘org2blog/wp-blog-alist’ and try "
                            "logging in again."))
      (throw 'return nil))
    (setq owp-blog-key
          (or
           blog-name
           (and (equal (length org2blog/wp-blog-alist) 1)
                (car (car org2blog/wp-blog-alist)))
           (completing-read
            "What blog would you like to log in to? ([Tab] to see list): "
            (-map 'car org2blog/wp-blog-alist) nil t)))
    (unless (> (length owp-blog-key) 1)
      (message
       (concat "Sorry, I can’t log in to blogs with names less than 2 "
               "characters long! It is weird, but I just can’t! Please "
               "run me again and tell  me about a blog with a name at "
               "least 2 characters long. There are 3 ways to do it: "
               "tell me inside \"this call\", configure "
               "‘org2blog/wp-blog-alist’, or choose a different blog from "
               "the list you are presented."))
      (throw 'return nil))
    (setq owp-blog (assoc owp-blog-key org2blog/wp-blog-alist)
          owp-xmlrpc (owp-blog-get :url)
          owp-username (owp-blog-get :username)
          owp-blogid (or (owp-blog-get :id) owp--default-blogid)
          owp-password
          (or
           (owp-blog-get :password)
           (read-passwd
            (format (concat
                     "It looks like you still haven’t entered a password, "
                     "and I need that to log you in. "
                     "What is your password for ‘%s’ on ‘%s’? "
                     "(type C-g to quit)")
                    owp-username owp-blog-key))))
    (message "Loading categories…")
    (sit-for owp-step-time)
    (condition-case-unless-debug err
        (setq owp-categories (owp--load-categories))
      (error
       (owp--error
        (format (concat "I’m sorry I ran into a problem trying to load categories "
                        "inside of ‘owp-user-login’."))
        (format "%s" err))
       (throw 'return nil)))
    (message "Loading tags…")
    (sit-for owp-step-time)
    (condition-case-unless-debug err
        (setq owp-tags (owp--load-tags))
      (error
       (owp--error
        (format (concat "I’m sorry I ran into a problem trying to load tags "
                        "inside of ‘owp-user-login’."))
        (format "%s" err))
       (throw 'return nil)))
    (message "Loading page list…")
    (sit-for owp-step-time)
    (condition-case-unless-debug err
        (setq owp-pages (owp--load-pages 'summaries))
      (error
       (owp--error
        (format (concat "I’m sorry I ran into a problem trying to load page "
                        "summaries inside of ‘owp-user-login’."))
        (format "%s" err))
       (throw 'return nil)))
    (setq owp-logged-in t)
    (let ((cats (length owp-categories))
          (tags (length owp-tags))
          (pages (length owp-pages)))
      (message (concat
                "You are now logged in to your blog “%s. "
                "Current stats: %s categories, %s tags, and %s pages.")
               owp-blog-key cats tags pages))))

;;;###autoload
(defun owp-user-logout()
  "Log out of blog."
  (interactive)
  (setq owp-xmlrpc nil
        owp-username nil
        owp-blogid nil
        owp-password nil
        owp-categories nil
        owp-tags nil
        owp-pages nil
        owp-logged-in nil)
  (message (concat
            "You are now logged out of your blog “%s”. "
            "Hope you had fun blogging and have a great day!")
           owp-blog-key))

;;;###autoload
(defun owp--new (destination)
  "Create new entry buffer for DESTINATION.
Destination is either a symbol ‘buffer’ or a ‘subtree’."
  (catch 'return
    (unless (or (eq destination 'buffer) (eq destination 'subtree))
      (owp--error
       (format
        (concat "I’m sorry I ran into a problem "
                (format "inside of ‘owp--new’ with a destination ‘%s’."
                        destination))))
      (throw 'return nil))
    (when (and (not owp-logged-in)
               (y-or-n-p
                (concat
                 "It looks like you are not logged in right now. "
                 "Would you like to login before composing "
                 "this new entry?")))
      (owp-user-login))
    (let* ((buf-name (cond ((eq destination 'buffer) "Buf")
                           ((eq destination 'subtree) "Sub")))
           (buf (generate-new-buffer
                 (format owp-buffer-name buf-name owp-blog-key)))
           content)
      (switch-to-buffer buf)
      (add-hook 'kill-buffer-hook
                (apply-partially #'owp-on-new-entry-kill destination) nil 'local)
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
                    (format "#+ORG2BLOG

* %s
:PROPERTIES:
:BLOG: %s
:DATE: %s
:OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil
:CATEGORY: %s
:POST_TAGS: %s
:END:\n\n"
                            (owp--blog-property-or :default-title-sub org2blog/wp-default-title-subtree)
                            owp-blog-key
                            (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))
                            (mapconcat
                             (lambda (cat) cat)
                             (owp--blog-property-or :default-categories-sub org2blog/wp-default-categories-subtree)
                             ", ")
                            "")))))
      (insert content)
      (org2blog/wp-mode t))))

;;;###autoload
(defun owp-buffer-new ()
  "Create new post entry."
  (interactive)
  (owp--new 'buffer))

;;;###autoload
(defun owp-subtree-new ()
  "Create new subtree entry."
  (interactive)
  (owp--new 'subtree))

;;;###autoload
(defun owp-buffer-post-save (&optional publish)
  "Save new or existing post. Publish if PUBLISH is non-nil."
  (interactive "P")
  (owp-entry-save 'buffer 'post publish))

;;;###autoload
(defun owp-buffer-post-publish ()
  "Publish post."
  (interactive)
  (owp-buffer-post-save t))

;;;###autoload
(defun owp-buffer-page-save (&optional publish)
  "Save new page to the blog or edits an existing page. Publish if PUBLISH is non-nil. Do as subtree if SUBTREE-P is non-nil."
  (interactive "P")
  (owp-entry-save 'buffer 'page publish))

;;;###autoload
(defun owp-buffer-page-publish ()
  "Publish page."
  (interactive)
  (owp-buffer-page-save t))

;;;###autoload
(defun owp-subtree-post-save (&optional publish)
  "Save the current subtree entry as a draft. Publish if PUBLISH is non-nil."
  (interactive "P")
  (catch 'return
    (owp--in-subtree-check)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (org-id-get nil t "o2b")
        (owp-entry-save 'subtree 'post publish)
        (widen)
        (when (buffer-file-name)
          (save-buffer))))))

;;;###autoload
(defun owp-subtree-post-publish ()
  "Publish subtree post."
  (interactive)
  (owp-subtree-post-save t))

;;;###autoload
(defun owp-subtree-page-save (&optional publish)
  "Save new subtree page to the blog or edits an existing page. If PUBLISH is non-nil then save and publish it."
  (interactive "P")
  (catch 'return
    (owp--in-subtree-check)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (org-id-get nil t "o2b")
        (owp-entry-save 'subtree 'page publish)
        (widen)
        (when (buffer-file-name)
          (save-buffer))))))

;;;###autoload
(defun owp-subtree-page-publish ()
  "Publish page."
  (interactive)
  (owp-subtree-page-save t))

;;;###autoload
(defun owp-entry-save (source type &optional publish)
  "Save new or existing entry of TYPE from SOURCE. In non-nil PUBLISH, do. If non-nil SUBTREE-P, do."
  (interactive "P")
  (org2blog/wp-mode t)
  (owp--ensure-login)
  (save-excursion
    (save-restriction
      (catch 'return
        (widen)
        (let* ((from-buffer (eq source 'buffer))
               (from-subtree (eq source 'subtree))
               (to-post (eq type 'post))
               (to-page (eq type 'page))
               (thing (symbol-name type))
               (made-new-entry nil)
               (post (owp--export-as-post from-subtree))
               (confirm (and
                         (owp--blog-property-or :confirm org2blog/wp-confirm-post)
                         publish))
               (show (or (owp-blog-has :show)
                         org2blog/wp-show-post-in-browser))
               post-id)
          (owp--create-categories (cdr (assoc "categories" post)))
          (setq post-id (cdr (assoc "post-id" post)))
          (when confirm
            (when (not (y-or-n-p
                        (format "Would you like to publish your %s: “%s” (ID “%s”)?"
                                thing (cdr (assoc "title" post)) post-id)))
              (message
               (concat "Canceled publishing your %s: “%s” (ID “%s”).")
               thing (cdr (assoc "title" post))
               post-id)
              (throw 'return nil)))
          (condition-case-unless-debug err
              (cond ((and to-post post-id)
                     (metaweblog-edit-post owp-xmlrpc
                                           owp-username
                                           owp-password
                                           post-id
                                           post
                                           publish))
                    ((and to-post (not post-id))
                     (setq post-id (metaweblog-new-post
                                    owp-xmlrpc
                                    owp-username
                                    owp-password
                                    owp-blogid
                                    post
                                    publish))
                     (setq made-new-entry t))
                    ((and to-page post-id)
                     (wp-edit-page owp-xmlrpc
                                   owp-username
                                   owp-password
                                   owp-blogid
                                   post-id
                                   post
                                   publish))
                    ((and to-page (not post-id))
                     (setq post-id
                           (wp-new-page owp-xmlrpc
                                        owp-username
                                        owp-password
                                        owp-blogid
                                        post
                                        publish))
                     (setq made-new-entry t)))
            (error
             (owp--error
              (format (concat "I’m sorry I ran into a problem "
                              "on %s: “%s” (ID “%s”) "
                              "inside of ‘owp-entry-save’.")
                      thing (cdr (assoc "title" post)) post-id)
              (format "%s" err))
             (throw 'return nil)))
          (when made-new-entry
            (run-hook-with-args
             'owp-buffer-entry-save-hook
             (owp--get-post-or-page post-id))
            (when to-page
              (condition-case-unless-debug err
                  (setq owp-pages (owp--load-pages 'summaries))
                (error
                 (owp--error
                  (format (concat "I just saved your new page, "
                                  "but it won’t show up yet "
                                  "when you try to complete it. "
                                  "After you log out and back in "
                                  "though it will show up."))
                  (format "%s" err)))))
            (when from-buffer
              (goto-char (point-min))
              (when to-post (insert (concat "#+BLOG: " owp-blog-key "\n")))
              (insert (concat "#+POSTID: " post-id "\n")))
            (when from-subtree
              (when to-post (org-entry-put (point) "BLOG" owp-blog-key))
              (org-entry-put (point) "POSTID" post-id)))
          (owp--save-details post post-id publish from-subtree)
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
                  ((not owp-logged-in)
                   (message
                    (concat did
                            "It looks like you wanted to display your %s, but "
                            "I couldn’t because you are not logged in to your "
                            "blog. Please log in to your blog and try doing "
                            "this again.")
                    thing))
                  (show (message "%s" did)
                        (cond ((and from-buffer to-post)
                               (owp-buffer-post-or-page-view))
                              ((and from-buffer to-page)
                               (owp-buffer-post-or-page-view))
                              ((and from-subtree to-post)
                               (owp-subtree-post-or-page-view))
                              ((and from-subtree to-page)
                               (owp-subtree-post-or-page-view))))
                  ((and ask (y-or-n-p
                             (format
                              (concat did
                                      "Would you like to display "
                                      "your %s: “%s” (ID “%s”)? ")
                              thing (cdr (assoc "title" post)) post-id)))
                   (cond ((and from-buffer to-post)
                          (owp-buffer-post-or-page-view))
                         ((and from-buffer to-page)
                          (owp-buffer-post-or-page-view))
                         ((and from-subtree to-post)
                          (owp-subtree-post-or-page-view))
                         ((and from-subtree to-page)
                          (owp-subtree-post-or-page-view)))))))))))

;;;###autoload
(defun owp-entry-trash-prompt (id)
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
      (owp-entry-trash 'post id)
      (throw 'return nil))
    (when (y-or-n-p
           (format
            (concat "Is “%s” a ***PAGE*** that you want to trash? "
                    "(type C-g to quit)") id))
      (owp-entry-trash 'page id)
      (throw 'return nil))
    (message (concat
              "It looks like you don’t want to trash "
              "either a post or page "
              "so I won’t try to trash anything right now."))))

;;;###autoload
(defun owp-buffer-post-trash (&optional post-id)
  "Trash buffer post. If POST-ID is non-nil trash that."
  (interactive "P")
  (owp-entry-trash 'post post-id))

;;;###autoload
(defun owp-subtree-post-trash (&optional post-id)
  "Trash subtree post. If POST-ID is non-nil trash that."
  (interactive "P")
  (catch 'return
    (owp--in-subtree-check)
    (owp-entry-trash 'post post-id)))

;;;###autoload
(defun owp-buffer-page-trash (&optional page-id)
  "Trash page. If PAGE-ID is non-nil trash that."
  (interactive "P")
  (owp-entry-trash 'page page-id))

;;;###autoload
(defun owp-subtree-page-trash (&optional page-id)
  "Trash page. If PAGE-ID is non-nil trash that."
  (interactive "P")
  (catch 'return
    (owp--in-subtree-check)
    (owp-entry-trash 'page page-id)))

;;;###autoload
(defun owp-entry-trash (type &optional entry-id)
  "Trash entryof TYPE. If ENTRY-ID is non-nil trash that one."
  (interactive "P")
  (owp--ensure-login)
  (when (null entry-id)
    (setq entry-id (or (owp--bprop "POSTID")
                       (owp--bprop "POST_ID")
                       (progn (org-narrow-to-subtree)
                              (widen)
                              (or (owp--eprop "POSTID")
                                  (owp--eprop "POST_ID"))))))
  (catch 'return
    (let* ((safetrash (owp--blog-property-or :safe-trash org2blog/wp-safe-trash))
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
              (metaweblog-delete-post owp-xmlrpc
                                      owp-username
                                      owp-password
                                      entry-id))
            (when is-page
              (wp-delete-page owp-xmlrpc
                              owp-blogid
                              owp-username
                              owp-password
                              entry-id)))
        (error
         (owp--error
          (format (concat "I’m sorry I ran into a problem "
                          "trying to trash your %s "
                          "ID “%s” "
                          "inside of ‘owp-entry-trash’.")
                  type entry-id)
          (format "%s" err))
         (throw 'return nil)))
      (message "I just trashed your %s with ID: “%s”." thing entry-id))))

;;;###autoload
(defun owp-complete ()
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
             (setq ls owp-categories)
             (setq thing "Category"))
            (see-tag
             (setq ls owp-tags)
             (setq thing "Tag"))
            (see-parent
             (setq ls owp-pages)
             (setq thing "Parent"))
            (t (owp--error
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
(defun owp-insert-more ()
  "Insert WordPress “More” tag.
“More” tags only work in posts, not Pages."
  (interactive)
  (let* ((pre "@@html:<!--more")
         (post "-->@@")
         (msg (s-trim (read-string "“More” message? (hit return to leave blank): ")))
         (str (or (and (s-blank? msg) (s-concat pre post))
                  (s-concat pre " " msg post))))
    (insert str)))

(defun help/insert-log-entry ()
  "Produce and insert a compact datestamp with full year and maybe a log message."
  (interactive)
  (let* ((stamp (format-time-string "%Y%m%dT%H%M"))
         (msg (s-trim (read-string "Log message? (hit return to leave blank): ")))
         (str (or (and (s-blank? msg) stamp)
                  (s-concat stamp "-" (s-replace " " "-" msg)))))
    (insert str)))

;;;###autoload
(defun owp-insert-mathjax ()
  "Insert the WordPress ‘MathJax’ shortcode."
  (interactive)
  (insert "[mathjax]"))

;;;###autoload
(defun owp-insert-latex ()
  "Insert WordPress ‘LaTeX’ string."
  (interactive)
  (insert "$\\LaTeX$"))

;;;###autoload
(defun owp-buffer-track ()
  "Track buffer."
  (interactive)
  (owp-entry-track 'buffer))

;;;###autoload
(defun owp-subtree-track ()
  "Track subtree."
  (interactive)
  (catch 'return
    (owp--in-subtree-check)
    (owp-entry-track 'subtree)))

;;;###autoload
(defun owp-entry-track (source &optional published?)
  "Track entry from SOURCE. Was it already PUBLISHED?"
  (interactive)
  (let ((is-buffer (eq source 'buffer))
        (is-subtree (eq source 'subtree)))
    (save-excursion
      (save-restriction
        (when is-buffer
          (widen)
          (owp--save-details (owp--export-as-post) "" published? nil))
        (when is-subtree
          (org-narrow-to-subtree)
          (owp--save-details (owp--export-as-post t) "" published? t)
          (widen))))))

;;;###autoload
(defun owp-buffer-post-view ()
  "View buffer post."
  (interactive)
  (owp-buffer-post-or-page-view))

;;;###autoload
(defun owp-buffer-page-view ()
  "View buffer page."
  (interactive)
  (owp-buffer-post-or-page-view))

;;;###autoload
(defun owp-buffer-post-or-page-view ()
  "View buffer post or page."
  (interactive)
  (owp-source-post-view 'buffer))

;;;###autoload
(defun owp-subtree-post-view ()
  "View subtree post."
  (interactive)
  (owp-subtree-post-or-page-view))

;;;###autoload
(defun owp-subtree-page-view ()
  "View subtree page."
  (interactive)
  (owp-subtree-post-or-page-view))

;;;###autoload
(defun owp-subtree-post-or-page-view ()
  "View subtree post or page."
  (interactive)
  (catch 'return
    (owp--in-subtree-check)
    (owp-source-post-view 'subtree)))

;;;###autoload
(defun owp-source-post-view (source)
  "View post stored in SOURCE.

Source is either a ’post or ’subtree"
  (interactive)
  (let ((is-subtree (eq source 'subtree))
        (thing (symbol-name source)))
    (when is-subtree (org-narrow-to-subtree))
    (owp--ensure-login)
    (when is-subtree (widen))
    (let* ((entry-id (or (owp--bprop "POSTID")
                         (owp--bprop "POST_ID")
                         (owp--eprop "POSTID")
                         (owp--eprop "POST_ID")))
           (url owp-xmlrpc))
      (if (not entry-id)
          (message (concat "Sorry I can’t display this %s post because it "
                           "hasn’t been saved or published yet. Please do "
                           "either and try again.") thing)
        (setq url (substring url 0 -10))
        (setq url (concat url "?p=" entry-id "&preview=true"))
        (browse-url url)))))

;;;###autoload
(defun owp-insert-link-to-post ()
  "Insert link to post."
  (interactive)
  (owp-insert-link nil))

;;;###autoload
(defun owp-insert-link-to-page ()
  "Insert link to page."
  (interactive)
  (owp-insert-link t))

;;;###autoload
(defun owp-insert-link (is-page)
  "Choose and insert link to entry using IS-PAGE if non-nil.

When IS-PAGE is nil then chose from page IDs
instead of posts."
  (interactive "P")
  (owp--ensure-login)
  (message "Loading %s…" (if is-page "page list"
                           (format "last %s posts"
                                   org2blog/link-selection-size)))
  (sit-for owp-step-time)
  (catch 'return
    (let* ((post-list
            (condition-case-unless-debug err
                (if is-page
                    (owp--load-pages)
                  (metaweblog-get-recent-posts owp-xmlrpc
                                               owp-blogid
                                               owp-username
                                               owp-password
                                               org2blog/link-selection-size))
              (error
               (owp--error
                (format (concat "I’m sorry I ran into a problem "
                                "trying to insert a link "
                                "inside of ‘owp-insert-link’."))
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
                   (replace-regexp-in-string "xmlrpc\\.php$" "?p=" owp-xmlrpc)
                   entryid))
        (insert (format "[[%s][%s]]" url post-title))))))

;;;###autoload
(defun owp-reload-entry-mode-map ()
  "Re-initialize `owp-mode-map'.

Use the prefix key sequence defined by
`org2blog/wp-keymap-prefix' and update `minor-mode-map-alist'
accordingly."
  (interactive)
  (owp--init-entry-mode-map)
  (let ((keymap (assoc 'org2blog/wp-mode minor-mode-map-alist)))
    (setcdr keymap owp-mode-map)))

;;;###autoload
(defun owp-about ()
  "Display brief about page."
  (interactive)
  (switch-to-buffer "*Org2Blog About*")
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "╔═══════════════════════════════════════╗\n")
  (widget-insert "║ 🐃 → 🦄 → Org2Blog → WordPress → 🌐    ║\n")
  (widget-insert "╚═══════════════════════════════════════╝")
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
  (widget-insert "see the ‘LICENSE.TXT' that ")
  (widget-insert "came installed with this package).")
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
  (widget-insert "If you’re curious about some of the other Org2Bloggers out there\n")
  (widget-insert "then be sure to check out the links in the file ‘Org2Bloggers.org'\n")
  (widget-insert "that came with this package.")
  (widget-insert "\n\n")
  (widget-insert "To all and soon to be Org2Bloggers out there:")
  (widget-insert "\n\n")
  (widget-insert "Hope you have fun blogging and have a great day!")
  (widget-insert "\n\n")
  (widget-create
   'url-link
   :tag "Copyright 2019"
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
(defun owp-org2blog-keyword-check ()
  "Insert the ORG2BLOG keyword unless it exists.

Inserts ‘#+ORG2BLOG’ on the first empty lines that it finds.

If it doesn’t find one then it doesn’t insert it."
  (interactive)
  (catch 'return
    (when (owp--bprop "ORG2BLOG")
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



;;; Funtion - Private

(defun owp--load-categories ()
  "Load categories from server.
Caller must handle any errors."
  (let* ((raw (metaweblog-get-categories
               owp-xmlrpc
               owp-username
               owp-password
               owp-blogid))
         (cats (-map
                (lambda (category) (cdr (assoc "categoryName" category)))
                raw)))
    cats))

(defun owp--load-tags ()
  "Load tags from server.
Caller must handle any errors."
  (let* ((raw (wp-get-tags
               owp-xmlrpc
               owp-username
               owp-password
               owp-blogid))
         (tags (-map
                (lambda (tag) (cdr (assoc "slug" tag)))
                raw)))
    tags))

(defun owp--load-pages (&optional summaries)
  "Load raw pages from server or SUMMARIES if non-nil.
Caller must handle any errors."
  (let* ((pages (wp-get-pagelist
                 owp-xmlrpc
                 owp-username
                 owp-password
                 owp-blogid))
         (page-summaries
          (-map (lambda (pg)
                  (cons (cdr (assoc "page_title" pg))
                        (cdr (assoc "page_id" pg))))
                pages))
         (result (if summaries
                     page-summaries
                   pages)))
    result))

(defun owp--in-subtree-check ()
  "Generate error unless cursor is within a subtree."
  (unless (owp--in-subtree-p)
    (owp--error
     (concat
      "I’m sorry but your cursor is not in "
      "subtree so I can’t do that. Please "
      "move your cursor to a subtree and try "
      "that again."))
    (throw 'return nil)))

(defun owp--in-subtree-p ()
  "Non-nil if cursor is below a subtree."
  (interactive)
  (not (org-before-first-heading-p)))

(defun owp--bprop (name)
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

(defun owp--eprop (name)
  "Return entry property for NAME.
See ‘owp--bprop’ docstring for details."
  (org-entry-get (point) name))

(defun owp-blog-has (property)
  "Return non-nil if current blog PROPERTY exists."
  (plist-member (cdr owp-blog) property))

(defun owp-blog-get (property)
  "Return current blog PROPERTY."
  (plist-get (cdr owp-blog) property))

(defun owp--blog-property-or (property value)
  "Return current blog PROPERTY, else or VALUE."
  (if (owp-blog-has property)
      (owp-blog-get property)
    value))

(defun owp--login-status ()
  "User login status of current blog."
  (let ((msg (if (not owp-logged-in) "Logged Out."
               (format "Logged In To ‘%s’ as ‘%s’."
                       owp-blog-key owp-username))))
    msg))

(defun owp--define-key (map suffix function)
  "Helper to populate ‘owp-mode-map’ in MAP for FUNCTION with SUFFIX.

Define a key sequence SUFFIX in MAP for FUNCTION.

Uses the mode's key map with the prefix
`org2blog/wp-keymap-prefix', and the given suffix."
  (let ((keyseq (read-kbd-macro (concat org2blog/wp-keymap-prefix " " suffix))))
    (define-key map keyseq function)))

(defun owp--init-entry-mode-map ()
  "Initialize `owp-mode-map'.

Uses the prefix key sequence defined by
`org2blog/wp-keymap-prefix'.

Both sets the map and returns the map so that it can be used both
at mode start time and after the user re-configures it."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (owp--define-key map "p" 'owp-buffer-post-publish)
    (owp--define-key map "P" 'owp-buffer-page-publish)
    (owp--define-key map "d" 'owp-buffer-post-save)
    (owp--define-key map "D" 'owp-buffer-page-save)
    (owp--define-key map "t" 'owp-complete)
    (owp--define-key map "g" 'owp-user-interface)
    (setq owp-mode-map map)
    map))

(defun owp--create-categories (new-categories)
  "Add NEW-CATEGORIES to ‘owp-categories'."
  (let ((result
         (-map
          (lambda (cat)
            (if (and (not (-contains? owp-categories cat))
                     (y-or-n-p
                      (format
                       (concat "Would you like to "
                               "create the a new "
                               "category named: “%s”?")
                       cat)))
                (condition-case-unless-debug err
                    (wp-new-category
                     owp-xmlrpc
                     owp-username
                     owp-password
                     owp-blogid
                     cat)
                  (error
                   (owp--error
                    (format
                     (concat "I’m sorry I ran into a problem "
                             "trying to create categories "
                             "inside of ‘owp--create-categories’."))
                    (format "%s" err))
                   (throw 'return nil))))
            (add-to-list 'owp-categories cat))
          new-categories)))
    result))

(defun owp--entry-blog-name ()
  "Return the blog name for this entry (buffer or subtree)."
  (let ((blog-name
         (if (org-buffer-narrowed-p)
             (or (owp--eprop "BLOG") "")
           (or (owp--bprop "blog") ""))))
    (or (and (assoc blog-name org2blog/wp-blog-alist) blog-name) nil)))

(defun owp--ensure-login ()
  "Ensure user is logged in to current entry’s blog.

This function handles two scenarios:

Scenario #1

User logs into BLOG-A and starts blogging. At some point
User returns to an OLD-ENTRY to make changes. OLD-ENTRY belongs
to BLOG-B  Upon completing the changes User chooses to save the
entry. At this point User is logged into BLOG-A while attempting
to post to BLOG-B. This won’t work, there will be an authentication
failure.

This function handles this scenario by logging the U out of
BLOG-A and logging User into BLOG-B.

Scenario #2:

User is not logged in and attempts to save a post.

This function prompts the user to login."
  (let ((blog-name (owp--entry-blog-name)))
    (when (and blog-name (not (equal blog-name owp-blog-key)))
      (owp-user-logout))
    (unless owp-logged-in
      (owp-user-login blog-name))))

(defun owp-entry-buffer-make (buffer-template)
  "Create new entry buffer populated using BUFFER-TEMPLATE.

See ‘org2blog/wp-buffer-template’ for details about how it is used."
  (format buffer-template
          (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))
          (string-join
           (owp--blog-property-or :default-categories org2blog/wp-default-categories)
           ", ")
          (owp--blog-property-or :default-title org2blog/wp-default-title)))

(defun owp--upload-files-replace-urls (text)
  "Upload files and replace their links in TEXT."
  (catch 'return
    (let ((file-all-urls nil)
          (file-regexp "<a href=\"\\(.*?\\)\"\\|<img src=\"\\(.*?\\)\"")
          file-name file-web-url beg file-thumbnail-name upload-ret)
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
                                       (string-match org-plain-link-re file-name)
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
                                    owp-xmlrpc
                                    owp-username
                                    owp-password
                                    owp-blogid
                                    (get-file-properties file-name)))
                (error
                 (owp--error
                  (format (concat "I’m sorry I ran into a problem "
                                  "inside of ‘owp--upload-files-replace-urls’."))
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
                        (xml-rpc-method-call owp-xmlrpc
                                             "wp.getMediaItem"
                                             owp-blogid
                                             owp-username
                                             owp-password
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
              (owp--new-line-no-indent)
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

(defun owp--get-post-or-page (post-or-page-id)
  "Retrieve an entry for POST-OR-PAGE-ID.

For information about its fields see
URL`https://codex.wordpress.org/XML-RPC_MetaWeblog_API#metaWeblog.newPost'"
  (interactive)
  (catch 'return
    (condition-case-unless-debug err
        (let ((post-or-page (metaweblog-get-post owp-xmlrpc
                                                 owp-username
                                                 owp-password
                                                 post-or-page-id)))
          post-or-page)
      (error
       (owp--error
        (format (concat "I’m sorry I ran into a problem trying to retrieve "
                        "ID “%s” "
                        "inside of ‘owp--get-post-or-page’.")
                post-or-page-id)
        (format "%s" err))
       (throw 'return nil)))))

(defun owp--save-details (post pid pub subtree-p)
  "Save POST details of PID and PUB. If SUBTREE-P is non-nil, record that."
  (catch 'return
    (save-excursion
      (let ((the-file (if (owp-blog-has :track-posts)
                          (car (owp-blog-get :track-posts))
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
                            ((f-absolute? the-file) the-file)
                            (org-directory
                             (f-expand the-file org-directory))
                            (t
                             (owp--error
                              (format
                               (concat
                                "Sorry I had a problem creating your post "
                                "tracking file. The problem is that the "
                                "filename is ambiguous. The solution is to "
                                "either use an absolute path or to set "
                                "the variable ‘org-directory’, then try "
                                "tracking your entry again.")))
                             (throw 'return nil))))
                 (headline (if (owp-blog-has :track-posts)
                               (cadr (plist-get (cdr owp-blog) :track-posts))
                             (cadr org2blog/wp-track-posts)))
                 p)
            (make-directory (f-dirname log-file) t)
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
                  (owp--update-details post o2b-id pid pub))
                (save-buffer)))))))))

(defun owp--update-details (post o2b-id pid pub)
  "Store O2B-ID, PID, and PUB in POST."
  (insert (format "[[%s][%s]]"
                  o2b-id
                  (cdr (assoc "title" post))))
  (org-entry-put (point) "POSTID" (or pid ""))
  (org-entry-put (point) "POST_DATE" (cdr (assoc "date" post)))
  (org-entry-put (point) "PUBLISHED" (if pub "Yes" "No")))

(defun owp--new-line-no-indent ()
  "Insert a new line without indenting."
  (insert (if use-hard-newlines hard-newline "\n")))

(defun owp--collect-export-options ()
  "Return a plist of export options.

This can be passed on to the export functions to configure the
various export options."
  (let ((export-options owp-export-options))
    (plist-put export-options :wp-keep-new-lines
               (owp--blog-property-or :keep-new-lines org2blog/wp-keep-new-lines))
    (plist-put export-options :wp-latex
               (owp--blog-property-or :wp-latex org2blog/wp-use-wp-latex))
    (plist-put export-options :wp-shortcode
               (owp--blog-property-or :wp-code org2blog/wp-use-sourcecode-shortcode))
    (plist-put export-options :tags-as-categories
               (owp--blog-property-or :tags-as-categories org2blog/wp-use-tags-as-categories))
    (plist-put export-options :wp-shortcode-langs-map
               org2blog/wp-shortcode-langs-map)
    export-options))

(defun owp--convert-timestamp-to-iso (timestamp)
  "Convert org TIMESTAMP to ISO."
  (let ((result (format-time-string
                 "%Y%m%dT%T%z"
                 (apply 'encode-time (org-parse-time-string timestamp))
                 t)))
    result))

(defun owp--export-as-html (subtree-p export-options)
  "Create entry HTML given EXPORT-OPTIONS and SUBTREE-P."
  (let ((result
         (save-excursion
           (owp--upload-files-replace-urls
            (org-no-properties (org-wp-export-as-string nil subtree-p
                                                        export-options))))))
    result))

(defun owp--export-as-post (&optional subtree-p)
  "Parse a buffer entry. If SUBTREE-P is non nill then parse subtree."

  (let* ((export-options (owp--collect-export-options))
         (tags-as-categories (plist-get export-options :tags-as-categories)))
    (save-excursion
      (save-restriction
        (let ((post (if subtree-p
                        (owp--parse-subtree-entry)
                      (owp--parse-buffer-entry))))
          (when tags-as-categories
            (setcdr (assoc "categories" post) (cdr (assoc "tags" post)))
            (setcdr (assoc "tags" post) nil))

          (setcdr (assoc "date" post)
                  (owp--convert-timestamp-to-iso
                   (owp--insert-current-time subtree-p
                                             (cdr (assoc "date" post)))))
          (setcdr (assoc "description" post)
                  (owp--export-as-html subtree-p export-options))
          post)))))

(defun owp--bprop-parent-id (parent)
  "Return ID of PARENT.

If parent is the id of the parent page, the user need not be
logged in.  Otherwise, the user is prompted to login."

  (when (and parent (equal 0 (string-to-number parent)))
    (owp--ensure-login))
  (if parent
      (or
       (cdr (assoc
             (car (split-string parent "\\( *, *\\)" t))
             owp-pages))
       (number-to-string (string-to-number parent))
       "0")
    "0"))

(defun owp--insert-current-time (subtree-p time)
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

(defun owp--parse-buffer-entry ()
  "Parse an org2blog buffer entry.

The entry object returned does not contain the exported html.
This entry needs to be further processed by the
`owp--export-as-post' function, to add the export html
and munge it a little to make it suitable to use with the
`metaweblog' functions."
  (let*
      ((export-environment (org-export-with-buffer-copy (org-export-get-environment)))
       (parsed-entry
        (list
         (cons "point" (point))
         (cons "date" (owp--bprop "DATE"))
         (cons "title" (org-element-interpret-data
                        (or (plist-get export-environment :title)
                            "No Title")))
         (cons "description" nil)
         (cons "tags"
               (split-string (or (owp--bprop "TAGS") "")
                             "\\( *, *\\)" t))
         (cons "categories"
               (split-string (or (owp--bprop "CATEGORY") "")
                             "\\( *, *\\)" t))
         (cons "post-id" (owp--bprop "POSTID"))
         (cons "parent" (owp--bprop-parent-id
                         (owp--bprop "PARENT")))
         (cons "excerpt" (org-element-interpret-data
                          (or (plist-get export-environment
                                         :description) "")))
         (cons "permalink" (or (owp--bprop "PERMALINK") "")))))
    parsed-entry))

(defun owp--parse-subtree-entry ()
  "Parse an org2blog subtree entry.

The entry object returned does not contain the exported html.
This entry needs to be further processed by the
`owp--export-as-post' function, to add the export html
and munge it a little to make it suitable to use with the
`metaweblog' functions."
  (let*
      ((parsed-entry
        (list
         (cons "point" (point))
         (cons "date" (or (owp--eprop "POST_DATE")
                         (owp--eprop "SCHEDULED")
                         (owp--eprop "DEADLINE")
                         (owp--eprop "TIMESTAMP_IA")
                         (owp--eprop "TIMESTAMP")))
         (cons "title" (or (owp--eprop "TITLE")
                          (nth 4 (org-heading-components))))
         (cons "description" nil)
         (cons "tags" (or
                       (split-string (or (owp--eprop "POST_TAGS") "") "\\( *, *\\)" t)
                       (-map 'org-no-properties (org-get-tags-at (point) nil))))
         (cons "categories"
               (split-string (or (owp--eprop "CATEGORY") "")
                             "\\( *, *\\)" t))
         (cons "post-id" (or (owp--eprop "POSTID")
                            (owp--eprop "POST_ID")))
         (cons "parent" (owp--bprop-parent-id
                         (owp--eprop "PARENT")))
         (cons "excerpt" (owp--eprop "DESCRIPTION"))
         (cons "permalink" (owp--eprop "PERMALINK")))))
    parsed-entry))

(defun owp--startup-library-check (library-name current-version min-version)
  "Warn when LIBRARY-NAME CURRENT-VERSION is less than the MIN-VERSION version."
  (when (version< current-version min-version)
    (display-warning
     'org2blog/wp
     (format
      (concat "Sorry, I might have problems running right now. It looks like "
              "version %s of %s is installed, but I need "
              "at least version %s of %s to run. You might not run "
              "into problems, but please install at "
              "least version %s of %s and run me again. "
              "Sorry for the trouble, but running a newer version "
              "will make your whole Org2Blog experience "
              "faster, easier, and more fun. See you soon!")
      current-version library-name
      min-version library-name
      min-version library-name)
     :error)))

(defun owp--startup-asserts ()
  "Verify startup assertions."
  (owp--startup-library-check "Org mode" org-version
                              org2blog/wp-required-org-version)
  (owp--startup-library-check "Emacs" emacs-version owp--minimal-emacs))

(defun owp--error (amessage &optional report-details)
  "Display error AMESSAGE and non-nil REPORT-DETAILS for a human."
  (display-warning 'org2blog/wp amessage :error)
  (when report-details
    (display-warning
     'org2blog/wp
     (concat
      (owp--get-timestamp) "\n"
      (format "Please report that along with the following details:\n%s\n%s"
              report-details
              (owp-version-info t)))
     :error))
  (display-buffer "*Warnings*")
  (message
   (concat
    "I’m sorry, I ran into an error and couldn’t do that 🙇. "
    "Please view the “*Warnings*” buffer for details.")))

(defun owp--get-timestamp ()
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

(defun owp--contact-info (contact)
  "Create string from CONTACT info."
  (let ((result (concat (car contact) " <" (cdr contact) ">")))
    result))

(defun owp--contacts-info (contacts)
  "Create string from CONTACTS info."
  (let* ((contacts (-map
                    'owp--contact-info
                    contacts))
         (separated (-interpose ", " contacts))
         (all (apply 's-concat separated)))
    all))

(defun owp--update-header ()
  "Update Org2Blog file header."
  (interactive)
  (find-file "org2blog.el")
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^;; Author: ")
    (kill-whole-line 6)
    (insert (format ";; Author: %s\n" (owp--contacts-info (owp--pkg "authors"))))
    (insert (format ";; Maintainer: %s\n" (owp--contact-info
                                           (owp--pkg "maintainer"))))
    (insert (format ";; Version: %s\n" (owp--pkg "version")))
    (insert (format ";; Package-Requires: (%s)\n"
                    (let* ((ls (cons (cons 'emacs (list (owp--pkg "emacs")))
                                     (owp--pkg "requirements")))
                           (defs (-map (lambda (req)
                                         (format "(%s \"%s\")" (car req) (cadr req)))
                                       ls))
                           (spcd (-interpose " " defs))
                           (result (apply 's-concat spcd)))
                      result)))
    (insert (format ";; Keywords: %s\n"
                    (apply 's-concat (-interpose ", " (owp--pkg "keywords")))))
    (insert (format ";; Homepage: %s\n" (owp--pkg "homepage")))))



;;; Mode

;;;###autoload
(define-minor-mode org2blog/wp-mode
  "Toggle org2blog/wp minor mode.

With no argument, the mode is toggled on/off.

Non-nil argument turns mode on.

Nil argument turns mode off.

Commands:
\\{owp-mode-map}

Entry to this mode calls the value of `owp-mode-hook'."

  :init-value nil
  :lighter " o2b"
  :group 'org2blog/wp
  :keymap (owp--init-entry-mode-map)

  (when org2blog/wp-mode
    (run-mode-hooks 'owp-mode-hook)))



;;;; Initialization

(cond (after-init-time
       (owp--startup-asserts)
       (owp-version-info))
      ((not after-init-time)
       (add-hook 'after-init-hook #'owp--startup-asserts t)
       (add-hook 'after-init-hook #'owp-version-info t)))

(provide 'org2blog)

;;; org2blog.el ends here
