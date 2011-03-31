
;;;### (autoloads (org2blog/wp-preview-subtree-post org2blog/wp-preview-buffer-post
;;;;;;  org2blog/wp-mark-subtree-as-draft org2blog/wp-post-subtree
;;;;;;  org2blog/wp-new-entry org2blog/wp-login org2blog/wp-mode)
;;;;;;  "org2blog" "org2blog.el" (19859 17639))
;;; Generated autoloads from org2blog.el

(autoload 'org2blog/wp-mode "org2blog" "\
Toggle org2blog/wp mode.
With no argument, the mode is toggled on/off.  
Non-nil argument turns mode on. 
Nil argument turns mode off.

Commands:
\\{org2blog/wp-entry-mode-map}

Entry to this mode calls the value of `org2blog/wp-mode-hook'.

\(fn &optional ARG)" t nil)

(autoload 'org2blog/wp-login "org2blog" "\
Logs into the blog. Initializes the internal data structures.

\(fn)" t nil)

(autoload 'org2blog/wp-new-entry "org2blog" "\
Creates a new blog entry.

\(fn)" t nil)

(autoload 'org2blog/wp-post-subtree "org2blog" "\
Post the current entry as a draft. Publish if PUBLISH is non-nil.

\(fn &optional PUBLISH)" t nil)

(autoload 'org2blog/wp-mark-subtree-as-draft "org2blog" "\
Post the current subtree as a draft. Saves details in tracking file.

\(fn)" t nil)

(autoload 'org2blog/wp-preview-buffer-post "org2blog" "\
Preview the present buffer in browser, if posted.

\(fn)" t nil)

(autoload 'org2blog/wp-preview-subtree-post "org2blog" "\
Preview the present subtree in browser, if posted.

\(fn)" t nil)

;;;***

(provide 'org2blog-autoloads)
