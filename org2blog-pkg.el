(define-package "org2blog" "1.1.0" "Blog from Org mode to WordPress"
  '((dash "2.15.0" "https://github.com/magnars/dash.el.git")
    (dash-functional "2.15.0" nil)
    (htmlize "1.55" "https://github.com/hniksic/emacs-htmlize.git")
    (hydra "0.14.0" "https://github.com/abo-abo/hydra.git")
    (metaweblog "1.0.1" "https://github.com/org2blog/metaweblog.git")
    (org "9.2.1" "https://code.orgmode.org/bzg/org-mode")
    (xml-rpc "1.6.12" "https://github.com/hexmode/xml-rpc-el.git"))
  :authors
  '(("Puneeth Chaganti" . "punchagan+org2blog@gmail.com"))
  :maintainer
  '("Grant Rettke" . "grant@wisdomandwonder.com")
  :keywords
  '("comm" "convenience" "outlines" "wp")
  :homepage "https://github.com/org2blog/org2blog")
