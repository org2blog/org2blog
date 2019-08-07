(define-package "org2blog" "1.1.0" "Blog from Org mode to WordPress"
  '((htmlize "1.55" "https://github.com/hniksic/emacs-htmlize.git")
    (hydra "0.14.0" "https://github.com/abo-abo/hydra.git")
    (org "9.1.9" "https://code.orgmode.org/bzg/org-mode")
    (xml-rpc "1.6.12" "https://github.com/hexmode/xml-rpc-el.git"))
  :authors
  '(("Puneeth Chaganti" . "punchagan+org2blog@gmail.com"))
  :maintainer
  '("Grant Rettke" . "grant@wisdomandwonder.com")
  :keywords
  '("comm" "convenience" "outlines" "wp")
  :homepage "https://github.com/org2blog/org2blog")
