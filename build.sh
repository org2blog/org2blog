org2blog ()
{
  printf "\n"
  printf "=======================================\n"
  printf "Welcome To The Org2Blog Package Creator\n"
  printf "=======================================\n"
  printf "\n"
  printf "Welcome and thank you for using Org2Blog.\n"
  printf "\n"
  printf "This script prepares an Org2Blog Emacs Lisp package.\n"
  printf "\n"
  printf "It is non-destructive. It will only create and\n"
  printf "never destroy. Therefore between runs you need to\n"
  printf "delete the package directory and the package.\n"
  printf "They both live under /tmp. The directory is named\n"
  printf "‘org2blog-<version>’ and the package is named\n"
  printf "‘org2blog-<version>.tar’.\n"
  printf "\n"
  printf "Status messages will explain what is happening where\n"
  printf "and when.\n"
  printf "\n"
  printf "Upon successful completion you will have a package that you\n"
  printf "can install using ‘package-install-file’.\n"
  printf "\n"
  printf "Thanks and have a great time blogging."
  printf "\n\n"

  local version="1.1.0" #  TODO Automate version number
  local name="org2blog-$version"
  local dir="$TMPDIR/$name"
  local content="$dir/$name"

  if [ -d "$dir" ]; then
    printf "I'm sorry but %s already exists.\n" $dir
    printf "Please delete it and run me again."
    printf "\n\n"
    return 0
  fi

  if [ -f "$dir.tar" ]; then
    printf "I'm sorry but %s.tar already exists.\n" $dir
    printf "Please delete it and run me again."
    printf "\n\n"
    return 0
  fi

  printf "Building...\n\n"

  mkdir -p "$content"

  cp LICENSE.txt "$content" #  TODO Sort alpha, add metaweblog
  cp org2blog.el "$content"
  cp org2blog-pkg.el "$content"
  cp docs/Org2Bloggers.org "$content"
  cp ox-wp.el "$content"
  cp README.org "$content"

  tar -cvf "$dir.tar" --directory "$dir" "$name"

  printf "\n"
  printf "Just created %s.tar.\n" $dir

  printf "\n"
  printf "If it looks like everything went well then you are\n"
  printf "ready to test. If not then please let me know!\n"
  printf "\n"
  printf "Thanks and have a great day."
  printf "\n\n"
}

org2blog "$@"
