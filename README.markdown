# ari.el - Emacs Lisp Collection for Several Developers.

## Description

This is an useful library collection for Emacs, written by Emacs users in Ariel Networks, Inc., writing articles about Emacs for SoftwareDesign.

## Installation

We recommend you to install [auto-install.el](http://www.emacswiki.org/AutoInstall) if you are not ready to use it yet.

    (let ((buffer (url-retrieve-synchronously
                   "http://github.com/arielnetworks/ari-el/raw/master/ari-install.el")))
      (save-excursion
        (set-buffer buffer)
        (goto-char (point-min))
        (re-search-forward "^$" nil 'move)
        (eval-region (point) (point-max))
        (kill-buffer (current-buffer))))

Do you have any problem? Try a below process.

    $ wget https://github.com/arielnetworks/ari-el/zipball/master
    $ unzip arielnetworks-ari-el-*.zip
    $ cp arielnetworks-ari-el-*/lisp/* /path/to/your/load-path

## Packages

* ari
* ari-cursor
* ari-debug
* ari-fn
* ari-net
* ari-seq
* ari-string
* ari-symbol
* ari-util
* ari-ext-yasnippet

## Something like a "Coding standard"

* Write DocString in each functions unless the behavior is too clear.
* Add a package name to each functions at a front of it's name (ex. ari-util:compose / don't use global namespace).
* Add `#'' prefix to any LAMBDA.
* Private functions are given a name starts with it's namespace and '%' (ex. ari:%g!-symbol-p)

## Bugs

## Contributors

* Taiki SUGAWARA &lt;sugawara_t@ariel-networks.com&gt;
* Eitarow FUKAMACHI &lt;fukamachi_e@ariel-networks.com&gt;

## License

Copyright (c) 2010-2011 Ariel Networks, Inc. All rights reserved.  
For detail, see COPYING file.
