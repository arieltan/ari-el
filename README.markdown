# ari.el - Emacs Lisp Collection for Several Developers.

## Description

This is an useful library collection for Emacs, written by Emacs users in Ariel Networks, Inc., writing articles about Emacs for SoftwareDesign.

## Installation

Put all files, in the "lisp" directory, to your load-path.

    (require 'ari)

## Packages

* ari
* ari-cursor
* ari-debug
* ari-fn
* ari-net
* ari-seq
* ari-string
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

Copyright (c) 2010 Ariel Networks, Inc. All rights reserved.  
For detail, see COPYING file.
