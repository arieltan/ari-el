# ari.el - Emacs Lisp Collection for Several Developers.

## Description

This is an useful library collection for Emacs, written by Emacs users in Ariel Networks, Inc., writing articles about Emacs for SoftwareDesign.

## Installation

    (require 'ari)

## Packages

* ari
* ari-fn
* ari-seq
* ari-string
* ari-util

## Something like a "Coding standard"

* Write DocString in each functions unless the behavior is too clear.
* Add a package name to each functions at a front of it's name (ex. ari-util:compose / don't use global namespace).
* Add `#'' prefix to any LAMBDA.

## Bugs

## Contributors

* Taiki SUGAWARA &lt;sugawara_t@ariel-networks.com&gt;
* Eitarow FUKAMACHI &lt;fukamachi_e@ariel-networks.com&gt;

## License

Copyright (c) 2010 Ariel Networks, Inc. All rights reserved.  
For detail, see COPYING file.
