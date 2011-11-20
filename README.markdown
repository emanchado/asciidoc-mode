AsciiDoc major mode for Emacs
=============================

This is my first attempt to write an Emacs major mode. I choose
AsciiDoc because it's a format I'm using, and I didn't like the only
major mode I found for it,
[doc-mode](http://xpt.sourceforge.net/tools/doc-mode/). I followed
an
[old tutorial](http://web.archive.org/web/20070702002238/http://two-wugs.net/emacs/mode-tutorial.html)
and some other stuff I found on the net to improve the
highlighting of multi-line constructs.

It doesn't do a lot just yet, but I'll probably improve it when I
learn a bit more about Emacs major modes and I discover more things
I'd like to add to the mode. Suggestions and patches welcome ;-)

To use:

    (add-to-list 'load-path "~/wherever/you/downloaded/this/asciidoc")
    (require 'asciidoc-mode)
    ; Enable for .asciidoc and .txt files automatically
    (setq auto-mode-alist
       (cons '("\\.\\(asciidoc\\|txt\\)" . asciidoc-mode) auto-mode-alist))
