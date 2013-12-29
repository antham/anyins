# Anyins [![Build Status](https://travis-ci.org/antham/anyins.png?branch=master)](https://travis-ci.org/antham/anyins)

Insert content either from kill-ring or from shell command result at marked point or vertically like rectangular do.

[Look at demonstration video](http://www.dailymotion.com/video/x18qqzc_anyins-emacs-plugin_tech)

### Install

You can use melpa:

    M-x package-install RET anyins

or you can pull it from here and you have to add in your emacs config :

```elisp
(add-to-list 'load-path "/path/to/anyins")
(require 'anyins)
```

### Usage

You can map `anyins-mode` command to key to start anyins-mode easily :

```elisp
(global-set-key (kbd "C-c a") 'anyins-mode)
```

When you turn anyins-mode on, you can press <kbd>RET</kbd> to mark point in buffer where you want to insert some contents. After that press <kbd>!</kbd> to insert result from a shell command or press <kbd>y</kbd> to insert last entry in kill-ring. <kbd>q</kbd> will stop anyins-mode leaving everything untouched.

Newline is used as delimitor to split content to insert and content is inserted in same order of recording.

### Commands

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>RET</kbd>     | Mark current point in buffer.
<kbd>y</kbd>       | Insert last entry from kill-ring.
<kbd>!</kbd>       | Insert shell command result.
<kbd>q</kbd>       | Abort anyins-mode.
