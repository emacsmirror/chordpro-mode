# ChordPro mode #

## Introduction ##

This is an Emacs major mode, derived from text-mode, for editing files
in the [ChordPro
format](https://www.chordpro.org/chordpro/ChordPro-File-Format-Specification.html).

It is currently somewhat crude, but it does simplify a few things and
I do intend to keep adding any refinements that I find useful in my
work with Chordpro files. The font-lock is based off a previously
existing chordpro mode, but this provides more operations.

I'm still developing and experimenting with this, so as I go along I
might decide that different mouse or keybindings work better and will
probably provide more functionality as I see things that will make the
work that I do easier. There are some fairly obvious things left to do
and I'll add them I need them. If you have any ideas feel free to
create an issue (or even better do it and send a pull request).

## Installation ##

Copy chordpro-mode.el into somewhere in your emacs load path (I put it
in ~/.emacs.d).

Then put something like this into your .emacs file (I use the .pro
extension for such files):

    (setq auto-mode-alist (cons '("\\.pro$" . chordpro-mode) auto-mode-alist))
    (autoload 'chordpro-mode "chordpro-mode")

Now when you visit a .pro file you should automatically get
chordpro-mode. The file will automatically be saved in latin-1
encoding (you can change this by setting the chordpro-file-encoding
variable in your .emacs, but you probably shouldn't as Chordii
currently expects latin-1 encoded files).

Some of the functions use dropdown-list.el, which can be installed
with package-list-packages in modern emacs. If you don't have this
installed they'll just do nothing.

## Use ##

Files with the `.cho` extension will automatically be opened with `chordpro-mode`.

### Keyboard ###

Most of the keyboard commands use the Ctrl-c prefix.

* Ctrl-c i : Insert a chord at the point. You'll be prompted for the
  chord name in the minibuffer. The brackets will automatically be
  inserted, space trimmed, and the chord capitalized.
* Ctrl-c l : Insert a chord at the point, chosen from a dropdown list
  of chords already in the document.
* Ctrl-c w : Kills the current chord. The current chord is one
  containing the point - because of the way emacs works this means
  that this command doesn't do what you want if the cursor is on the
  opening [ of a chord, only if it is between that and the closing ],
  inclusive. But there's another command for that.
* Ctrl-c z : Kills the next chord. Finds the next chord after the
  point and kills it. This one works if you are on the opening
  [, or if you are between chords.
* Ctrl-c r : Replace the current chord with one chosen from a dropdown
  list of chords already in the document.
* Ctrl-c c : Copy the current chord
* Ctrl-c x : Copy the next chord
* Ctrl-Meta-n : Move current chord forward PREFIX chars. 
* Ctrl-Meta-p : Move current chord backward PREFIX chars
* Ctrl-c h : Insert a chordpro comment
* Ctrl-c h : Insert a chordpro chorus
* Ctrl-c t : Insert a chordpro title
* Ctrl-c s : Insert a chordpro subtitle

### Export to PDF

If the `chordpro` command is in your `$PATH` ([installation
instructions](https://www.chordpro.org/chordpro/ChordPro-Installation.html)),
`M-x chordpro-export` will export the current ChordPro buffer to PDF.

### Live export

This hook runs `chordpro-export` each time you save the buffer:

```
(add-hook 'after-save-hook #'chordpro-export nil t)
```

You can get a live preview of the exported PDF if you run an PDF
viewer like [Zathura](https://pwmt.org/projects/zathura/) which
automatically re-renders the document when the underlying file
changes.

## Changelog

### 2.0-pre

- Forked in 2023
- Add `chordpro-export` function
- Remove broken mouse keybindings
- Clean up obsolete code
- Add `.cho` to auto-mode-alist

### 1.0

- Initial version created by Howard Ding in 2014
