# ChordPro mode

## Introduction

Emacs major mode for editing files in the [ChordPro
format](https://www.chordpro.org/chordpro/chordpro-introduction/).

## Installation

Clone this repository, then manually install `chordpro-mode` with `M-x
package-install-file`.

## Usage

Files with the `.cho` extension will automatically be opened with `chordpro-mode`.

### Editing

`chordpro-mode.el` adds functionality for normalizing (formatting)
chords correctly so that the ChordPro command line program can read
them. This means inserting brackets, trimming leading and trailing
whitespace, and capitalizing chords.

* `]`        : Close and normalize chord at point.
* `[`        : Insert a normalized chord at point.
* `C-c C-w`  : Kill the chord at point.
* `C-c C->`  : Kill the next chord after point.
* `C-c M-w`  : Copy the chord at point.
* `C-c >`    : Copy the next chord after point.
* `C-c ;`    : Insert a comment directive.
* `C-c {`    : Insert an environment directive, e.g. `bridge`,
        `chorus`, `grid`, `tab`, or `verse`.  Wraps region when active.
* `C-c }`    : Close current open environment directive.
* `C-c C-t`  : Insert a title directive.
* `C-c C-s`  : Insert a subtitle directive.
* `C-c %`    : Replace the chord at point with another normalized chord.
* `C-M-n`    : Move chord at point forward PREFIX chars.
* `C-M-p`    : Move chord at point backward PREFIX chars.
* `C-c C-a`  : Replace buffer or active region with ChordPro format.
* `C-c C-c`  : Export ChordPro file to PDF.

### Generate ChordPro files

Certain websites, such as [Ultimate
Guitar](https://www.ultimate-guitar.com/) provide tabs with lyrics and
chords in a format that the `chordpro` program can convert into
ChordPro files:

```
        D        G      D

Swing low, sweet chariot,

                       A7

Comin' for to carry me home.

       D7        G      D

Swing low, sweet chariot,

               A7       D

Comin' for to carry me home.
```

To replace the text above to ChordPro format, select the region and
run `chordpro-a2crd` with a prefix argument `C-u`, which deletes empty
lines in the region before attempting conversion:

```
Swing lo[D]w, sweet [G]chariot[D],
Comin' for to carry me [A7]home.
Swing l[D7]ow, sweet [G]chariot[D],
Comin' for to c[A7]arry me h[D]ome.
```

You can then edit this buffer and save it to a `.cho` file before
exporting it to PDF.

### Export to PDF

If the `chordpro` command is in your `$PATH` ([installation
instructions](https://www.chordpro.org/chordpro/ChordPro-Installation.html)),
`M-x chordpro-export` will export the current ChordPro buffer to PDF.
The ChordPro buffer must be visiting a file on disk in order for this
function to work.

### Live export

This hook runs `chordpro-export` each time you save the buffer:

```
(add-hook 'after-save-hook #'chordpro-export nil t)
```

You can get a live preview of the exported PDF if you run an PDF
viewer like [Zathura](https://pwmt.org/projects/zathura/) which
automatically re-renders the document when the underlying file
changes:

![demo.png](./img/demo.png)

## Customization

- `chordpro-environment-directives`

    You can change if you want to insert environment directives
    besides `bridge`, `chorus`, `grid`, `tab`, or `verse`.

## Acknowledgements

- [Howard Ding](https://github.com/hading) for writing the original version of `chordpro-mode.el`
- [Johan Vromans](https://johan.vromans.org/) for writing `chordpro`
  and for feedback on `chordpro-mode.el`

## Changelog

### 2.7.0-pre

Nothing yet...

### 2.6.0 [2025-04-10]

- Fix regexp matching bug in `chordpro-close-environment-directive`.
- Always insert closing directives on newline.

### 2.5.0 [2025-03-30]

- Update keybindings to fit Emacs keybinding conventions.  See
  [Keybinding
  Conventions](http://www.gnu.org/s/emacs/manual/html_node/elisp/Key-Binding-Conventions.html) for more information.

### 2.4.0 [2025-03-30]

- Fix `chordpro-insert-environment-directive` by removing duplicate
  `defun`.
- Add new command `chordpro-close-environment-directive` to close the
  current open directive.

### 2.3.0 [2024-08-18]

- `chordpro-insert-environment-directive` wraps the region when active.
- Minimum required Emacs version is now 29.1.

### 2.2.0 [2023-12-04]

- Add `chordpro-insert-environment-directive` command and
  `chordpro-environment-directives` customization option
- Remove `chordpro-insert-chorus` (superceded by
  `chordpro-insert-environment-directive`)

### 2.1.0 [2023-09-20]

- Add `chordpro-a2crd` function
- Add `chordpro-close-chord` function for easy editing
- Bind `chordpro-export` to `C-c C-c`
- Handle errors in `chordpro-export`
- Rename `chordpro-transpose-*` to `chordpro-move-*` for clarity

### 2.0 [2023-07-15]

- Forked in 2023
- Add `chordpro-export` function
- Remove broken mouse keybindings
- Use thingatpt for chords
- Clean up obsolete code
- Add `.cho` to auto-mode-alist

### 1.0

- Initial version created by Howard Ding in 2014

<!-- LOCALWORDS: comin arry ome -->
