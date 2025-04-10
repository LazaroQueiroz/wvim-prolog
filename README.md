# 📝 Wvim
Worse Vim is a text editor, inspired by Vim, written in Haskell and Prolog. Created as a project for the PLP course (Programming Language Paradigms), managed by Everton L. G. Alves. 

---
## How To Run It
1. After you cloned, go to the source file of the project (the one that contains app/ and src/ folders).
2. Then, run `./wvim <nome-do-arquivo || vazio>`.
3. Now, you can use Wvim at your will.

## 🛠️ Features
✅ Basic cursor movement

✅ Text selection

✅ Character insertion and deletion

✅ Undo/redo functionality

✅ Implementation without pre-built text components

✅ CLI-based interactive interface

## 📌 Development Team
* [Lázaro Queiroz do Nascimento](https://github.com/LazaroQueiroz)
* [Igor Raffael Menezes de Melo](https://github.com/igor-raffael)
* [Rafael Barreto da Silva](https://github.com/rafaell-silva)
* [Matheus Galdino](https://github.com/MatheusGaldinoo)

## CheatSheet

### Normal Mode

The commands below can be used with multipliers.
To use a multiplier, type a number followed by the motion
`multiplier<motion>` (e.g.: `3dd` deletes 3 lines, `5x` deletes 5 characters, etc.)

- `h` - moves right
- `l` - moves left
- `j` - moves down
- `k` - moves up
- `dd` - delete current line
- `x` - delete current character
- `w` - move to next word
- `b` - move to previous word
- `r<new char>` - replaces the character under the cursor for the new char typed

The commands below cannot be used with multipliers

- `arrow keys` - move around the text
- `i` - enters Insert Mode before the cursor
- `a` - enters Insert Mode after the cursor
- `o` - enters Insert Mode after the current line, e.t., creates a new line below the current line the cursor is on
- `:` - enters Command Mode after the cursor
- `/` - enters Substitution Mode after the cursor
- `v` - enters Visual Mode after the cursor
- `p` - paste copy buffer
- `u` - undo some alteration
- `t` - redo some alteration
- `{` - opens a new empty buffer
- `[` - go to the left buffer (if exists)
- `]` - go to the right buffer (if exists)
- `n` - go to next search word occurrence (if exists)
- `N` - go to previous search word occurrence (if exists)

### Insert Mode

- `<any key>` - type the key
- `backspace` - delete text
- `enter` - break lines
- `ESC` - exit Insert Mode to Normal Mode

### Command Mode

- `w` - save file
- `w <filename>` - save file with filename
- `q` - quit buffer
- `q!` - force quit buffer
- `wq` - save file and quit buffer
- `wq!` - force save file and force save

### Visual Mode

- `v` - select text to copy buffer and enter Normal Mode
- `ESC` - exit Visual Mode and enter Normal Mode (without adding the selected text to copy buffer)

### Substitution Mode (and Search Mode)

- `<any text><enter>` - add text to search buffer and enter Normal Mode
- `<original text>/<replace text><enter>` - replace text
- `ESC` - exit Substitution Mode and enter Normal Mode
