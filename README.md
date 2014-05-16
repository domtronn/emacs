simplemacs
==========

Emacs Configuration &amp; Setup
-------------------------------
First of all, we need to install emacs, you can [download Emacs for OSX from here](http://emacsformacosx.com/). Next, clone this branch into your home directory.
```
git clone https://github.com/domtronn/emacs.git ~/.simplemacs
```
`cd` into it and then run `./Setup`
```
cd ~/.simplemacs
./Setup
```
Once it's installed you can open Emacs! On start up, you will be asked for a `*.project` file (a list of project include directories). If you don't have one, just hit enter.

All things M-x
--------------
Starting out in emacs can be difficult to know where or what to do next. This set up has been designed to be familiar to modern IDE users. It *should* behave in a way you expect.

E.g.
```
Cmd+o  : Opens a file
Cmd+w  : Closes a tab
Cmd+c  : Copies
Cmd+z  : Undo
```

But the real thing begins with `M-x` (*meta+x*) command *i.e.* `Alt+x`. This is a builtin emacs command prompt. Hitting this key will open the `Minibuffer` (*at the bottom of your window*) and you can start typing commands to be evaluated! Such as `find-file`, `eval-region`, `forward-word` etc.

This `Minibuffer` is used for everything in emacs. You're trying to open a file? Look in the `Minibuffer`. Can't seem to do anything in the window?  You're probably in the `Minibuffer`.

It takes a bit of getting used to, but it's the go to place for messages and, feedback and prompts. If you find yourself in the `Minibuffer` and want to get out, hit either `Esc` or `Ctrl+g`.

Key-Binding Cheat Sheet
-----------------------
IMO, the best feature of Emacs is it's adaptability, primarily, it's ability to rebind ANYTHING to ANYTHING! People always ask me "Why is undo `Ctrl+_` in emacs?! That doesn't make sense", __BECAUSE THEY COULD!!__ But the principal is very much, if you want Emacs to do something, you can make it do it and then bind it to any key combination you like.

#### Understanding key Combinations
To see what key combinations we have (although I will list them below), you can open the `keys.el` file and look at what binds to what. Key combinations usually consist of an operator (`Ctrl`, `Alt`, `Cmd` etc.) and a key. The operators are as follows:

|`C`| Ctrl|
|`M`| Meta or Alt |
|`H`| Hyper or Fn |
|`S`| Shift |
|`s`| Cmd |

This means that `(global-set-key (kbd "C-j") 'foo)` would bind `Ctrl+j` to the `foo` function. 

Some things to be aware of though if you want to start rebinding:
* You can bind multiple operators to one key!
  _e.g._ `C-M-u` would be pressing `Ctrl+Alt+u`
* Some key combinations are called __Prefix__ Keys, these allow you to chain key combinations.
  _e.g._ `C-x C-l` would be pressing `Ctrl+x` followed by `Ctrl+l`. (__N.B.__ I wouldn't rebind these...)
* Binding against shift can be funny. Since holding `Shift` toggles capitalisation, it's mostly found with other operators.
  However, you might think `C-S-x` would be the key binding for `Ctrl+Shift+x`, but it would actually be `C-X`.
  Notice how the _X_ is capital in the real binding? Yeah, that's a thing.
  
  #### Cheat Sheet
