# js-injector.el #

A package which adds various utility functions around the use of
`require.js`, such as importing modules from requirejs projects,
importing modules relatively and importing in a projects installed
node modules package.json.

This package is primarily built to work with my own project management
package, [`jpop.el`](https://github.com/domtronn/jpop.el).

This can also be used with with
[`projectile`](https://github.com/bbatsov/projectile), but due to its
source contorl based project creation, it can only import modules
_relatively_.

However, you can write your own 3rd party adapters to make this work
with the way you would like.

## Installation ##

This project requries [`js2-mode`](https://github.com/mooz/js2-mode)
for the AST parsing.

Clone this repository and add it to Emac's `load-path`. Then require the file:
```
(add-to-list 'load-path "/path/to/cloned/repo")
(require 'js-injector)
```
Then enable it in `js2-mode` using
```
(add-hook 'js-mode-hook 'js-injector-minor-mode)
```

# Usage

If you enable `js-injector-minor-mode` you will have access to the following keybindings:

Key Binding | Command & Effect
-------- | ---
`C-c C-j C-i` | `js-injector-clever-import-module`
 | Try to cleverly import the module based on current point context.
`C-c C-j i` | `js-injector-import-module`
 | Import a module from the current project
 `C-c j i` | `js-injector-import-module-at-point`
 | Import the module under point
`C-c C-j k` | `js-injectore-remove-module`
 | Remove a module from the current file
 `C-c j k` | `js-injector-remove-unused`
 | Remove modules from the current file that are unused
`C-c C-j l` | `js-injector-sort-imports`
 | Sort all of the imports in the current file
`C-c C-j u` | `js-injector-update-imports`
 | Run through all imports defined in the function params and import/reimport them
`C-c C-j n` | `js-injector-node-import-module`
 | Import a module using nodes inline require
 `C-c j n` | `js-injector-node-import-module-at-point`
 | Import the module at point using nodes inline require

 Calling most of the above commands with the prefix argument _(`C-u`)_
 will allow you to import the module with a different name, rather
 than using the file name as the import.

### Drag & Drop

You can also drag and drop files from both **Dired** &
[**Neotree**](https://github.com/jaypei/emacs-neotree). To enable
this, you will have to add the following to your config;

```elisp
(define-key neotree-mode-map [drag-mouse-1] 'js-injector-drag-inject-file)
(define-key dired-mode-map [drag-mouse-1] 'js-injector-drag-inject-file)
```

# Examples #

## `js-injector-node-import-module` ##

Given working on a node project with the following `package.json`

```json
...
"dependencies": {
  "express": "1.4.5",
  "request": "0.5.7",
  "package-name": "10.2.9"
}
...
```

Calling `js-injector-node-import-module` will prompt you for a module,
including installed node modules, like `express, request,
package-name`.  Selecting one of the options will expand point into

```javascript
var express = require('express');
```

Also, pacakges with dashes in their name _(e.g. `package-name` above)_
will be sanitised according to the custom variable
`js-injector-node-camelise`.

By default it will concatenate words, e.g.

```javascript
var packageone   = require('package-one');
var packagetwo   = require('package_two');
var packageThree = require('packageThree');
var packageFour  = require('package_Four');
```

## `require-dependency-at-point` ##

Calling `js-injector-node-import-module` over the `classFileName` below,
will expand the path to the path relative to the current file like this

```javascript
var classFileName = require('../../folder/classFileName');
```

## `inject-dependency-at-point` ##

Given that you're in a require js file like below, and have
[`jpop`](https://github.com/domtronn/jpop.el) set up with your
requirejs config.

```javascript
require([
  'project/myclass'
], function (MyClass) {
...
  var class = new Image();
...
})
```

Calling `js-injector-import-mode-at-point` over the `new Image()` will
require the class _(if it exists in the project)_ like this

```javascript
require([
  'project/myclass',
  'project/path/to/image'
], function (MyClass, Image) {
...
  var class = new Image();
...
})
```

If you have configured your [`jpop`](https://github.com/domtronn/jpop.el) project, like this

```json
"libs": [
  { "id":"project", "dir":"path/to/project"}
  { "id":"library", "dir":"path/to/library"}
]
```

And try to include a class which exists in _both_ the library and the
project, you will be prompted with the two choices,
e.g. `project/path/to/image, library/path/to/image ` Selecting one of
these will inject the selected one as above, so if you chose the
`image` class in the library it would expand to something like this
instead

```javascript
require([
  'project/myclass',
  'library/path/to/image'
], function (MyClass, Image) {
...
  var class = new |Image();
...
})
```
