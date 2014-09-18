lgtk-plot-demo
==============

A small demo app for LGtk plotting simple 2D functions

Installation
============

This demo works with lgtk==0.9 and only with the GTK backend:
```
git clone https://github.com/divipp/lgtk.git
cd lgtk
cabal install -fgtk
cd ..
git clone https://github.com/pdani/lgtk-plot-demo.git
cd lgtk-plot-demo
cabal install
lgtk-plot-demo
```

Documentation
=============

```
$ cabal configure
$ cabal haddock
```

The generated docs can be found here:
dist/doc/html/LinkedList/index.html
