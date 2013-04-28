Haskell Hangman
=================

A simple [Haskell][4] hangman game that runs in your terminal.

I wrote this hangman game within 2 hours while I was bored out of my wits
revising for exams. I personally got quite a kick out of actually writing
something so fun in such a short amount of time.

Hopefully by making this open source, this project can serve as a very simple
skeleton project for more advanced Haskell command-line based applications.


## Getting started

Before you start, make sure that you have the Cabal binaries directory in your
PATH (on Mac this is `~/Library/Haskell/bin`).


#### Installating from [Hackage][3]

The fastest way to install this game from Hackage is with [cabal-install][5]. Just do:

```
cabal install hangman
```

You can then run the game by executing `hangman`.


#### Developing / Building from Source

This project is [cabalized][1] and I highly recommend using [cabal-dev][2] to
build the project. You can install cabal-dev by:

```
cabal update
cabal install cabal-dev
```

Then you'll just want to check the project out and tell cabal-dev to build the project for you.

```
git clone git@github.com:fatuhoku/haskell-hangman.git
cd haskell-hangman
cabal-dev install
```

Cabal-dev will download, compile and install the project dependencies into a
local directory named `cabal-dev`. It will also configure and build the hangman
game itself. Just run the game with `dist/build/hangman/hangman`.


## In case of trouble...

Don't panic. Contact me with any questions on Twitter (@hokshunpoon).


## Thinking of helping out?

Fork this project and contribute by submitting a Pull Request!

Though I usually advocate TDD, there are no tests in this project at the
moment. You can help me add some!

## Thanks

[TODO INSERT THANKS HERE]

## See also

[TODO INSERT LINKS HERE]

[1]: http://www.haskell.org/cabal Haskell Cabal
[2]: https://github.com/creswick/cabal-dev/blob/master/README.md Cabal-dev README
[3]: http://hackage.haskell.org/ Hackage
[4]: http://www.haskell.org/haskellwiki/Haskell Haskell
[5]: http://www.haskell.org/haskellwiki/Cabal-Install Cabal Install
