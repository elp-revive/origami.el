# Change Log

All notable changes to this project will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.


## 4.2 (Unreleased)
> Released N/A

* Add new feature, `indicators`.
* Add support for JSON.
* Add support for HTML.
* Add support for XML.
* Fix folding for single-line opening declaration, [#36](https://github.com/emacs-origami/origami.el/issues/36).
* Limit the click to only plus/minus indicators.
* Simplify c marco parser.
* Add c# marco parser.
* Add double slashes parser to c#.
* Split `indciators` to it's minor mode, `origami-indicators-mode`.
* refactor: Use buil-in ellipsis (#54)

## 4.1
> Released Mar 2, 2021

* Fixed `issue from c-style` overlap node.

## 4.0
> Released Feb 4, 2021

* Expand `c-style` parser to the beginning of line.
* Add support for multiline comment in Lua.
* Add `else` region folding.
* Fixed filtering error if previous line is comment/string block.
* Add Ruby support.

## 3.0
> Released Jan 30, 2021

* Add CI test
* Fixed all linting issues

## 3.0
> Released Jan 26, 2021

* Fixed all compile warnings
* Code clean up and improve document string
* Resolved error, void variable `origami-mode-map`

## 1.0
> Released May 14, 2020

* Upstream release
