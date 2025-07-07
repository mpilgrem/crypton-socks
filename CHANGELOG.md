Change log for `crypton-socks`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## UNRELEASED

* Move library modules to directory `src` and example application to directory
  `example`.
* Change data types `SocksHello` and `SocksHelloResponse` (each with a single,
  unary data constructor without strictness annotation) to `newtype`.
* Add missing top-level signatures to library.
* Name the example application `crypton-socks-example`, and move it being built
  behind Cabal flag `example` (default: false).

## 0.6.1

* Rename `socks-0.6.1` package as `crypton-socks-0.6.1`.
* Change maintainer field to `Mike Pilgrem <public@pilgrem.com>` and
  `Kazu Yamamoto <kazu@iij.ad.jp>`.
* Add `CHANGELOG.md`.
