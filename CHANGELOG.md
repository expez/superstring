
# Changelog

## 2.1.0

### New features
* Add `translate`, used to translate occurrences of certain characters into something else.

### Bugs fixed

* [#5](https://github.com/expez/superstring/issues/5) `strip-accents` translated the letter `ł` to `l` in the cljs version.

## 2.0.0

### New features

* Add Clojurescript support.
* Remove `title-case`, it was duplicating `clojure.string/capitalize`.
* Various docstring improvements.

### Bugs fixed

* [#4](https://github.com/expez/superstring/issues/4) Inconsistency in the `pascal-case` docstring.  Examples were correct, prose was wrong.

# 1.1.0
### New features

* Add `char-at`, which wraps the native String/charAt
* Add `re-quote` which quotes a string, for use in regular expressions.

### Bugs fixed

* `mixed-case?` is now false on strings like "123" where no character has a case.
* `swap-case` on "ß" now returns "SS"
* Fix `starts-with?` throwing exception when `prefix` is longer than `s`.
