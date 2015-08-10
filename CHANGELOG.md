
# Changelog

## master (unreleased)

### New features

* clojurescript support

### Bugs fixed

# 1.1.0
### New features

* Add `char-at`, which wraps the native String/charAt
* Add `re-quote` which quotes a string, for use in regular expressions.

### Bugs fixed

* `mixed-case?` is now false on strings like "123" where no character has a case.
* `swap-case` on "ß" now returns "SS"
* Fix `starts-with?` throwing exception when `prefix` is longer than `s`.
