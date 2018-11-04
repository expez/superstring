# superstring [![Build Status](https://travis-ci.org/expez/superstring.svg?branch=master)](https://travis-ci.org/expez/superstring)

## Installation

Add the following dependency to your `project.clj` file:

```clj
[superstring "3.0.0"]
```

## Documentation

[API Docs](http://expez.github.io/superstring/doc/superstring.core.html)<br>
<br>
[Live documentation](https://cdn.rawgit.com/expez/superstring/master/live-documentation.html)

## Raison d'être
<img src="https://cloud.githubusercontent.com/assets/1006557/8227518/71776fb2-15a8-11e5-82b5-72e4a5fd4db0.jpg" align="right">
This is a convenience library.  There's nothing in here that you haven't written at least once before, but your time is valuable and you shouldn't have to do it again.

It's also annoying to pull in `$library` as well as `clojure.string`
to get complete coverage.  Or pull in `$library` where the functions in `clojure.string` are renamed, or re-implemented with slightly different semantics.  I've opted to alias all the vars in
`clojure.string` so you only need to require a single namespace and because I know you hate surprises.

A ton of functions, either on `String`, or elsewhere, return `true` as the truthy value.  In a lisp this is a terrible waste.  To afford string punning this library returns `s` as the truthy value so you can do stuff like this:

```clj
(require [superstring.core :as str])

(some-> linkedIn-profile
        (str/includes? "developer" :ignore-case)
        (str/includes-all? ["java" "xml"] :ignore-case)
        (str/includes-any? ["ninja" "rockstar"] :ignore-case)
        extract-contact-info
        send-unsolicited-job-offer)
```

## Quirks

The character `ß` is a lower case, german, character that is shorthand
for `ss`.  This character is, to my knowledge, exceptional in that it
upper cases to a combination of two characters.  Frankly, this
annoying character should be deprecated, but in the meanwhile we
should try to treat it in a consistent manner.

The jvm has the following behavior:

```clj
(Character/toUppercase \ß) ;;=> \ß
(.toUppercase "ß") ;;=> "SS"
(.equalsIgnoreCase "ß" "ss") ;;=> false
```

I'm interpreting this to mean:

1. String comparisons ignoring case should not treat `ß` and `ss` or
`SS` as equal.
2. Any string operations which change the case should consider `SS` to be the upper case variant of `ß`.

## Is it any good?

Yes!

## License

Copyright &copy; 2018 Lars Andersen

This project is licensed under the [Eclipse Public License 1.0][license].

[license]: http://www.eclipse.org/legal/epl-v10.html
