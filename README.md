# str [![Build Status](https://travis-ci.org/xsc/lein-ancient.svg?branch=master)](https://travis-ci.org/expez/str)

Clojure string library

## Installation

Add the following dependency to your `project.clj` file:

```clj
[str "1.0.0"]
```

## Documentation

[API Docs](http://expez.github.io/str/doc/str.core.html)

## Raison d'être

This is a convenience library.  There's nothing in here that you haven't written at least once before, but your time is valuable and you shouldn't have to do it again.

It's also annoying to pull in `$library` as well as `clojure.string`
to get complete coverage.  Or pull in `$library` where the functions in `clojure.string` are renamed, or re-implemented with slightly different semantics.  I've opted to alias all the vars in
`clojure.string` so you only need to require a single namespace and because I know you hate surprises.

A ton of functions, either on `String`, or elsewhere, return `true` as the truthy value.  In a lisp this is a terrible a waste.  To afford string punning this library returns `s` as the truthy value so you can do stuff like this:

```clj
(some-> linked-in-profile
        (str/contains? "developer" :ignore-case)
        (str/any? ["ninja" "rockstar"] :ignore-case)
        extract-contact-info
        send-random-job-offer)
```

## License

Copyright &copy; 2015 Lars Andersen

This project is licensed under the [Eclipse Public License 1.0][license].

[license]: http://www.eclipse.org/legal/epl-v10.html
