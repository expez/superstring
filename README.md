# str

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

Copyright (c)  2015, Lars Andersen

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
