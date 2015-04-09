# str

Clojure string library

## Usage

### To shorter string

* [chomp](#chomp) `(s)` `(s separator)`
* [chop](#chop) `(s)`

### Predicates

* [ends-with?](#ends-with?) `(s suffix)`
* [starts-with?](#starts-with?) `(s prefix)` `(s prefix ignore-case)`

### Misc

* [capitalize](#capitalize) `(s)`
* [invert-case](#invert-case) `(s)`

#### chomp `(s)` `(s separator)`

Return a new string with the given record separator removed from the end of the string (if present).

Separator defaults to line.separator.

#### chop `(s)`

Return a new string with the last character removed. If the string
ends with \\r\\n, both characters are removed. Applying chop to an
empty string is a no-op.

#### starts-with? `(s prefix)`

Returns `s` if `s` starts with `prefix`.

If a third argument is provided the string comparison is insensitive to case.

#### ends-with? `(s suffix)`

Returns `s` if `s` ends with `suffix`.

#### capitalize `(s)`

Return a new string where the first character is
in upper case and all others in lower case.  ## License

#### invert-case `(s)`

Change lower case characters to upper case and vice versa.

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
