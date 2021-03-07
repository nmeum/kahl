# kahl

Implementation of [BARE][bare web] in R⁷RS scheme.

## Motivation

This started us as a nice demo for a binary parser combinator library
based on [`(chibi parse)`][chibi parse] which I had lying around from a
previous Scheme project I never finished. By providing parser
combinators for BARE, this library allows application developers to
follow [langsec][langsec web] patterns for [secure input handling][bratus parser].

## Status

Except floating-point numbers (`f32` and `f64`), all types from
[`draft-devault-bare-01`][draft-devault-bare-01] are supported
currently. The BARE schema language is currently not supported, but may
be supported in future version of this library. Additionally, this
hasn't been tested extensively yet and the API may still be subject to
change.

## Documentation

This file is documented using Scheme Scribble syntax as implemented by
[chibi scribble][chibi scribble]. Documentation can be generated using
`chibi-doc(1)`. To generate HTML documentation run:

	$ chibi-doc kahl > kahl.html

If you want to improve the existing documentation, take a look at the
[Scribble quick start guide][racket scribble]. Commands supported in the
default Chibi-Scheme doc environment can be obtained by importing
[`(chibi doc)`][chibi doc] and running `(make-default-doc-env)`.

## Limitations

* Since this is a parser combinator library it is not possible to
  satisfy BARE invariants regarding types during parser construction.
  For instance, it cannot be guaranteed that only primitive non-data
  types are used as map keys. This is the callers responsibility.

## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.

[bare web]: https://baremessages.org/
[chibi parse]: https://synthcode.com/scheme/chibi/lib/chibi/parse.html
[draft-devault-bare-01]: https://datatracker.ietf.org/doc/html/draft-devault-bare-01
[langsec web]: https://langsec.org/
[bratus parser]: https://www.usenix.org/publications/login/spring2017/bratus
[chibi scribble]: https://synthcode.com/scheme/chibi/lib/chibi/scribble.html
[racket scribble]: https://docs.racket-lang.org/scribble/getting-started.html
[chibi doc]: https://synthcode.com/scheme/chibi/lib/chibi/doc.html
