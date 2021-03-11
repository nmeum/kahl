# kahl

R⁷RS Scheme parser combinator library for decoding [BARE][bare web] messages.

## Status

With the exception of floating-point numbers (`f32` and `f64`), all
types from [`draft-devault-bare-01`][draft-devault-bare-01] are
supported. The BARE schema language is currently not supported, but may
be supported in future version of this library. Additionally, this
hasn't been tested extensively and the API may still change.

## Installation

Depends on your Scheme implementation. I have tested this with
[CHICKEN Scheme][chicken scheme] and [chibi-scheme][chibi github].
For simple experiments, simply use the following commands:

	$ git clone https://github.com/nmeum/kahl
	$ cd kahl
	$ chibi-scheme
	> (import (kahl))
	> (parse (parse-struct parse-u8 parse-u8) #u8(23 42))
	#(23 42)

Tests require [`(chibi test)`][chibi test] and can be run as follows:

	$ ./run-tests.scm

## Extended Example

Consider the following [BARE schema][bare schema] definition:

	type Customer {
		name: string
		email: string
		orders: []{
			orderId: i64
			quantity: i32
		}
	}

Messages of this type can be parsed using the following Scheme code:
This can be parsed using the following Scheme code:

	(import (kahl))

	;; Define a parser for the Customer type.
	(define parse-customer
	  (parse-struct
	    parse-string ;; name
	    parse-string ;; email
	    (parse-list  ;; orders
	      (parse-struct
	        parse-i64
	        parse-i32))))

	;; Create a parse stream for the file customer.bin
	;; and use the parse-customer parser to parse it.
	(let ((s make-parse-stream "customer.bin"))
	  (parse parse-customer s))

Refer to the documentation for more information on individual procedures.

## Documentation

This library is documented using Scheme Scribble syntax as implemented by
[chibi scribble][chibi scribble]. Documentation can be generated using
`chibi-doc(1)`. To generate HTML documentation run:

	$ chibi-doc kahl > kahl.html

If you want to improve the existing documentation, take a look at the
[Scribble quick start guide][racket scribble]. Commands supported in the
default Chibi-Scheme doc environment can be obtained by importing
[`(chibi doc)`][chibi doc] and running `(make-default-doc-env)`.

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
[draft-devault-bare-01]: https://datatracker.ietf.org/doc/html/draft-devault-bare-01
[bare schema]: https://datatracker.ietf.org/doc/html/draft-devault-bare-01#section-3
[langsec web]: https://langsec.org/
[bratus parser]: https://www.usenix.org/publications/login/spring2017/bratus
[chibi parse]: https://synthcode.com/scheme/chibi/lib/chibi/parse.html
[chibi test]: https://synthcode.com/scheme/chibi/lib/chibi/test.html
[chibi scribble]: https://synthcode.com/scheme/chibi/lib/chibi/scribble.html
[chibi doc]: https://synthcode.com/scheme/chibi/lib/chibi/doc.html
[racket scribble]: https://docs.racket-lang.org/scribble/getting-started.html
[chicken scheme]: https://call-cc.org
[chibi github]: https://github.com/ashinn/chibi-scheme
