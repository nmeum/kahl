# kahl

Implementation of [BARE][bare web] in R⁷RS scheme.

## Status

Work in progress, not really usable currently. Features implemented:

* [x] uint
* [x] int
* [x] u8, u16, u32, u64
* [x] i8, i16, i32, i64
* [ ] f32, f64
* [x] bool
* [ ] enum
* [x] string
* [x] data<length>
* [x] data
* [ ] void
* [x] optional<type>
* [x] [length]type
* [x] []type
* [x] map[type A]type B
* [x] (type | type | ...)
* [x] struct

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
