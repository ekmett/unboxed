# unboxed

[![Travis Continuous Integration Status][travis-img]][travis]

This is a small package exploring how to overload Prelude typeclasses to work over multiple `RuntimeRep`s.

The end result is that with enough extensions, including `NoImplicitPrelude`, and `RebindableSyntax`, importing `Unlifted.Prelude` will allow you to work with `Eq`, `Ord`, `Num`, etc. in any `TYPE r`, not just `TYPE 'LiftedRep`. This allows using numeric literals such as 3 in unlifted types such as `Int#`, or `Float#`. If you are working with types of kind Type, this package will delegate to normal Prelude instances, so you don't have to provide duplicate definitions.

In addition to overloading classes, some limited data types are also offered. Notably lists of lifted or unboxed values, and lifted and unlifted Maybes of lifted or unlifted values. When GHC 9.2 lands, hopefully with support for unboxed data types, then an unlifted list should be possible.

The key operation to allow this is `Lev` in `Unlifted.Levitation`. It can be used to adapt _any_ `TYPE r` to `Type` in negative position, by observing that in core, any constrained type is in Type and attaching a trivial constraint.

This allows `ifThenElse` to work under `RebindableSyntax` at all `TYPE r` types, despite levity polymorphism not allowing its use in negative position, _and_ for the `if` to be appropriately lazy.

If you are going to explore the library using the `ghci`, I'd recommend running ghci with `-fno-it` to keep it from trying to bing the last variable, given it can't bind unlifted variables. Running with `-interactive-print print` will use whatever defininition of `print` is in scope, and the one in `Unlifted.Prelude` is sufficiently polymorphic to work with lifted and unlifted values. See the comments in the local `.ghci` file.

There are a lot of classes in `base` that should be ported, and many hands make for light work, so please feel free to pitch in!

License
=======

[Licensed](LICENSE.md) under either of
 * Apache License, Version 2.0 (http://www.apache.org/licenses/LICENSE-2.0)
 * BSD 2-Clause license (https://opensource.org/licenses/BSD-2-Clause)
at your option.

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you shall be dual-licensed as above, without any
additional terms or conditions.

Contact Information
===================

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the `##coda` or `#haskell` IRC channels on `irc.freenode.net`.

-Edward Kmett

 [travis]: http://travis-ci.org/ekmett/linear-primitive
 [travis-img]: https://secure.travis-ci.org/ekmett/linear-primitive.png?branch=master
