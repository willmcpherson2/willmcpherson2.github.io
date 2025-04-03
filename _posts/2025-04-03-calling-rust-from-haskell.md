---
layout: post
title: "Calling Rust from Haskell"
---

| This setup is available as a project template: [hsrs](https://github.com/willmcpherson2/hsrs)

Haskell is a great, but not low-level.
Fortunately, Haskell has a low-overhead FFI which allows you to call C.
Of course, it doesn't have to be C.
Any C ABI will do.

# C ABI

If you want to learn what "C ABI" actually means, I highly recommend Alexis King's answer to the Stack Exchange question ["Why do common Rust packages depend on C code?"](https://langdev.stackexchange.com/a/3237)

But basically, it's the lingua franca.
Rust speaks the lingua franca like this:

[`rs/lib.rs`](https://github.com/willmcpherson2/hsrs/blob/537e437c606fe5c57a4045450bd5bdf4af9e3115/rs/lib.rs)

```rust
#[repr(C)]
#[derive(Debug)]
pub struct Point {
    x: f64,
    y: f64,
}

impl Point {
    #[no_mangle]
    pub extern "C" fn new_point(x: f64, y: f64) -> Box<Point> {
        Box::new(Point { x, y })
    }

    #[no_mangle]
    pub extern "C" fn point_length(&self) -> f64 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }

    #[no_mangle]
    pub extern "C" fn print_point(&self) {
        println!("{:?}", self)
    }
}

#[no_mangle]
pub extern "C" fn free_point(_point: Box<Point>) {}
```

The `free_point` definition is interesting - since we take the `Box<Point>` by value, it is moved into the function and dropped.
So while the function looks redundant, there is a call to [`std::mem::drop`](https://doc.rust-lang.org/std/mem/fn.drop.html).
You can verify that this works if you call it from C and compile with `-fsanitize=leak`.

We need to set up our crate to compile to a static library (`.a`) and a header file (`.h`):

[`Cargo.toml`](https://github.com/willmcpherson2/hsrs/blob/537e437c606fe5c57a4045450bd5bdf4af9e3115/Cargo.toml)

```toml
[package]
name = "hsrs"
version = "0.1.0"
edition = "2021"

[lib]
path = "rs/lib.rs"
crate-type = ["staticlib"]

[build-dependencies]
cbindgen = "0.28.0"
```

We're using [cbindgen](https://github.com/mozilla/cbindgen) to generate header files, which you can run via Rust build script:

[`build.rs`](https://github.com/willmcpherson2/hsrs/blob/537e437c606fe5c57a4045450bd5bdf4af9e3115/build.rs)

```rust
fn main() {
    let crate_name = std::env::var("CARGO_PKG_NAME").unwrap();
    let crate_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let header_path = std::path::PathBuf::from(format!("lib/{}.h", crate_name));

    cbindgen::Builder::new()
        .with_crate(crate_dir)
        .with_language(cbindgen::Language::C)
        .generate()
        .unwrap()
        .write_to_file(header_path);
}
```

Now we can build our library:

```
$ cargo build -Z unstable-options --artifact-dir=lib
   Compiling hsrs v0.1.0 (/home/will/Desktop/hsrs)
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.11s

$ ls lib
hsrs.h  libhsrs.a
```

Perfect.
This can be consumed by anything that speaks the C ABI.
Let's set up our Haskell project to do so.

# Into the burrito

[`hsrs.cabal`](https://github.com/willmcpherson2/hsrs/blob/537e437c606fe5c57a4045450bd5bdf4af9e3115/hsrs.cabal)

```
cabal-version: 3.4
name:          hsrs
version:       0.1.0.0
build-type:    Simple

executable hsrs
  hs-source-dirs:     hs
  main-is:            Main.hs
  other-modules:      Lib
  include-dirs:       lib
  extra-lib-dirs:     lib
  extra-libraries:    hsrs
  default-language:   GHC2024
  build-depends:      base >=4.7 && <5
  build-tool-depends: c2hs:c2hs >=0.28.8
```

The important parts are `include-dirs`, `extra-lib-dirs`, `extra-libraries` and the dependency on [c2hs](https://github.com/haskell/c2hs), which basically does the opposite of cbindgen - consuming C header files.

You will need a bit of interfacing code however:

[`hs/Lib.chs`](https://github.com/willmcpherson2/hsrs/blob/de28e89db03e817c6a02978cce8d029123edf5e7/hs/Lib.chs)

```
module Lib where

import Foreign.Ptr
import Foreign.C.Types

#include "hsrs.h"

{#pointer *Point as PointPtr foreign newtype #}

{#fun new_point as ^ { `Double', `Double' } -> `PointPtr' #}

{#fun point_length as ^ { `PointPtr' } -> `Double' #}

{#fun print_point as ^ { `PointPtr' } -> `()' #}

{#fun free_point as ^ { `PointPtr' } -> `()' #}
```

This isn't a Haskell file - it's a `.chs` file, which c2hs will use to generate marshalling code and [FFI calls](https://en.wikibooks.org/wiki/Haskell/FFI).

We're including the C header file that we generated with cbindgen and defining some bindings to it.
In this case, I'm not defining the fields of the `Point` struct - I'm just defining an opaque pointer.

I find this pattern very useful: you marshall an opaque pointer to an object which has some state in it, and you call functions with it.
This is basically OOP encapsulation - define an object and only access it through methods, because the insides are too hairy to work with directly.
This design makes your FFI layer thin, which means less can go wrong.

Let's call our new FFI functions.

[`hs/Main.hs`](https://github.com/willmcpherson2/hsrs/blob/de28e89db03e817c6a02978cce8d029123edf5e7/hs/Main.hs)

```haskell
module Main where

import Lib

main :: IO ()
main = do
  point <- newPoint 1.5 2.0
  printPoint point
  length <- pointLength point
  putStrLn $ "point length: " <> show length
  freePoint point
```

You can see that `snake_case` became `camelCase`. Other than that, no surprises.

```
$ cabal run
Point { x: 1.5, y: 2.0 }
point length: 2.5
```

Awesome.

This setup is available as a [template on GitHub](https://github.com/willmcpherson2/hsrs). It includes a Nix flake to install everything you need to develop a Haskell + Rust project, including language servers and formatters.
