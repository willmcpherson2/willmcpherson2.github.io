---
layout: post
title: "Inline Your Runtime"
published: false
---

* toc
{:toc}

---

| [github.com/willmcpherson2/inline-your-runtime](https://github.com/willmcpherson2/inline-your-runtime)

> Inlining is the mother of all optimisations

\- Rémi Forax ([earliest source I could find](https://wingolog.org/archives/2011/07/05/v8-a-tale-of-two-compilers#788347f5d21641a7115ba069f58715848dba9850))

# Compiler vs Runtime

In programming language implementation, we often separate the **compiler** from the **runtime**.
This is useful when generating code that isn't affected by the input program.

For example, arithmetic and conditionals require generating very specific instructions, whereas a built-in data structure should be defined in the runtime system.
The compiler can then generate instructions to call into the runtime API.
This is especially pertinent if you're using something like LLVM where generating instructions can be cumbersome.

So basically: if you find yourself defining a function *via code generation*, you should just define that function in a programming language.

This separation of concerns becomes practically unavoidable for high-level languages where you need to define closures, data structures, I/O, etc.

# Link Your Runtime

Ok, so how do we actually use this technique?
The most straightforward solution is to implement the programming language as an executable compiler and a runtime library.
The compiler generates a binary which is then linked against the runtime library.

This is a fine solution, and I actually recommend it over the monstrosity that I'll be describing in this post.
However it's worth understanding some of the downsides of linking your runtime.

By separately compiling the executable from its runtime, we miss out on a lot of the optimisations available in LLVM.
While the executable and the runtime can be optimised separately, information is lost in the boundary.

This is particularly problematic because this is the *runtime of a programming language*.
Micro-optimisations actually matter here - a 1% improvement is a 1% improvement for every program.

# Inline Your Runtime

Basically we want something [Link Time Optimisation](https://llvm.org/docs/LinkTimeOptimization.html) (LTO), which can optimise across module boundaries.

LTO is a bit confusing and I don't understand it.
But basically, instead of linking object files, you tell the compiler (e.g. Clang) to emit something it can actually optimise (e.g. LLVM bitcode).
This enables whole-program optimisation.

But how do we do that with our executable and runtime?
Well we could compile our runtime to LLVM bitcode ahead of time, compile the input program to LLVM bitcode and then link them.
For example if `rts.bc` is our runtime library and `output.bc` is our generated code:

```
$ llvm-link rts.bc output.bc -o main.bc
$ opt main.bc -o main.bc
$ llc --filetype=obj main.bc -o main.o
$ cc main.o -o main
```

Our compiler calls some LLVM APIs to link the bitcode, optimise it and compile it to an object file.
Then we invoke the system compiler to link to an executable.

Or in the case of JIT compiling:

```
$ llvm-link rts.bc output.bc -o main.bc
$ opt main.bc -o main.bc
$ lli main.bc
```

# Implementation

To properly implement this in a compiler, we can do the following:

- Build a runtime library
  - Use a Rust toolchain to compile the library to LLVM bitcode
- Build a compiler
  - Include the runtime system bitcode in the compiler
  - Link the compiler to the same LLVM version as the runtime library
  - Use the Rust toolchain to compile the compiler to a native executable

## Toolchain

First we need a development environment which provides LLVM libraries for our compiler, and also a Rust toolchain that uses the same LLVM version.

[`flake.nix`](https://github.com/willmcpherson2/inline-your-runtime/blob/3a27713dddb4cebad4f6cf9fd9b0dbffda49419d/flake.nix)

```nix
{
  description = "Rust + LLVM";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      rust-overlay,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
        llvm = pkgs.llvmPackages_18.llvm;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            llvm
            pkgs.valgrind
            pkgs.libffi
            pkgs.libxml2
            (pkgs.rust-bin.nightly."2024-07-31".default.override {
              extensions = [
                "rust-src"
                "rust-analyzer-preview"
                "miri"
              ];
            })
          ];
        };
      }
    );
}
```

I chose LLVM 18 because it's supported by the [Inkwell crate](https://github.com/TheDan64/inkwell) which provides high-level Rust bindings to LLVM.

Now we can verify that our LLVM versions match:

```
$ llvm-config --version
18.1.8

$ rustc -vV
rustc 1.82.0-nightly (f8060d282 2024-07-30)
binary: rustc
commit-hash: f8060d282d42770fadd73905e3eefb85660d3278
commit-date: 2024-07-30
host: x86_64-unknown-linux-gnu
release: 1.82.0-nightly
LLVM version: 18.1.7
```

Close enough.

To find a Rust version that uses a certain LLVM version, you can trawl through the [Rust changelog](https://raw.githubusercontent.com/rust-lang/rust/master/RELEASES.md).

## Runtime

The next step is to build the runtime library.

[`rts/src/lib.rs`](https://github.com/willmcpherson2/inline-your-runtime/blob/3a27713dddb4cebad4f6cf9fd9b0dbffda49419d/rts/src/lib.rs)

```rust
#![no_std]
#![allow(internal_features)]
#![feature(rustc_attrs, linkage)]

extern crate alloc;

use alloc::vec;
use alloc::{
    alloc::{GlobalAlloc, Layout},
    boxed::Box,
    vec::Vec,
};
use libc::{abort, aligned_alloc, c_void, free};

#[allow(unused_imports)]
use core::panic::PanicInfo;

struct Allocator;

unsafe impl GlobalAlloc for Allocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        unsafe { aligned_alloc(layout.align(), layout.size()) as *mut u8 }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        unsafe { free(ptr as *mut c_void) };
    }
}

#[global_allocator]
static GLOBAL: Allocator = Allocator;

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    unsafe { abort() }
}

#[rustc_std_internal_symbol]
#[linkage = "weak"]
fn __rust_alloc_error_handler(_size: usize, _align: usize) -> ! {
    unsafe { abort() }
}

#[rustc_std_internal_symbol]
#[linkage = "weak"]
#[allow(non_upper_case_globals)]
static __rust_no_alloc_shim_is_unstable: u8 = 0;

pub struct Foo {
    numbers: Vec<i32>,
}

impl Foo {
    #[no_mangle]
    pub extern "C" fn new_foo() -> Box<Foo> {
        Box::new(Foo {
            numbers: vec![1, 2, 3],
        })
    }

    #[no_mangle]
    pub extern "C" fn sum_foo(self: &Foo) -> i32 {
        self.numbers.iter().sum()
    }

    #[no_mangle]
    pub extern "C" fn free_foo(_foo: Box<Foo>) {}
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_rts() {
        let foo = Foo::new_foo();
        let result = foo.sum_foo();
        assert_eq!(result, 6);
        Foo::free_foo(foo);
    }
}
```

This is a pretty typical `no_std` library that just depends on [libc](https://langdev.stackexchange.com/questions/3233/why-do-common-rust-packages-depend-on-c-code/3237#3237).
The only quirk is that we need to define [`__rust_alloc_error_handler`](https://stdrs.dev/nightly/x86_64-unknown-linux-gnu/alloc/alloc/fn.__rust_alloc_error_handler.html) and [`__rust_no_alloc_shim_is_unstable`](https://stdrs.dev/nightly/x86_64-unknown-linux-gnu/alloc/alloc/static.__rust_no_alloc_shim_is_unstable.html), which are special symbols inserted by the Rust compiler which are mysteriously *not* inserted when compiling to LLVM bitcode.

Our runtime system provides a simple API for allocating, summing and freeing a `Foo`.
Imagine this is a very important built-in data structure in our language.

Verifying the correctness of the runtime system is extremely important.
Any bug or vulnerability in this system compromises the security of every program in the language.
Here we're using both of the age-old techniques for program verification:

**Types**: by providing methods on an opaque pointer, we can write safe Rust code.
You don't have to write your runtime in C.

**Tests**: because our runtime is just a standalone library, we can develop a comprehensive test suite.
We can use the [Miri interpreter](https://github.com/rust-lang/miri) to verify the safety of any low-level code:

```
$ cargo miri test -p rts
   Compiling rts v0.1.0 (/home/will/Desktop/inline-your-runtime/rts)
    Finished `test` profile [unoptimized + debuginfo] target(s) in 0.06s
     Running unittests src/lib.rs (target/miri/x86_64-unknown-linux-gnu/debug/deps/rts-b728a07dc5e60d37)

running 1 test
test test::test_rts ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.13s

   Doc-tests rts

running 0 tests

test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s
```

Now we can build the runtime library for consumption in the compiler.

[`Makefile`](https://github.com/willmcpherson2/inline-your-runtime/blob/3a27713dddb4cebad4f6cf9fd9b0dbffda49419d/Makefile)

```
ARCH=x86_64-unknown-linux-gnu

.PHONY: debug
debug:
	$(MAKE) build DIR=debug PROFILE=dev

.PHONY: release
release:
	$(MAKE) build DIR=release PROFILE=release

.PHONY: build
build:
	RUSTFLAGS="--emit=llvm-bc" cargo build \
		--package rts \
		--profile $(PROFILE) \
		--target $(ARCH) \
		-Z build-std=core,compiler_builtins,alloc

	llvm-link \
		target/$(ARCH)/$(DIR)/deps/core-*.bc \
		target/$(ARCH)/$(DIR)/deps/compiler_builtins-*.bc \
		target/$(ARCH)/$(DIR)/deps/alloc-*.bc \
		target/$(ARCH)/$(DIR)/deps/libc-*.bc \
		target/$(ARCH)/$(DIR)/deps/rts-*.bc \
		-o target/rts.bc

	opt \
		--internalize-public-api-list="new_foo,sum_foo,free_foo" \
		--passes="internalize,globaldce" \
		target/rts.bc \
		-o target/rts.bc

	opt \
		--passes="internalize" \
		target/rts.bc \
		-o target/rts.bc
```

Basically we do the following:

- Compile the library and its dependencies to LLVM bitcode
- Link the LLVM modules
- Perform dead-code elimination using [internalize](https://llvm.org/docs/Passes.html#internalize-internalize-global-symbols) and [globaldce](https://llvm.org/docs/Passes.html#globaldce-dead-global-elimination)
- Perform a final internalize pass

The dead-code elimination is important because without it, the final module includes the code of all its dependencies.
Also, the size of the runtime module will affect the performance of our compiler.

Let's check that we have all our symbols:

```
$ llvm-nm target/rts.bc
---------------- t
---------------- t .Ltmp0
---------------- t .Ltmp1
---------------- t _ZN5alloc5alloc18handle_alloc_error17he8ceb1c27494fe14E
---------------- t __rust_alloc_error_handler
---------------- d __rust_no_alloc_shim_is_unstable
---------------- T __rust_probestack
                 U abort
                 U aligned_alloc
                 U free
---------------- T free_foo
---------------- T new_foo
---------------- T sum_foo
```

Perfect - the only undefined symbols are the C functions we're calling.
Note that this is for the `release` profile - the `dev` profile will create a much larger module.

## Compiler

Finally we can write our compiler.

[`compiler/src/main.rs`](https://github.com/willmcpherson2/inline-your-runtime/blob/3a27713dddb4cebad4f6cf9fd9b0dbffda49419d/compiler/src/main.rs)

```rust
use inkwell::{
    context::Context,
    memory_buffer::MemoryBuffer,
    module::Module,
    passes::PassBuilderOptions,
    targets::{FileType, InitializationConfig, Target, TargetMachine, TargetMachineOptions},
    OptimizationLevel,
};
use std::{env::args, path::Path, process::exit};

const RTS_BC: &[u8] = include_bytes!("../../target/rts.bc");

fn main() {
    Target::initialize_all(&InitializationConfig::default());
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).unwrap();
    let options = TargetMachineOptions::new().set_level(OptimizationLevel::None);
    let machine = target
        .create_target_machine_from_options(&triple, options)
        .unwrap();

    let context = Context::create();
    let buffer = MemoryBuffer::create_from_memory_range(RTS_BC, "main");
    let module = Module::parse_bitcode_from_buffer(&buffer, &context).unwrap();
    let builder = context.create_builder();

    let main_fun_type = context.i32_type().fn_type(&[], false);
    let main_fun = module.add_function("main", main_fun_type, None);

    let block = context.append_basic_block(main_fun, "start");
    builder.position_at_end(block);

    let new_foo = module.get_function("new_foo").unwrap();
    let foo = builder
        .build_call(new_foo, &[], "foo")
        .unwrap()
        .try_as_basic_value()
        .unwrap_left()
        .into_pointer_value();

    let sum_foo = module.get_function("sum_foo").unwrap();
    let result = builder
        .build_call(sum_foo, &[foo.into()], "result")
        .unwrap()
        .try_as_basic_value()
        .unwrap_left()
        .into_int_value();

    let free_foo = module.get_function("free_foo").unwrap();
    builder.build_call(free_foo, &[foo.into()], "").unwrap();

    builder.build_return(Some(&result)).unwrap();

    module
        .run_passes("default<O3>", &machine, PassBuilderOptions::create())
        .unwrap();

    module.verify().unwrap();

    let eval = args().any(|arg| arg == "-e");
    if eval {
        let engine = module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .unwrap();
        let code = unsafe { engine.run_function_as_main(main_fun, &[]) };
        exit(code)
    } else {
        machine
            .write_to_file(&module, FileType::Object, Path::new("main.o"))
            .unwrap();
    }
}
```

The compiler is the easy part.

- Include the runtime bitcode directly in the executable
- Load the runtime bitcode to create a `main` module
- Define a `main` function which calls our runtime API
- Run some optimisation passes
- Verify the module
- Either run the JIT or compile to an object file

Let's JIT:

```
$ ./target/debug/compiler -e
$ echo $?
6
```

Success! We got our `1 + 2 + 3`.

Let's compile:

```
$ ./target/debug/compiler
$ cc main.o -o main
$ ./main
$ echo $?
6
```
