fuuid4
======

Fortran module to generate random (version 4) Universally Unique
Identifiers (UUIDs). Fuuid4 can either wrap the [libuuid][libuuid]
generator, or use the bundled [Mersenne Twister][mt] psuedo-random
number generator (PRNG) to generate UUIDs itself.

Requirements
------------

Fuuid4 requires a Fortran 2008 compliant compiler and, optionally,
libuuid.

Installation
------------

There are a few ways you can use fuuid4 in your project.

### Single file inclusion

Perhaps the easiest way is to copy `src/fuuid4.f90` into your project and build
it as part of your package. The downside to this approach is that you will have
to apply any updates manually.

There is a single point of configuration: the preprocessor macro
`FUUID4_HAS_LIBUUID`. If this is defined to be `1`, then fuuid4 must be linked
against `libuuid`. If it is `0` or undefined, then fuuid4 uses its internal PRNG
instead.

### External library

You can build fuuid4 into a library using CMake:

```bash
$ cmake . -B build
$ cmake --build build
```

You can then use the built libraries under `build/lib` and the `.mod` file under
`build/mod`. You can install these somewhere convenient with:

```bash
$ cmake . -B build -DCMAKE_INSTALL_PREFIX=/path/to/install
$ cmake --build build --target install
```

fuuid4 installs configuration files for CMake, so you can include fuuid4 in
your CMake project with:

```cmake
find_package(fuuid4)

target_link_libraries(<your target> PRIVATE fuuid4::fuuid4)
```

You can point CMake at either the install location or the build directory
directly with `-Dfuuid4_ROOT=/path/to/install/or/build/dir`

### Internal target in CMake

You can use fuuid4 directly in a CMake project with `FetchContent` like so:

```cmake
include(FetchContent)

FetchContent_declare(fuuid4
    GIT_REPOSITORY https://github.com/PlasmaFAIR/fuuid4.git
    GIT_TAG v0.1.0)
FetchContent_makeavailable(fuuid4)

target_link_libraries(<your target> PRIVATE fuuid4::fuuid4)
```

[libuuid]: https://linux.die.net/man/3/libuuid
[mt]: https://en.wikipedia.org/wiki/Mersenne_Twister
