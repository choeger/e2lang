e2lang
======

e2l is the **e** mbedded **e** quation **l** anguage. 

It's purpose is to provide a simple language for automatic differentiation and JIT code generation in variable-structure DAE simulation environments.

Dependencies
------------
 
 * oasis build manager (opam package)
 * libtnp (https://github.com/AMSUN-Berlin/libtnp)
 * kaputt (opam package) 
 * core (opam package)
 * core_bench (opam package)
 * smart_print (opam package)

You can install the OCaml dependencies running:

```sh
  opam install core core_bench smart_print kaputt oasis
```

Building
--------

Run the following commands:

```sh
  oasis setup
  ./configure --enable-tests
  make
```

When libtnp is installed in a non-standard location, you have to set the environment variable LIBRARY_PATH accordingly:

```sh
  oasis setup
  ./configure --enable-tests
  LIBRARY_PATH=$HOME/local/lib make
```

Testing
-------

After building the tests are run via (assuming native compilation is supported on your platform)

```sh
  ./e2test.native
```


