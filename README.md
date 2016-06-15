scalan-starter
==============

Scalan-starter is a simple base project to start working with [Scalan](https://github.com/scalan/scalan) 
(see its README for more information).

Because there is currently no published version of Scalan, to build scalan-starter you first need to:
 1. Clone or download [LMS](https://github.com/scalan/virtualization-lms-core) (`git clone https://github.com/scalan/virtualization-lms-core.git` from command line).
 2. Run `sbt publishLocal` in the directory it was cloned to.
 3. Clone or download [Scalan](https://github.com/scalan/scalan) (`git clone https://github.com/scalan/scalan.git`).
 4. Similarly run `sbt publishLocal`.
 5. Previous step produces file
 `examples/target/scala-2.11/src_managed/main/buildinfo/application.scala` in `scalan` directory.
 6. Copy value of `extraClassPath` from there (including quotes) into
 `scalan-starter-core/src/main/resources/application.conf` under key `scalan.plugins.extraClassPath` in this directory (see `reference.conf` files in `scalan` for other configuration options, which you may also want to change).

You should now be able to run `sbt test` to check that demonstration examples. You will see messages like
`error: illegal sharing of mutable objects ...`, which can be ignored.

Now you can try writing your own Scalan DSLs and kernels and running them.
