scalan-starter
==============

In order to build scalan-starter first you need:
 - clone, build and `publishLocal`
`scalan-develop` branch of https://github.com/scalan/virtualization-lms-core.
 - clone, build and `publishLocal` https://github.com/scalan/scalan-ce
 - Setup VM Options in run configuration  
     `-Dscalan.plugins.extraClassPath=<your-home-dir>/.ivy2/cache/org.luaj/luaj-jse/jars/luaj-jse-3.0.1.jar:<your-cloned-scalan-dir>/lua-backend/core/target/scala-2.11/classes:<your-cloned-scalan-dir>/lms-backend/core/target/scala-2.11/classes:<your-home-dir>/.ivy2/cache/org.scala-lang.virtualized/scala-library/jars/scala-library-2.11.2.jar:<your-home-dir>/.ivy2/cache/org.scala-lang.virtualized/scala-compiler/jars/scala-compiler-2.11.2.jar:<your-home-dir>/.ivy2/local/org.scala-lang.lms/lms-core_2.11/0.9.1-SNAPSHOT/jars/lms-core_2.11.jar` 
     
