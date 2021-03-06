#!/bin/bash

classpath="target/scala-2.13/classes:${HOME}/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.13.0.jar"
main="tel.schich.steganograph.KernelConvolver"

sbt compile

mkdir "in" 2>/dev/null
mkdir "out" 2>/dev/null

for f in "in/"*
do
    java -cp "$classpath" "$main" "$f" "out/$(basename "$f").png" "${@}"
done
