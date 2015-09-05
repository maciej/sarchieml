# SArchieML
[![Build Status](https://travis-ci.org/maciej/sarchieml.svg)](https://travis-ci.org/maciej/sarchieml)

A Scala parser for [ArchieML](http://archieml.org).

Uses [fastparse](http://lihaoyi.github.io/fastparse/) parser generator and produces a [Spray Json](https://github.com/spray/spray-json) `JsObject`.

## Getting started
In SBT add:
```scala
libraryDependencies += "me.maciejb" %% "sarchieml" % "0.1.0"
```

## Usage
```scala
import me.maciejb.sarchieml.ArchiemlParser
ArchiemlParser.parse("key: value")
```

## Resources
* [Specification](http://archieml.org/spec/1.0/CR-20150509.html)
* [Existing ArchieML Parsers](http://archieml.org/#resources)
* [archieclj - ArchieML clojure parser](https://github.com/mihi-tr/archieclj)
