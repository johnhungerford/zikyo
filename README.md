# ZiKyo

### Introduction

[![Build Status](https://github.com/johnhungerford/zikyo/workflows/test/badge.svg)](https://github.com/johnhungerford/zikyo/actions)
[![Version](https://img.shields.io/maven-central/v/io.github.johnhungerford/zikyo-core_3)](https://search.maven.org/search?q=g:io.github.johnhungerford%20a:zikyo)

ZiKyo provides a ZIO-like API for the algebraic effects library [Kyo](https://getkyo.io), allowing users who are used to ZIO to construct and manipulate Kyo effects in a more familiar way.

For ZIO users, Kyo's API can be frustrating for three reasons:
1. Kyo provides a minimal API by design. 

While its uncluttered namespaces make it more approachable for beginners, users who are addicted to ZIO's powerful and intuitive combinators will likely find it unwieldy and possibly not worth the effort.

2. Kyo effects are handled by functions that take effects as arguments, rather by methods on effects.

ZIO users are used to having a large menu of combinators on `ZIO` values that can be chained to fluently manipulate effects. Kyo, by contrast, requires nesting effects within method calls, inverting the order in which users handle effects and requiring them either to create deeply nested expressions or to break expressions up into many intermediate expressions.

3. Factory methods are distributed among different objects

Being more modular that ZIO, Kyo segregates its effect types more cleanly than ZIO and places its effect constructors in companion objects to their corresponding types. This is not a problem given the minimal API that Kyo offers, but ZIO users will miss typing `ZIO.` and seeing a rich menu of factory methods pop up on their IDE.

---

ZiKyo answers these frustrations by providing:
1. A single object `KYO` with a bunch of factory methods for many of the core Kyo effect types styled after the ones that can be found on `ZIO`
2. Extension methods on Kyo effects modeled on ZIO combinators.

Whenever possible the names of ZiKyo methods are the same as the corresponding methods in ZIO. When this is not possible or doesn't make sense, ZiKyo tries to keep as close to ZIO conventions as possible.

### Getting Started

Add the following line to build.sbt:

```sbt
libraryDependencies += "io.github.johnhungerford" %% "zikyo-core" % "<version>"
```

To use the zikyo API, add the following import statement

```scala 
import zikyo.*
```

This will bring into scope `KYO` and all of zikyo's extensions to `A < S`. The following example illustrates the usage of some of these:

```scala 3
import kyo.*
import zikyo.*
import scala.concurrent.duration.*

trait HelloService:
	def sayHelloTo(saluee: String): Unit < (IOs & Aborts[Throwable])

object HelloService:
    object Live extends HelloService:
        override def sayHelloTo(saluee: String): Unit < (Consoles & Aborts[Throwable]) =
            KYO.attempt {                           // Adds Aborts[Throwable] effect
                Consoles.println(s"Hello $saluee!") // Adds Consoles effect
			}

val keepTicking: Nothing < (Consoles & Fibers) =
	(Consoles.print(".") *> KYO.sleep(1.second)).forever

val effect: Unit < (Consoles & Fibers & Resources & Aborts[Throwable] & Envs[NameService]) = for {
    nameService <- KYO.service[NameService]       // Adds Envs[NameService] effect
    _           <- keepTicking.forkScoped         // Adds Consoles, Fibers, and Resources effects
    saluee      <- Consoles.readln                // Uses Consoles effect
    _           <- KYO.sleep(2.seconds)           // Uses Fibers (semantic blocking)
    _           <- nameService.sayHelloTo(saluee) // Adds Aborts[Throwable] effect
} yield ()

// There are no combinators for handling IOs or blocking Fibers, since this should
// be done at the edge of the program
IOs.run {                                                 // Handles IOs
    Fibers.runAndBlock(Duration.Inf) {                    // Handles Fibers
        KYO.scoped {                                      // Handles Resources
            effect
              .provideAs[HelloService](HelloService.Live) // Handles Envs[HelloService]
              .catchAborts((thr: Throwable) => {          // Handles Aborts[Throwable]
                  KYO.debug(s"Failed printing to console: ${throwable}")
              })
              .provideDefaultConsole                      // Handles Consoles
        }
    }
}
```

### Failure conversions

One notable departure from the ZIO API worth calling out is a set of combinators for converting between failure effects. Whereas ZIO has a single channel for describing errors, Kyo has at least three different effect types that can describe failure in the basic sense of "short-circuiting": `Aborts`, `Options`, and `Choices` (an empty `Seq` being equivalent to a short-circuit). It's useful to be able to move between these effects easily, so ZiKyo provides a number of extension methods, usually in the form of `def effect1ToEffect2`.

Some examples:

```scala 3
val abortsEffect: Int < Aborts[String] = ???

// Converts failures to Options.empty
val optionsEffect: Int < Options = abortsEffect.abortsToOptions

// Converts option to Seq of length 1
val seqsEffect: Int < Choices = optionsEffect.optionsToChoices

// Fails with Nil#head exception if empty and succeeds with Seq.head if non-empty
val newAbortsEffect: Int < Aborts[Throwable] = seqsEffect.choicesToThrowable

// Throws a throwable Aborts failure
val unsafeEffect: Int < Any = newAbortsEffect.implicitThrowable

// Catch any thrown exceptions
val safeEffect: Int < Aborts[Throwable] = unsafeEffect.explicitThrowable
```

### Dependency injection

Kyo does not yet have a `provide` macro that can reliably construct a dependency graph from a collection of constructors like ZIO does. Kyo's `.provide` will provide only a single dependency, removing from the `Envs` intersection. It must be called multiple times to handle all the dependencies. Use `provideAs` to ensure the type of the dependency is widened to the required type when providing an implementation of an abstract service.

## Acknowledgements

Thanks, of course, to Kyo's author Flavio Brasil, as well as to the small but growing crew of contributors to kyo. It's really exciting to see this new approach to effects in Scala take shape! 

Much love to the ZIO contributors who have set the standard for effect system usability. Pretty much everything in this library has been lifted from ZIO's API.


License
-------

See the [LICENSE](https://github.com/getkyo/kyo/blob/master/LICENSE.txt) file for details.
