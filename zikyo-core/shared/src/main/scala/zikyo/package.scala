import kyo.*
import kyo.Aborts.HasAborts
import kyo.Envs.HasEnvs
import scala.annotation.implicitNotFound
import scala.annotation.tailrec
import scala.annotation.targetName
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

package object zikyo:
    object KYO:
        def acquireRelease[A, S](acquire: => A < S)(release: A => Unit < IOs): A < (S & Resources) =
            acquire.map(a => Resources.ensure(release(a)).as(a))

        def addFinalizer(finalizer: => Unit < IOs): Unit < Resources =
            Resources.ensure(finalizer)

        def async[A](register: (A < Fibers => Unit) => Unit < Fibers)(
            using Flat[A < Fibers]
        ): A < Fibers =
            for
                promise <- Fibers.initPromise[A]
                registerFn = (eff: A < Fibers) =>
                    val effFiber = Fibers.init(eff)
                    val updatePromise =
                        effFiber.map(_.onComplete(a => promise.complete(a).unit))
                    val updatePromiseIO = Fibers.init(updatePromise).unit
                    IOs.run(updatePromiseIO)
                _ <- register(registerFn)
                a <- promise.get
            yield a

        def attempt[A, S](effect: => A < S): A < (S & Aborts[Throwable]) =
            Aborts[Throwable].catching(effect)

        def collect[A, S, A1, S1](
            sequence: => Seq[A] < S
        )(
            useElement: PartialFunction[A, A1 < (Seqs & S1)]
        )(using Flat[A1 < (Seqs & S & S1)]): Seq[A1] < (S & S1) =
            Seqs.run(Seqs.get(sequence).map(a => useElement.applyOrElse(a, _ => Seqs.drop)))

        def debug[S](message: => String < S): Unit < (S & IOs) =
            message.map(m => Console.default.println(m))

        def dropSeqs: Nothing < Seqs = Seqs.drop

        def fail[E, S](error: => E < S)(using Tag[Aborts[E]]): Nothing < (S & Aborts[E]) =
            error.map(e => Aborts[E].fail(e))

        def foreach[A, S, A1, S1](
            sequence: => Seq[A] < S
        )(
            useElement: A => A1 < (S1 & Seqs)
        )(using Flat[A1 < (Seqs & S & S1)]): Seq[A1] < (S & S1) =
            Seqs.run(Seqs.get(sequence).map(useElement))

        def foreachDiscard[A, S, S1](
            sequence: => Seq[A] < S
        )(
            useElement: A => Any < S1
        )(using Flat[Any < (Seqs & S & S1)]): Unit < (S & S1) =
            foreach(sequence)(useElement).unit

        def foreachPar[A, S, A1](
            sequence: => Seq[A] < S
        )(
            useElement: A => A1 < Fibers
        )(using Flat[A1 < Fibers]): Seq[A1] < (S & Fibers) =
            sequence.map(seq => Fibers.parallel(seq.map(useElement)))

        def foreachParDiscard[A, S, Any](
            sequence: => Seq[A] < S
        )(
            useElement: A => Any < Fibers
        )(using Flat[Any < Fibers]): Unit < (S & Fibers) =
            sequence.map(seq => Fibers.parallel(seq.map(v => useElement(v).unit))).unit

        def fromAutoCloseable[A <: AutoCloseable, S](closeable: => A < S): A < (S & Resources) =
            acquireRelease(closeable)(c => IOs(c.close()))

        def fromEither[E, A](either: Either[E, A])(using Tag[Aborts[E]]): A < (Aborts[E]) =
            val aborts = Aborts[E]
            println("HI")
            aborts.get(either)
        end fromEither

        def fromFuture[A: Flat, S](future: => Future[A] < S): A < (S & Fibers) =
            future.map(f => Fibers.fromFuture(f))

        def fromPromiseScala[A: Flat, S](promise: => scala.concurrent.Promise[A] < S)
            : A < (S & Fibers) =
            promise.map(p => fromFuture(p.future))

        def fromOption[A, S](option: => Option[A] < S): A < (S & Options) =
            Options.get(option)

        def fromSeq[A, S](sequence: => Seq[A] < S): A < (S & Seqs) =
            Seqs.get(sequence)

        def fromTry[A, S](_try: => scala.util.Try[A] < S): A < (S & Aborts[Throwable]) =
            _try.map(t => Aborts[Throwable].get(t.toEither))

        inline def logInfo[S](message: => String < S): Unit < (S & IOs) =
            message.map(m => Logs.info(m))

        inline def logInfo[S, S1](
            message: => String < S,
            err: => Throwable < S1
        ): Unit < (S & S1 & IOs) =
            message.map(m => err.map(e => Logs.info(m, e)))

        inline def logWarn[S](message: => String < S): Unit < (S & IOs) =
            message.map(m => Logs.warn(m))

        inline def logWarn[S, S1](
            message: => String < S,
            err: => Throwable < S1
        ): Unit < (S & S1 & IOs) =
            message.map(m => err.map(e => Logs.warn(m, e)))

        inline def logDebug[S](message: => String < S): Unit < (S & IOs) =
            message.map(m => Logs.debug(m))

        inline def logDebug[S, S1](
            message: => String < S,
            err: => Throwable < S1
        ): Unit < (S & S1 & IOs) =
            message.map(m => err.map(e => Logs.debug(m, e)))

        inline def logError[S](message: => String < S): Unit < (S & IOs) =
            message.map(m => Logs.error(m))

        inline def logError[S, S1](
            message: => String < S,
            err: => Throwable < S1
        ): Unit < (S & S1 & IOs) =
            message.map(m => err.map(e => Logs.error(m, e)))

        inline def logTrace[S](message: => String < S): Unit < (S & IOs) =
            message.map(m => Logs.trace(m))

        inline def logTrace[S, S1](
            message: => String < S,
            err: => Throwable < S1
        ): Unit < (S & S1 & IOs) =
            message.map(m => err.map(e => Logs.trace(m, e)))

        def never: Nothing < Fibers =
            Fibers.never.join
                *> IOs(throw new IllegalStateException("Fibers.never completed"))

        val none: Nothing < Options =
            Options.empty

        def provide[D, SD, A, SA, E, SR](
            dependency: => D < SD
        )(
            effect: A < (SA & Envs[E])
        )(
            using
            he: HasEnvs[D, E] { type Remainder = SR },
            fl: Flat[A < (SA & Envs[E])]
        ): A < (SA & SD & SR) =
            dependency.map(d => Envs[D].run[A, SA, E, SR](d)(effect))

        def scoped[A, S](resource: => A < (S & Resources)): A < (IOs & S) =
            Resources.run(resource)

        def service[D](using Tag[Envs[D]]): D < Envs[D] =
            Envs[D].get

        def serviceWith[D](using Tag[Envs[D]]): [A, S] => (D => A < S) => A < (S & Envs[D]) =
            [A, S] => (fn: D => (A < S)) => service[D].map(d => fn(d))

        def sleep[S](duration: => Duration < S): Unit < (S & Fibers) =
            duration.map(d => Fibers.sleep(d))

        def suspend[A, S](effect: => A < S): A < (S & IOs) =
            IOs(effect)

        def suspendAttempt[A, S](effect: => A < S): A < (S & IOs & Aborts[Throwable]) =
            IOs(Aborts[Throwable].catching(effect))

        def traverse[A, S, S1](
            sequence: => Seq[A < (S & Seqs)] < S1
        )(using Flat[A < (S & Seqs)]): Seq[A] < (S & S1) =
            Seqs.run(traverseSeqs(sequence))

        def traverseDiscard[A, S, S1](
            sequence: => Seq[A < (S & Seqs)] < S1
        )(using Flat[A < (S & Seqs)]): Unit < (S & S1) =
            val seqEffect: Nothing < (S & S1 & Seqs) = traverseSeqs(sequence) *> KYO.dropSeqs
            Seqs.run(seqEffect).discard
        end traverseDiscard

        def traverseSeqs[A, S, S1](
            sequence: => Seq[A < (S & Seqs)] < S1
        )(using Flat[A < (S & Seqs)]): A < (S & S1 & Seqs) =
            Seqs.get[A < (S & Seqs), S1](sequence).flatten

        def traverseEachSeqs[A, S](
            each: => (A < (S & Seqs))*
        )(using Flat[A < (S & Seqs)]): A < (S & Seqs) =
            Seqs.get[A < (S & Seqs), Any](each).flatten

        def traversePar[A, S](
            sequence: => Seq[A < (Fibers & Seqs)] < S
        )(using Flat[A < (S & Seqs)]): Seq[A] < (S & Fibers) =
            sequence.map(seq => foreachPar(seq.map(eff => Seqs.run(eff)))(identity).map(_.flatten))

        def traverseParDiscard[A, S](
            sequence: => Seq[A < (Fibers & Seqs)] < S
        )(using Flat[A < (S & Seqs)]): Unit < (S & Fibers) =
            sequence.map(seq =>
                foreachPar(seq.map(eff => Seqs.run(eff)))(identity).map(
                    _.flatten
                ).discard
            )
    end KYO

    extension [A, S](effect: A < S)
        @targetName("zipRight")
        def *>[A1, S1](next: => A1 < S1): A1 < (S & S1) =
            effect.map(_ => next)

        @targetName("zipLeft")
        def <*[A1, S1](next: => A1 < S1): A < (S & S1) =
            effect.map(e => next.as(e))

        @targetName("zip")
        def <*>[A1, S1](next: => A1 < S1): (A, A1) < (S & S1) =
            effect.map(e => next.map(n => (e, n)))

        def as[A1, S1](value: => A1 < S1): A1 < (S & S1) =
            effect.map(_ => value)

        def debug: A < (S & IOs) =
            effect.tap(value => Console.default.println(value.toString))

        def debug(prefix: => String): A < (S & IOs) =
            effect.tap(value => Console.default.println(s"$prefix: $value"))

        def delayed[S1](duration: Duration < S1): A < (S & S1 & Fibers) =
            KYO.sleep(duration) *> effect

        def discard: Unit < S = as(())

        def repeat(policy: Retries.Policy)(using Flat[A < S]): A < (S & Fibers) =
            def loop(i: Int): A < (S & Fibers) =
                if i <= 0 then effect
                else loop(i - 1).delayed(policy.backoff(i))

            loop(policy.limit)
        end repeat

        def repeat[S1](limit: => Int < S1)(using Flat[A < S]): A < (S & S1) =
            @tailrec
            def loop(i: Int): A < (S & S1) =
                if i <= 0 then effect
                else loop(i - 1)

            limit.map(l => loop(l))
        end repeat

        def repeat[S1](backoff: Int => Duration, limit: => Int < S1)(using
            Flat[A < S]
        ): A < (S & S1 & Fibers) =
            def loop(i: Int): A < (S & Fibers) =
                if i <= 0 then effect
                else loop(i - 1).delayed(backoff(i))

            limit.map(l => loop(l))
        end repeat

        def repeatWhile[S1](fn: A => Boolean < S1)(using Flat[A < S]): A < (S & S1 & Fibers) =
            def loop(last: A): A < (S & S1 & Fibers) =
                fn(last).map { cont =>
                    if cont then effect.map(v => loop(v))
                    else last
                }

            effect.map(v => loop(v))
        end repeatWhile

        def repeatWhile[S1](fn: (A, Int) => (Boolean, Duration) < S1)(using
            Flat[A < S]
        ): A < (S & S1 & Fibers) =
            def loop(last: A, i: Int): A < (S & S1 & Fibers) =
                fn(last, i).map { case (cont, duration) =>
                    if cont then KYO.sleep(duration) *> effect.map(v => loop(v, i + 1))
                    else last
                }

            effect.map(v => loop(v, 0))
        end repeatWhile

        def repeatUntil[S1](fn: A => Boolean < S1)(using Flat[A < S]): A < (S & S1 & Fibers) =
            def loop(last: A): A < (S & S1 & Fibers) =
                fn(last).map { cont =>
                    if cont then last
                    else effect.map(v => loop(v))
                }

            effect.map(v => loop(v))
        end repeatUntil

        def repeatUntil[S1](fn: (A, Int) => (Boolean, Duration) < S1)(using
            Flat[A < S]
        ): A < (S & S1 & Fibers) =
            def loop(last: A, i: Int): A < (S & S1 & Fibers) =
                fn(last, i).map { case (cont, duration) =>
                    if cont then last
                    else KYO.sleep(duration) *> effect.map(v => loop(v, i + 1))
                }

            effect.map(v => loop(v, 0))
        end repeatUntil

        def retry(policy: Retries.Policy)(using Flat[A < S]): A < (S & Fibers) =
            Retries(policy)(effect)

        def retry[S1](n: => Int < S1)(using Flat[A < S]): A < (S & S1 & Fibers) =
            n.map(nPure => Retries(Retries.Policy(_ => Duration.Zero, nPure))(effect))

        def retry[S1](backoff: Int => Duration, n: => Int < S1)(using
            Flat[A < S]
        ): A < (S & S1 & Fibers) =
            n.map(nPure => Retries(Retries.Policy(backoff, nPure))(effect))

        def forever: Nothing < S =
            (effect *> effect.forever) *> (throw new IllegalStateException("infinite loop ended"))

        def when[S1](condition: => Boolean < S1): A < (S & S1 & Options) =
            condition.map(c => if c then effect else Options.empty)

        def explicitThrowable: A < (S & Aborts[Throwable]) =
            Aborts[Throwable].catching(effect)

        def tap[S1](f: A => Any < S1): A < (S & S1) =
            effect.map(a => f(a).as(a))

    end extension

    extension [A, S, E](effect: A < (S & Aborts[E]))
        def abortsToOptions(
            using
            Tag[Aborts[E]],
            ClassTag[E],
            Flat[A < (S & Aborts[E])]
        ): A < (S & Options) =
            Aborts[E].run(effect).map(e => Options.get(e.toOption))

        def someAbortsToOptions[E1: ClassTag](
            using
            ha: HasAborts[E1, E],
            t: Tag[Aborts[E1]],
            f: Flat[A < (S & Aborts[E])]
        ): A < (S & ha.Remainder & Options) =
            Aborts[E1].run(effect).map(e => Options.get(e.toOption))

        def abortsToSeqs(
            using
            Tag[Aborts[E]],
            ClassTag[E],
            Flat[A < (S & Seqs)]
        ): A < (S & Seqs) =
            Aborts[E].run(effect).map(e => Seqs.get(e.toOption.toList))

        def someAbortsToSeqs[E1: Tag: ClassTag](
            using
            ha: HasAborts[E1, E],
            t: Tag[Aborts[E1]],
            f: Flat[A < (S & Aborts[E])]
        ): A < (S & ha.Remainder & Seqs) =
            Aborts[E1].run(effect).map(e => Seqs.get(e.toOption.toList))

        def handleAborts(
            using
            Tag[Aborts[E]],
            ClassTag[E],
            Flat[A < (S & Aborts[E])]
        ): Either[E, A] < S =
            Aborts[E].run(effect)

        def handleSomeAborts[E1: ClassTag](
            using
            ha: HasAborts[E1, E],
            t: Tag[Aborts[E1]],
            f: Flat[A < (S & Aborts[E])]
        ): Either[E1, A] < (S & ha.Remainder) =
            Aborts[E1].run(effect)

        def catchAborts[A1 >: A, S1](fn: E => A1 < S1)(
            using
            Tag[Aborts[E]],
            ClassTag[E],
            Flat[A < (S & Aborts[E])]
        ): A1 < (S & S1) =
            Aborts[E].run(effect).map {
                case Left(e)  => fn(e)
                case Right(a) => a
            }

        def catchAbortsPartial[A1 >: A, S1](fn: PartialFunction[E, A1 < S1])(
            using
            Tag[Aborts[E]],
            ClassTag[E],
            Flat[A < (S & Aborts[E])]
        ): A1 < (S & S1 & Aborts[E]) =
            Aborts[E].run(effect).map {
                case Left(e) if fn.isDefinedAt(e) => fn(e)
                case Left(e)                      => Aborts[E].fail(e)
                case Right(a)                     => a
            }

        def catchSomeAborts[E1](using
            ct: ClassTag[E1],
            ha: HasAborts[E1, E],
            t: Tag[Aborts[E1]],
            f: Flat[A < (S & Aborts[E])]
        ): [A1 >: A, S1] => (E1 => A1 < S1) => A1 < (S & S1 & ha.Remainder) =
            [A1 >: A, S1] =>
                (fn: E1 => A1 < S1) =>
                    Aborts[E1].run(effect).map {
                        case Left(e1) => fn(e1)
                        case Right(a) => a
                }

        def catchSomeAbortsPartial[E1](using
            ct: ClassTag[E1],
            ha: HasAborts[E1, E],
            t: Tag[Aborts[E1]],
            f: Flat[A < (S & Aborts[E])]
        ): [A1 >: A, S1] => PartialFunction[E1, A1 < S1] => A1 < (S & S1 & Aborts[E]) =
            [A1 >: A, S1] =>
                (fn: PartialFunction[E1, A1 < S1]) =>
                    Aborts[E1].run(effect).map {
                        case Left(e1) if fn.isDefinedAt(e1) => fn(e1)
                        case Left(e1)                       => Aborts[E1].fail(e1)
                        case Right(a)                       => a
                        // Need asInstanceOf because compiler doesn't know ha.Remainder & Aborts[E1]
                        // is the same as Aborts[E]
                    }.asInstanceOf[A1 < (S & S1 & Aborts[E])]

        def swapAborts(
            using
            Tag[Aborts[E]],
            Tag[Aborts[A]],
            ClassTag[E],
            ClassTag[A],
            Flat[A < (S & Aborts[E])]
        ): E < (S & Aborts[A]) =
            val handled = handleAborts
            handled.map((v: Either[E, A]) => Aborts[A].get(v.swap))
        end swapAborts

        def swapSomeAborts[E1: ClassTag](
            using
            ha: HasAborts[E1, E],
            te: Tag[Aborts[E]],
            ta: Tag[Aborts[A]],
            te1: Tag[Aborts[E1]],
            cte: ClassTag[E],
            cta: ClassTag[A],
            f: Flat[A < (S & Aborts[E])]
        ): E1 < (S & ha.Remainder & Aborts[A]) =
            val handled = Aborts[E1].run(effect)
            handled.map((v: Either[E1, A]) => Aborts[A].get(v.swap))
        end swapSomeAborts

        def implicitThrowable(
            using
            f: Flat[A < (S & Aborts[E])],
            ha: HasAborts[Throwable, E]
        ): A < (S & ha.Remainder) =
            Aborts[Throwable].run(effect).map {
                case Right(a) => a
                case Left(e)  => throw e
            }
    end extension

    extension [A, S](effect: A < (S & Options))
        def handleOptions(
            using Flat[A < (S & Options)]
        ): Option[A] < S = Options.run(effect)

        def catchOptions[A1 >: A, S1](orElse: => A1 < S1)(
            using Flat[A]
        ): A1 < (S & S1) =
            Options.run(effect).map {
                case None    => orElse
                case Some(a) => a
            }

        def swapOptionsAs[A1, S1](value: => A1 < S1)(
            using Flat[A]
        ): A1 < (S & S1 & Options) =
            Options.run(effect).map {
                case None    => value
                case Some(a) => Options.empty
            }

        def swapOptions(using Flat[A]): Unit < (S & Options) =
            swapOptionsAs(())

        def optionsToAborts[E, S1](failure: => E < S1)(
            using
            Flat[A],
            Tag[Aborts[E]]
        ): A < (S & S1 & Aborts[E]) =
            Options.run(effect).map {
                case None    => KYO.fail(failure)
                case Some(a) => a
            }

        def optionsToThrowable(using Flat[A], Tag[Aborts[Throwable]]): A < (S & Aborts[Throwable]) =
            effect.optionsToAborts[Throwable, Any](new NoSuchElementException("None.get"))

        def optionsToUnit(using Flat[A]): A < (S & Aborts[Unit]) =
            effect.optionsToAborts(())

        def optionsToSeqs(using Flat[A]): A < (S & Seqs) =
            Options.run(effect).map(aOpt => Seqs.get(aOpt.toSeq))
    end extension

    extension [A, S, E](effect: A < (S & Envs[E]))
        def provide[E1, S1, SR](dependency: E1 < S1)(
            using
            fl: Flat[A < (S & Envs[E])],
            he: HasEnvs[E1, E] { type Remainder = SR },
            t: Tag[Envs[E1]]
        ): A < (S & S1 & SR) =
            dependency.map(d => Envs[E1].run[A, S, E, SR](d)(effect))

        def provideAs[E1](
            using
            f: Flat[A < (S & Envs[E])],
            t: Tag[Envs[E1]],
            he: HasEnvs[E1, E]
        ): ProvideAsPartiallyApplied[A, S, E, E1, he.Remainder] =
            ProvideAsPartiallyApplied(effect)
    end extension

    extension [A, S](effect: A < (S & Seqs))
        def filterSeqs[S1](fn: A => Boolean < S1): A < (S & S1 & Seqs) =
            effect.map(a => Seqs.filter(fn(a)).as(a))

        def handleSeqs(using Flat[A < (S & Seqs)]): Seq[A] < S = Seqs.run(effect)

        def seqsToOptions(using Flat[A < (S & Seqs)]): A < (S & Options) =
            Seqs.run(effect).map(seq => Options.get(seq.headOption))

        def seqsToAborts[E, S1](error: => E < S1)(
            using
            Flat[A < (S & Seqs)],
            Tag[Aborts[E]]
        ): A < (S & S1 & Aborts[E]) =
            Seqs.run(effect).map {
                case s if s.isEmpty => KYO.fail[E, S1](error)
                case s              => s.head
            }

        def seqsToThrowable(using Flat[A]): A < (S & Aborts[Throwable]) =
            seqsToAborts[Throwable, Any](new NoSuchElementException("head of empty list"))

        def seqsToUnit(using Flat[A]): A < (S & Aborts[Unit]) =
            seqsToAborts(())
    end extension

    extension [A, S](effect: A < (S & Fibers))
        def fork(
            using
            @implicitNotFound(
                "Only Fibers- and IOs-based effects can be forked. Found: ${S}"
            ) ev: S => IOs,
            f: Flat[A < (Fibers & S)]
        ): Fiber[A] < (S & IOs) = Fibers.init(effect)

        def forkScoped(
            using
            @implicitNotFound(
                "Only Fibers- and IOs-based effects can be forked. Found: ${S}"
            ) ev: S => IOs,
            f: Flat[A < (Fibers & S)]
        ): Fiber[A] < (S & IOs & Resources) =
            KYO.acquireRelease(Fibers.init(effect))(_.interrupt.discard)
    end extension

    extension [A, S](fiber: Fiber[A] < S)
        def join: A < (S & Fibers) = Fibers.get(fiber)
        def awaitCompletion(using Flat[A < (S & Fibers)]): Unit < (S & Fibers) =
            KYO.attempt(Fibers.get(fiber))
                .handleAborts
                .discard
    end extension

    extension [A](effect: A < Fibers)
        @targetName("zipRightPar")
        def &>[A1](next: A1 < Fibers)(
            using
            Flat[A < Fibers],
            Flat[A1 < Fibers]
        ): A1 < Fibers =
            for
                fiberA  <- effect.fork
                fiberA1 <- next.fork
                _       <- fiberA.awaitCompletion
                a1      <- fiberA1.join
            yield a1

        @targetName("zipLeftPar")
        def <&[A1](next: A1 < Fibers)(
            using
            Flat[A < Fibers],
            Flat[A1 < Fibers]
        ): A < Fibers =
            for
                fiberA  <- effect.fork
                fiberA1 <- next.fork
                a       <- fiberA.join
                _       <- fiberA1.awaitCompletion
            yield a

        @targetName("zipPar")
        def <&>[A1](next: A1 < Fibers)(
            using
            Flat[A < Fibers],
            Flat[A1 < Fibers]
        ): (A, A1) < Fibers =
            for
                fiberA  <- effect.fork
                fiberA1 <- next.fork
                a       <- fiberA.join
                a1      <- fiberA1.join
            yield (a, a1)
    end extension

    extension [A, S](effect: A < (S & Consoles))
        def provideDefaultConsole: A < (S & IOs) = Consoles.run(effect)

    final class ProvideAsPartiallyApplied[A, S, E, E1, ER](
        effect: A < (S & Envs[E])
    )(
        using
        t: Tag[Envs[E1]],
        he: HasEnvs[E1, E] { type Remainder = ER },
        f: Flat[A < (S & Envs[E])]
    ):
        def apply[S1](dependency: E1 < S1): A < (S & S1 & ER) =
            dependency.map(d => Envs[E1].run(d)(effect))
    end ProvideAsPartiallyApplied
end zikyo
