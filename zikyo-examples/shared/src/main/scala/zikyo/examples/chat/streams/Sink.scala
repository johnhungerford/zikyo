package zikyo.examples.chat.streams

import kyo.*
import zikyo.*

trait Sink[T]:
    def push(value: T): Unit < Fibers
    def pushAll(values: Seq[T]): Unit < Fibers =
        values.foldLeft((): Unit < Fibers) { (currentEff, nextValue) =>
            currentEff.flatMap(_ => push(nextValue))
        }
    def run[A, S](stream: Stream[A, T, S]): A < S
end Sink

object Sink:

    final class FromChannel[T](queue: Queue[T]) extends Sink[T]:
        override def push(value: T): Unit < Fibers =
            queue.offer(value).discard

        override def run[A, S](stream: Stream[A, T, S]): A < S = ???
    end FromChannel
end Sink
