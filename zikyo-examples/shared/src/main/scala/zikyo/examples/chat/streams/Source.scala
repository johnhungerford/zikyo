package zikyo.examples.chat.streams

import kyo.*

trait Source[T]:
    def pull: T < Fibers
    def pullN(n: Int): Seq[T] < Fibers =
        def loop(n: Int): List[T] < Fibers =
            if n <= 0 then Nil
            else pull.flatMap(t => loop(n - 1).map(next => t :: next))
        loop(n).map(_.reverse)
    end pullN
    def stream: Stream[Unit, T, Any]
end Source
