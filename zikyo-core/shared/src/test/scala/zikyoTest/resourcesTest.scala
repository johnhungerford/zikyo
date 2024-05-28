package zikyoTest

import kyo.*
import scala.concurrent.Future
import zikyo.*

class resourcesTest extends ZiKyoTest:

    "construct" - {
        "should construct a resource with acquireRelease" in {
            var state = 0
            val acquire = IOs {
                (i: Int) => IOs { state = i }
            }
            val resource = KYO.acquireRelease(acquire)(_(0))
            val effect: Int < Resources =
                for
                    setter <- resource
                    _      <- setter(50)
                    result <- IOs(state)
                yield result
            val beforeResources                = scala.concurrent.Future(assert(state == 0))
            val handledResources: Int < Fibers = Resources.run(effect)
            val handled = IOs.run(Fibers.run(handledResources).map(_.toFuture))
            for
                assertion1 <- beforeResources
                assertion2 <- handled.pure.map(_ == 50)
                assertion3 <- Future(assert(state == 0))
            yield assertion3
            end for
        }

        "should construct a resource using addFinalizer" in {
            var state   = 0
            val effect  = KYO.addFinalizer(IOs { state = 100 })
            val handled = IOs.run(Fibers.run(Resources.run(effect)).map(_.toFuture))
            for
                ass1 <- handled.pure
                ass2 <- Future(assert(state == 100))
            yield ass2
            end for
        }

        "should construct a resource from an AutoCloseable" in {
            var state = 0
            val closeable = new AutoCloseable:
                override def close(): Unit = state = 100
            val effect     = KYO.fromAutoCloseable(closeable)
            val futureAss1 = Future(assert(state == 0))
            val handled    = IOs.run(Fibers.run(Resources.run(effect)).map(_.toFuture))
            for
                ass1 <- futureAss1
                ass2 <- handled.pure.map(v => assert(v == closeable))
                ass3 <- Future(assert(state == 100))
            yield ass3
            end for
        }
    }
end resourcesTest
