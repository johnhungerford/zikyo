package zikyoTest

import kyo.*
import zikyo.*

class resourcesTest extends ZiKyoTest:

    "construct" - {
        "should construct a resource with acquireRelease" in {
            var state = 0
            val acquire = IOs {
                (i: Int) => IOs { state = i }
            }
            val resource = KYO.acquireRelease(acquire)(_(0))
            val effect =
                for
                    setter <- resource
                    _      <- setter(50)
                    result <- IOs(state)
                yield result
            assert(state == 0)
            val handled = IOs.run(Resources.run(effect))
            assert(handled.pure == 50)
            assert(state == 0)
        }

        "should construct a resource using addFinalizer" in {
            var state   = 0
            val effect  = KYO.addFinalizer(IOs { state = 100 })
            val handled = IOs.run(Resources.run(effect))
            assert(handled.pure == ())
            assert(state == 100)
        }

        "should construct a resource from an AutoCloseable" in {
            var state = 0
            val closeable = new AutoCloseable:
                override def close(): Unit = state = 100
            val effect = KYO.fromAutoCloseable(closeable)
            assert(state == 0)
            val handled = IOs.run(Resources.run(effect))
            assert(handled.pure == closeable)
            assert(state == 100)
        }
    }
end resourcesTest
