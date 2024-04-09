package zikyoTest

import kyo.*
import zikyo.*

class effectsTest extends ZiKyoTest:

    "all effects" - {
        "as" in {
            val effect         = IOs(23)
            val effectAsString = effect.as("hello")
            val handled        = IOs.run(effectAsString)
            assert(handled.pure == "hello")
        }

        "discard" in {
            val effect          = IOs(23)
            val effectDiscarded = effect.discard
            val handled         = IOs.run(effectDiscarded)
            assert(handled.pure == ())
        }

        "*>" in {
            val eff1    = IOs("hello")
            val eff2    = IOs("world")
            val zipped  = eff1 *> eff2
            val handled = IOs.run(zipped)
            assert(handled.pure == "world")
        }

        "<*" in {
            val eff1    = IOs("hello")
            val eff2    = IOs("world")
            val zipped  = eff1 <* eff2
            val handled = IOs.run(zipped)
            assert(handled.pure == "hello")
        }

        "<*>" in {
            val eff1    = IOs("hello")
            val eff2    = IOs("world")
            val zipped  = eff1 <*> eff2
            val handled = IOs.run(zipped)
            assert(handled.pure == ("hello", "world"))
        }

        "when" in {
            var state: Boolean = false
            val toggleState = IOs {
                state = !state
            }
            val getState          = IOs(state)
            val effectWhen        = (toggleState *> getState).when(getState)
            val handledEffectWhen = IOs.run(Options.run(effectWhen))
            assert(handledEffectWhen.pure == None)
            state = true
            val handledEffectWhen2 = IOs.run(Options.run(effectWhen))
            assert(handledEffectWhen2.pure == Some(false))
        }
    }
end effectsTest
