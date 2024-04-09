package zikyoTest

import kyo.*
import zikyo.*

class envsTest extends ZiKyoTest:

    class Dep(val value: Int)
    object DepImpl extends Dep(1)

    "envs" - {
        "construct" - {
            "should construct from type" in {
                val effect = KYO.service[String]
                assert(Envs[String].run("value")(effect).pure == "value")
            }

            "should construct from type and use" in {
                val effect = KYO.serviceWith[String](_.length)
                assert(Envs[String].run("value")(effect).pure == 5)
            }
        }

        "handle" - {
            "should provide" in {
                val effect: Int < Envs[String] = Envs[String].get.map(_.length)
                assert(effect.provide("value").pure == 5)
            }

            "should provide as" in {
                val effect: Int < Envs[Dep] = Envs[Dep].get.map(_.value)
                assert(effect.provideAs[Dep](DepImpl).pure == 1)
            }

            "should provide incrementally" in {
                val effect: Int < Envs[String & Int & Boolean & Char] =
                    Envs[String].get *> Envs[Int].get *> Envs[Boolean].get *> Envs[Char].get.as(23)
                val handled =
                    effect
                        .provide('c')
                        .provide("value")
                        .provide(1)
                        .provide(false)
                assert(handled.pure == 23)
            }
        }
    }

end envsTest
