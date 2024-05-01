package zikyoTest

import kyo.*
import zikyo.*

class choicesTest extends ZiKyoTest:

    "choices" - {
        "construct" - {
            "should construct choices from a sequence" in {
                val effect = KYO.fromSeq(Seq(1, 2, 3))
                assert(Choices.run(effect) == Seq(1, 2, 3))
            }
        }

        "handle" - {
            "should handle" in {
                val effect: Int < Choices = Choices.get(Seq(1, 2, 3))
                assert(effect.handleSeqs.pure == Seq(1, 2, 3))
            }
        }

        "filter" - {
            "should filter" in {
                val effect: Int < Choices = Choices.get(Seq(1, 2, 3))
                val filteredEffect        = effect.filterChoices(_ < 3)
                assert(filteredEffect.handleSeqs.pure == Seq(1, 2))
            }
        }

        "convert" - {
            "should convert seqs to options, constructing Some from Seq#head" in {
                val failure: Int < Choices        = Choices.get(Nil)
                val failureOptions: Int < Options = failure.choicesToOptions
                val handledFailureOptions         = Options.run(failureOptions)
                assert(handledFailureOptions.pure == None)
                val success: Int < Choices        = Choices.get(Seq(1, 2, 3))
                val successOptions: Int < Options = success.choicesToOptions
                val handledSuccessOptions         = Options.run(successOptions)
                assert(handledSuccessOptions.pure == Some(1))
            }

            "should convert seqs to aborts, constructing Right from Seq#head" in {
                val failure: Int < Choices              = Choices.get(Nil)
                val failureAborts: Int < Aborts[String] = failure.choicesToAborts("failure")
                val handledFailureAborts                = Aborts[String].run(failureAborts)
                assert(handledFailureAborts.pure == Left("failure"))
                val success: Int < Choices              = Choices.get(Seq(1, 2, 3))
                val successAborts: Int < Aborts[String] = success.choicesToAborts("failure")
                val handledSuccessAborts                = Aborts[String].run(successAborts)
                assert(handledSuccessAborts.pure == Right(1))
            }

            "should convert choices to throwable aborts, constructing Right from Seq#head" in {
                val failure: Int < Choices                 = Choices.get(Nil)
                val failureAborts: Int < Aborts[Throwable] = failure.choicesToThrowable
                val handledFailureAborts                   = Aborts[Throwable].run(failureAborts)
                assert(handledFailureAborts.pure.left.toOption.get.getMessage.contains(
                    "head of empty list"
                ))
                val success: Int < Choices                 = Choices.get(Seq(1, 2, 3))
                val successAborts: Int < Aborts[Throwable] = success.choicesToThrowable
                val handledSuccessAborts                   = Aborts[Throwable].run(successAborts)
                assert(handledSuccessAborts.pure == Right(1))
            }

            "should convert choices to unit aborts, constructing Right from Seq#head" in {
                val failure: Int < Choices            = Choices.get(Nil)
                val failureAborts: Int < Aborts[Unit] = failure.choicesToUnit
                val handledFailureAborts              = Aborts[Unit].run(failureAborts)
                assert(handledFailureAborts.pure == Left(()))
                val success: Int < Choices            = Choices.get(Seq(1, 2, 3))
                val successAborts: Int < Aborts[Unit] = success.choicesToUnit
                val handledSuccessAborts              = Aborts[Unit].run(successAborts)
                assert(handledSuccessAborts.pure == Right(1))
            }
        }

        "catch" - {
            "should catch" in {
                val effect1: Int < Options = KYO.none
                assert(effect1.catchOptions(100).pure == 100)

                val effect2: Int < Options = 23
                assert(effect2.catchOptions(100).pure == 23)
            }
        }

        "swap" - {
            "should swap" in {
                val failure: Int < Options         = Options.empty
                val swappedFailure: Unit < Options = failure.swapOptions
                val handledFailure                 = Options.run(swappedFailure)
                assert(handledFailure.pure == Some(()))
                val success: Int < Options         = 23
                val swappedSuccess: Unit < Options = success.swapOptions
                val handledSuccess                 = Options.run(swappedSuccess)
                assert(handledSuccess.pure == None)
            }

            "should swap as" in {
                val failure: Int < Options           = Options.empty
                val swappedFailure: String < Options = failure.swapOptionsAs("failure")
                val handledFailure                   = Options.run(swappedFailure)
                assert(handledFailure.pure == Some("failure"))
                val success: Int < Options           = 23
                val swappedSuccess: String < Options = success.swapOptionsAs("failure")
                val handledSuccess                   = Options.run(swappedSuccess)
                assert(handledSuccess.pure == None)
            }
        }

        "iteration" - {
            "should iterate using foreach" in {
                var state             = 0
                def effectFor(i: Int) = IOs { state += i; state }
                val effect            = KYO.foreach(1 to 10)(effectFor)
                assert(state == 0)
                val result = IOs.run(effect).pure
                //                   1, 2, 3,  4,  5,  6,  7,  8,  9, 10
                assert(result == Seq(1, 3, 6, 10, 15, 21, 28, 36, 45, 55))
                assert(state == 55)
            }

            "should iterate using collect" in {
                var state = 0
                val effect = KYO.collect(1 to 10) {
                    case i if i % 2 == 0 => IOs { state += i; i * 2 }
                }
                assert(state == 0)
                val result = IOs.run(effect).pure
                assert(result == Seq(4, 8, 12, 16, 20))
                assert(state == 30)
            }

            "should iterate using traverse" in {
                var state   = 0
                val effects = (1 to 10).map(i => IOs { state += i; state })
                val effect  = KYO.traverse(effects)
                assert(state == 0)
                val result = IOs.run(effect).pure
                assert(result == Seq(1, 3, 6, 10, 15, 21, 28, 36, 45, 55))
                assert(state == 55)
            }

            "should iterate using traverseDiscard" in {
                var state   = 0
                val effects = (1 to 10).map(i => IOs { state += i; state })
                val effect  = KYO.traverseDiscard(effects)
                assert(state == 0)
                val result = IOs.run(effect).pure
                assert(result == ())
                assert(state == 55)
            }
        }
    }

end choicesTest
