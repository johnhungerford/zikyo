package zikyo.examples.chat.model

import kyo.*

sealed trait DomainError

object DomainError:
    final case class ValidationError(dataIdentifier: String, message: String)

    final case class ValidationErrors(dataIdentifierToMessageMap: Map[String, String])

    opaque type Validations = Sums[ValidationErrors]
    object Validations:
        def fail(dataIdentifier: String, message: String): Unit < Validations =
            Sums[ValidationErrors].add(ValidationErrors(Map(dataIdentifier -> message)))

        given Summer[ValidationErrors] with
            override def init: ValidationErrors = ValidationErrors(Map.empty)

            override def add(v1: ValidationErrors, v2: ValidationErrors): ValidationErrors =
                ValidationErrors(v1.dataIdentifierToMessageMap ++ v2.dataIdentifierToMessageMap)

            override def result(v: ValidationErrors): ValidationErrors = v
        end given

        extension [A, S](effect: A < (S & Validations))
            def validationsToAborts(using
                Flat[A < (S & Validations)]
            ): A < (S & Aborts[ValidationErrors]) =
                val handledValidations: (ValidationErrors, A) < S =
                    Sums[ValidationErrors].run(effect)

                handledValidations.flatMap {
                    case (errs: ValidationErrors, a: A) =>
                        if errs.dataIdentifierToMessageMap.isEmpty then a
                        else Aborts[ValidationErrors].fail(errs)
                }
    end Validations
end DomainError
