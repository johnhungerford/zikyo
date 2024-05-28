package zikyo.examples.chat.model

import kyo.*

sealed trait DomainError

object DomainError:
    final case class ValidationError(dataIdentifier: String, message: String)

    final case class ValidationErrors(dataIdentifierToMessageMap: Map[String, String])

end DomainError
