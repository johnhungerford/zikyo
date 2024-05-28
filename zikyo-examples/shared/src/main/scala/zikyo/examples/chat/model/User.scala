package zikyo.examples.chat.model

import kyo.*
import scala.util.matching.Regex
import zikyo.*
import zikyo.examples.chat.model.DomainError.ValidationError

final case class User(
    userName: User.UserName,
    firstName: Option[String],
    lastName: Option[String]
)

object User:
    opaque type UserName <: String = String

    object UserName:
        val UserNameRegex: Regex = """[a-zA-Z0-9-_]{3,23}""".r

        def parse(userName: String): UserName < Aborts[String] =
            if UserNameRegex.matches(userName) then userName
            else
                Aborts.fail(s"$userName is not a valid username. Must match $UserNameRegex")

        def validated(dataId: String, userName: String): UserName < Aborts[ValidationError] =
            parse(userName).catchAborts(msg =>
                Aborts.fail(ValidationError(dataId, msg))
            )
    end UserName
end User
