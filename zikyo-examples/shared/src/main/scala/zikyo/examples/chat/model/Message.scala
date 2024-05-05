package zikyo.examples.chat.model

import kyo.*
import zikyo.*
import zikyo.examples.chat.model.DomainError.ValidationError

type Message = Message.Type

object Message:
    opaque type Type <: String = String
    given flatMessage: Flat[Message] =
        (new {}).asInstanceOf[Flat[Message]]

    val ValidMessage = """[a-zA-Z.,<>?/\\|\]\[\-_=+~`'":;!@#$%^&*()]*""".r

    def parse(message: String): Message < Aborts[String] =
        if message.length < 1 then
            Aborts[String].fail(s"Message text must have at least one character")
        else if message.length > 600 then
            Aborts[String].fail(s"Message text cannot be longer than 600 characters")
        else if !ValidMessage.matches(message) then
            Aborts[String].fail(s"Invalid message: must match $ValidMessage")
        else message

    def validated(dataId: String, message: String): Message < Aborts[ValidationError] =
        parse(message).catchAborts(msg =>
            Aborts[ValidationError].fail(ValidationError(dataId, msg))
        )

    def unapply(message: String): Option[Message] = Aborts[String].run(parse(message)).pure match
        case Left(value)  => None
        case Right(value) => Some(value)

    opaque type ID = Long
    object ID:
        def apply(long: Long): ID = long
        val random: ID < IOs      = Randoms.nextLong

end Message
