package zikyo.examples.chat.model

sealed trait Event

object Event:
    final case class MessageSent(message: Message)
