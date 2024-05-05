package zikyo.examples.chat.model

import DomainError.ValidationError
import java.time.Instant
import kyo.*
import zikyo.*

sealed trait Event:
    def timestamp: Instant

object Event:
    final case class MessageSendRequested(
        from: User.UserName,
        to: User.UserName,
        message: Message,
        identifier: Message.ID,
        override val timestamp: Instant
    ) extends Event

    def messageSendRequest(
        from: User.UserName,
        to: User.UserName,
        message: Message,
    ): MessageSendRequested < IOs =
        for
            identifier <- Message.ID.random
            timestamp <- Clocks.now
        yield MessageSendRequested(from, to, message, identifier, timestamp)

    def messageSendRequestValidated(
        from: String,
        to: String,
        message: String
    ): MessageSendRequested < (IOs & Aborts[ValidationError]) =
        for
            fromTyped <- User.UserName.validated("sender username", from)
            toTyped <- User.UserName.validated("recipient username", to)
            messageTyped <- Message.validated("message body", message)
            event <- messageSendRequest(fromTyped, toTyped, messageTyped)
        yield event

    final case class MessageSendFailed(
        from: User.UserName,
        to: User.UserName,
        identifier: Message.ID,
        failureReason: String,
        override val timestamp: Instant
    ) extends Event

    def messageSendFailed(
        from: User.UserName,
        to: User.UserName,
        identifier: Message.ID,
        failureReason: String,
    ): MessageSendFailed < IOs =
        Clocks.now.map(ts => MessageSendFailed(from, to, identifier, failureReason, ts))

    final case class MessageSent(
        from: User.UserName,
        to: User.UserName,
        message: Message,
        identifier: Message.ID,
        override val timestamp: Instant
    ) extends Event

    def messageSent(
        from: User.UserName,
        to: User.UserName,
        message: Message,
        identifier: Message.ID,
    ): MessageSent < IOs =
        Clocks.now.map(ts => MessageSent(from, to, message, identifier, ts))

    final case class MessageDeliveryFailed(
        identifier: Message.ID,
        failureReason: String,
        timestamp: Instant
    )

    def messageDeliveryFailed(
        identifier: Message.ID,
        failureReason: String,
    ): MessageDeliveryFailed < IOs =
        Clocks.now.map(ts => MessageDeliveryFailed(identifier, failureReason, ts))

    final case class MessageDelivered(
        identifier: Message.ID,
        override val timestamp: Instant
    ) extends Event

    def messageDelivered(identifier: Message.ID): MessageDelivered < IOs =
        Clocks.now.map(ts => MessageDelivered(identifier, ts))

    object MessageUpdate:
        opaque type ID = Long

        object ID:
            def apply(long: Long): ID = long
            val random: ID < IOs      = Randoms.nextLong
    end MessageUpdate

    final case class MessageUpdateRequested(
        newMessage: Message,
        messageIdentifier: Message.ID,
        updateIdentifier: MessageUpdate.ID,
        override val timestamp: Instant
    ) extends Event

    def messageUpdateRequested(
        newMessage: Message,
        messageIdentifier: Message.ID,
    ): MessageUpdateRequested < IOs =
        for
            updateIdentifier <- MessageUpdate.ID.random
            timestamp <- Clocks.now
        yield
            MessageUpdateRequested(newMessage, messageIdentifier, updateIdentifier, timestamp)

    final case class MessageUpdateFailed(
        messageIdentifier: Message.ID,
        updateIdentifier: MessageUpdate.ID,
        failureReason: String,
        override val timestamp: Instant
    ) extends Event

    def messageUpdateFailed(
        messageIdentifier: Message.ID,
        updateIdentifier: MessageUpdate.ID,
        failureReason: String,
    ): MessageUpdateFailed < IOs =
        Clocks.now.map(
            ts => MessageUpdateFailed(messageIdentifier, updateIdentifier, failureReason, ts)
        )

    final case class MessageUpdated(
        newMessage: Message,
        messageIdentifier: Message.ID,
        updateIdentifier: MessageUpdate.ID,
        override val timestamp: Instant
    ) extends Event

    def messageUpdated(
        newMessage: Message,
        messageIdentifier: Message.ID,
        updateIdentifier: MessageUpdate.ID,
    ): MessageUpdated < IOs =
        Clocks.now.map(ts => MessageUpdated(newMessage, messageIdentifier, updateIdentifier, ts))

    final case class MessageRead(
        identifier: Message.ID,
        override val timestamp: Instant
    ) extends Event

    def messageRead(identifier: Message.ID): MessageRead < IOs =
        Clocks.now.map(ts => MessageRead(identifier, ts))

    final case class UserCreateRequested(
        newUser: User,
        override val timestamp: Instant
    ) extends Event

    def userCreateRequested(newUser: User): UserCreateRequested < IOs =
        Clocks.now.map(ts => UserCreateRequested(newUser, ts))

    def userCreateRequestedValidated(
        userName: String,
        firstName: Option[String],
        lastName: Option[String],
    ): UserCreateRequested < (IOs & Aborts[ValidationError]) =
        for
            userNameTyped <- User.UserName.validated("new username", userName)
            user = User(userNameTyped, firstName, lastName)
            event <- userCreateRequested(user)
        yield event

    final case class UserCreated(
        newUser: User,
        override val timestamp: Instant
    ) extends Event

    def userCreated(
        newUser: User,
    ): UserCreated < IOs = Clocks.now.map(ts => UserCreated(newUser, ts))

    def userCreatedValidated(
        userName: String,
        firstName: Option[String],
        lastName: Option[String],
    ): UserCreated < (IOs & Aborts[ValidationError]) =
        User.UserName.validated("new username", userName).flatMap { userNameTyped =>
            val newUser = User(userNameTyped, firstName, lastName)
            userCreated(newUser)
        }

    object UserUpdate:
        opaque type ID = Long

        object ID:
            def apply(long: Long): ID = long
            val random: ID < IOs      = Randoms.nextLong
    end UserUpdate

    final case class UserUpdateRequested(
        updatedUser: User,
        updateIdentifier: UserUpdate.ID,
        override val timestamp: Instant
    ) extends Event

    def userUpdateRequested(
        updatedUser: User,
    ): UserUpdateRequested < IOs =
        for
            updateId <- UserUpdate.ID.random
            timestamp <- Clocks.now
        yield
            UserUpdateRequested(updatedUser, updateId, timestamp)

    def userUpdateRequestedValidated(
        userName: String,
        firstName: Option[String],
        lastName: Option[String],
    ): UserUpdateRequested < (IOs & Aborts[ValidationError]) =
        for
            userNameTyped <- User.UserName.validated("updated username", userName)
            user = User(userNameTyped, firstName, lastName)
            event <- userUpdateRequested(user)
        yield event

    final case class UserUpdateFailed(
        userIdentifier: User.UserName,
        updateIdentifier: UserUpdate.ID,
        failureReason: String,
        override val timestamp: Instant
    ) extends Event

    def userUpdateFailed(
        userIdentifier: User.UserName,
        updateIdentifier: UserUpdate.ID,
        failureReason: String,
    ): UserUpdateFailed < IOs =
        Clocks.now.map(
            ts => UserUpdateFailed(userIdentifier, updateIdentifier, failureReason, ts)
        )

    final case class UserUpdated(
        updatedUser: User,
        updateIdentifier: UserUpdate.ID,
        override val timestamp: Instant
    ) extends Event

    def userUpdated(
        updatedUser: User,
        updateIdentifier: UserUpdate.ID,
    ): UserUpdated < IOs =
        Clocks.now.map(ts => UserUpdated(updatedUser, updateIdentifier, ts))

    object UserDelete:
        opaque type ID = Long

        object ID:
            def apply(long: Long): ID = long
            val random: ID < IOs      = Randoms.nextLong
    end UserDelete

    final case class UserDeleteRequested(
        userIdentifier: User.UserName,
        deleteIdentifier: UserDelete.ID,
        override val timestamp: Instant
    ) extends Event

    def userDeleteRequested(
        userIdentifier: User.UserName,
        deleteIdentifier: UserDelete.ID,
    ): UserDeleteRequested < IOs =
        Clocks.now.map(ts => UserDeleteRequested(userIdentifier, deleteIdentifier, ts))

    final case class UserDeleteFailed(
        userName: User.UserName,
        deleteIdentifier: UserDelete.ID,
        failureReason: String,
        override val timestamp: Instant
    ) extends Event

    def userDeleteFailed(
        userName: User.UserName,
        deleteIdentifier: UserDelete.ID,
        failureReason: String,
    ): UserDeleteFailed < IOs =
        Clocks.now.map(ts => UserDeleteFailed(userName, deleteIdentifier, failureReason, ts))

    final case class UserDeleted(
        userName: User.UserName,
        deleteIdentifier: UserDelete.ID,
        override val timestamp: Instant
    ) extends Event

    def userDeleted(
        userName: User.UserName,
        deleteIdentifier: UserDelete.ID,
    ): UserDeleted < IOs =
        Clocks.now.map(ts => UserDeleted(userName, deleteIdentifier, ts))

end Event
