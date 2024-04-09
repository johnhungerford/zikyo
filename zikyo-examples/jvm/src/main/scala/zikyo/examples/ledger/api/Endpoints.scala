package zikyo.examples.ledger.api

import java.time.Instant
import kyo.*
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.generic.auto.*
import sttp.tapir.json.zio.*
import zikyo.*
import zikyo.examples.ledger.*
import zio.json.JsonEncoder

object Endpoints:

    val init: Unit < (Envs[Handler] & Routes) = defer {

        val handler = await(KYO.service[Handler])

        await {
            Routes.add(
                _.post
                    .in("clientes" / path[Int]("id") / "transacoes")
                    .errorOut(statusCode)
                    .in(jsonBody[Transaction])
                    .out(jsonBody[Processed])
            )(handler.transaction)
        }

        await {
            Routes.add(
                _.get
                    .in("clientes" / path[Int]("id") / "extrato")
                    .errorOut(statusCode)
                    .out(jsonBody[Statement])
            )(handler.statement)
        }

        await {
            Routes.add(
                _.get
                    .in("health")
                    .out(stringBody)
            )(_ => "ok")
        }
    }

end Endpoints
