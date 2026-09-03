package com.normation.rudder.db

import com.dimafeng.testcontainers.PostgreSQLContainer
import com.normation.rudder.db.PostgresContainer.dbLayer
import com.normation.utils.DateFormaterService.toJodaDateTime
import com.normation.utils.DateFormaterService.toOffsetDateTime
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import doobie.implicits.toSqlInterpolator
import doobie.syntax.all.*
import doobie.util.fragment.Fragment
import doobie.util.transactor
import java.time.Instant
import java.time.OffsetDateTime
import java.time.ZoneOffset
import java.time.temporal.ChronoUnit
import org.joda.time.DateTime
import org.joda.time.DateTimeConstants
import org.testcontainers.utility.DockerImageName
import zio.Random
import zio.Scope
import zio.Task
import zio.ULayer
import zio.ZIO
import zio.ZIO.blockingExecutor
import zio.ZLayer
import zio.interop.catz.asyncInstance
import zio.test.*
import zio.test.Assertion.*
import zio.test.Live

object DoobieSpec extends ZIOSpecDefault {

  val pgCompatibleOffset = Gen
    // postgres disallow zone offsets greater than 16h or smaller than -16h
    .int(DateTimeConstants.SECONDS_PER_HOUR * -16, DateTimeConstants.SECONDS_PER_HOUR * 16)
    .map(seconds => ZoneOffset.ofTotalSeconds(seconds / 60 * 60))

  override def spec = {
    suite("datetime serialization")(
      test("datetime written with joda datetime codec should be readable with javatime codec and should be UTC") {
        check(
          Gen
            .instant(
              min = Instant.EPOCH,
              max = Instant.parse("9999-01-01T00:00:00Z")
            )
            .map(_.truncatedTo(ChronoUnit.SECONDS)),
          pgCompatibleOffset
        ) { (instant, offset) =>
          val input    = instant.atOffset(offset)
          val dateTime = input.toJodaDateTime
          (for {
            transactor <- ZIO.service[transactor.Transactor[Task]]
            _          <- sql"create table test_tz (lastLogin timestamp with time zone)".update.run.transact(transactor)
            _          <- {
              import com.normation.rudder.db.Doobie.DateTimeMeta
              sql"insert into test_tz (lastLogin) values ($dateTime)".update.run.transact(transactor)
            }
            actual     <- {
              import doobie.postgres.implicits.JavaOffsetDateTimeMeta
              sql"select lastLogin from test_tz".query[OffsetDateTime].unique.transact(transactor)
            }
          } yield assert(actual)(equalTo(instant.atOffset(ZoneOffset.UTC))))
            .provideSomeAuto(dbLayer)
        }
      },
      test("datetime written with javatime datetime codec should be readable with joda codec and should be UTC") {
        check(
          Gen
            .instant(
              min = Instant.EPOCH,
              max = Instant.parse("9999-01-01T00:00:00Z")
            )
            .map(_.truncatedTo(ChronoUnit.SECONDS)),
          pgCompatibleOffset
        ) { (instant, offset) =>
          val input = instant.atOffset(offset)
          (for {
            transactor <- ZIO.service[transactor.Transactor[Task]]
            _          <- sql"create table test_tz (lastLogin timestamp with time zone)".update.run.transact(transactor)
            _          <- {
              import doobie.postgres.implicits.JavaOffsetDateTimeMeta
              sql"insert into test_tz (lastLogin) values ($input)".update.run.transact(transactor)
            }
            actual     <- {
              import com.normation.rudder.db.Doobie.DateTimeMeta
              sql"select lastLogin from test_tz".query[DateTime].unique.transact(transactor)
            }
          } yield assert(actual.toOffsetDateTime)(equalTo(instant.atOffset(ZoneOffset.UTC))))
            .provideSomeAuto(dbLayer)
        }
      }
    )
      .provideShared(PostgresContainer.containerLayer)
  } @@ TestAspect.shrinks(0)

}

object PostgresContainer {
  trait DbAdmin {
    private[PostgresContainer] def createDatabase: ZIO[Scope, Nothing, transactor.Transactor[Task]]
  }

  val containerLayer: ULayer[DbAdmin] = ZLayer.scoped {
    for {
      container            <- ZIO.attemptBlocking {
                                val container = PostgreSQLContainer(dockerImageNameOverride = DockerImageName.parse("postgres:18"))
                                container.start()
                                container
                              }.withFinalizerAuto.orDie
      schemalessTransactor <- makeTransactor(
                                host = container.host,
                                port = container.mappedPort(5432),
                                name = container.databaseName,
                                login = container.username,
                                password = container.password,
                                connectionPoolSize = 10
                              )

    } yield new DbAdmin {
      override val createDatabase: ZIO[Scope, Nothing, transactor.Transactor[Task]] = {
        for {
          schemaName       <- Live.live {
                                Random.nextUUID.map(_.toString.filterNot(_ == '-')).map(name => s"schema$name")
                              }
          _                <- (sql"""create schema """ ++ Fragment.const(schemaName)).update.run.transact(schemalessTransactor).orDie
          schemaTransactor <- makeTransactor(
                                host = container.host,
                                port = container.mappedPort(5432),
                                name = container.databaseName,
                                login = container.username,
                                password = container.password,
                                connectionPoolSize = 10,
                                schema = Some(schemaName)
                              )
        } yield schemaTransactor
      }
    }
  }

  val dbLayer: ZLayer[DbAdmin, Nothing, transactor.Transactor[Task]] = ZLayer.scoped {
    ZIO.serviceWithZIO[DbAdmin](_.createDatabase)
  }

  private def makeTransactor(
      port:               Int,
      host:               String,
      name:               String,
      login:              String,
      password:           String,
      connectionPoolSize: Int,
      schema:             Option[String] = None
  ): ZIO[Scope, Nothing, transactor.Transactor[Task]] = {
    val hconfig = new HikariConfig
    hconfig.setDriverClassName("org.postgresql.Driver")
    hconfig.setJdbcUrl(s"jdbc:postgresql://${host}:${port}/${name}")
    hconfig.setUsername(login)
    hconfig.setPassword(password)
    hconfig.setMaximumPoolSize(connectionPoolSize)
    schema.foreach(schema => hconfig.setSchema(schema))

    for {
      executionContext <- blockingExecutor.map(_.asExecutionContext)
      datasource       <- ZIO.succeed(new HikariDataSource(hconfig)).withFinalizerAuto
    } yield DoobieIO(datasource).xa
  }

}
