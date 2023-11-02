/*
 *************************************************************************************
 * Copyright 2023 Normation SAS
 *************************************************************************************
 *
 * This file is part of Rudder.
 *
 * Rudder is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In accordance with the terms of section 7 (7. Additional Terms.) of
 * the GNU General Public License version 3, the copyright holders add
 * the following Additional permissions:
 * Notwithstanding to the terms of section 5 (5. Conveying Modified Source
 * Versions) and 6 (6. Conveying Non-Source Forms.) of the GNU General
 * Public License version 3, when you create a Related Module, this
 * Related Module is not considered as a part of the work and may be
 * distributed under the license agreement of your choice.
 * A "Related Module" means a set of sources files including their
 * documentation that, without modification of the Source Code, enables
 * supplementary functions or services in addition to those offered by
 * the Software.
 *
 * Rudder is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Rudder.  If not, see <http://www.gnu.org/licenses/>.

 *
 *************************************************************************************
 */

package bootstrap.liftweb.checks.migration

import bootstrap.liftweb.BootstrapChecks
import bootstrap.liftweb.BootstrapLogger
import com.normation.errors.IOResult
import com.normation.rudder.db.Doobie
import com.normation.zio._
import doobie.implicits._
import doobie.util.fragment.Fragment
import zio.interop.catz._

/*
 * Ths migration applies a change in the schema of the event logs table :
 *  - add NOT NULL constraint to : eventtype, principal, severity, data
 *  - add default values to recover from potentially missing data, handling that case in the business logic
 */
class MigrateEventLogEnforceSchema(
    doobie: Doobie
) extends BootstrapChecks {

  import doobie._

  val msg: String = "eventLog columns that should be NOT NULL (eventtype, principal, severity, data)"

  override def description: String =
    "Check if eventtype, principal, severity, data have a NOT NULL constraint, otherwise migrate these columns"

  private val defaultSeverity  = Fragment.const("100")
  private val defaultPrincipal = Fragment.const("'unknown'")

  private def alterTableStatement: IOResult[Unit] = {
    val sql = {
      sql"""
        -- Alter the EventLog schema for the 'eventType' column
        ALTER TABLE IF EXISTS EventLog
        ALTER COLUMN eventType TYPE text USING COALESCE(eventType, ''),
        ALTER COLUMN eventType SET DEFAULT '',
        ALTER COLUMN eventType SET NOT NULL;

        -- Alter the EventLog schema for the 'principal' column
        ALTER TABLE IF EXISTS EventLog
        ALTER COLUMN principal TYPE text USING COALESCE(principal, ${defaultPrincipal}),
        ALTER COLUMN principal SET DEFAULT ${defaultPrincipal},
        ALTER COLUMN principal SET NOT NULL;

        -- Alter the EventLog schema for the 'severity' column
        ALTER TABLE IF EXISTS EventLog
        ALTER COLUMN severity TYPE integer USING COALESCE(severity, ${defaultSeverity}),
        ALTER COLUMN severity SET DEFAULT ${defaultSeverity},
        ALTER COLUMN severity SET NOT NULL;

        -- Alter the EventLog schema for the 'data' column
        ALTER TABLE IF EXISTS EventLog
        ALTER COLUMN data TYPE xml USING COALESCE(data, ''),
        ALTER COLUMN data SET DEFAULT '',
        ALTER COLUMN data SET NOT NULL;
      """
    }

    transactIOResult(s"Error with 'EventLog' table migration")(xa => sql.update.run.transact(xa)).unit
  }

  override def checks(): Unit = {
    val prog = {
      for {
        _ <- alterTableStatement
        _ <- BootstrapLogger.info(s"Migrated ${msg}")
      } yield ()
    }

    prog.catchAll(err => BootstrapLogger.error(s"Error when trying to migrate ${msg}: ${err.fullMsg}")).runNow
  }

}