/*
 *************************************************************************************
 * Copyright 2011 Normation SAS
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

package com.normation.rudder.web.services

import com.normation.eventlog.EventActor
import com.normation.eventlog.EventLog
import com.normation.eventlog.EventLogDetails
import com.normation.eventlog.ModificationId
import com.normation.rudder.domain.eventlog._
import com.normation.rudder.repository.EventLogRepository
import com.normation.utils.StringUuidGenerator
import com.normation.zio._
import net.liftweb.common.Loggable
import org.springframework.context.ApplicationEvent
import org.springframework.context.ApplicationListener
import org.springframework.security.authentication.event.AuthenticationFailureBadCredentialsEvent
import org.springframework.security.authentication.event.AuthenticationSuccessEvent
import org.springframework.security.core.userdetails.UserDetails

/**
 * A class used to log user session creation/destruction events.
 * This linked to Spring Security, and so to Spring to.
 * That creates a hard reference from/to spring for our app,
 * hopefully a simple one to break.
 */
class UserSessionLogEvent(
    repository: EventLogRepository,
    uuidGen:    StringUuidGenerator
) extends ApplicationListener[ApplicationEvent] with Loggable {

  def onApplicationEvent(event: ApplicationEvent): Unit = {
    event match {
      case login: AuthenticationSuccessEvent =>
        login.getAuthentication.getPrincipal match {
          case u: UserDetails =>
            repository
              .saveEventLog(
                ModificationId(uuidGen.newUuid),
                LoginEventLog(
                  EventLogDetails(
                    modificationId = None,
                    principal = EventActor(u.getUsername),
                    details = EventLog.emptyDetails,
                    reason = None
                  )
                )
              )
              .runNowLogError(err => logger.error(s"Error when saving user login event log: ${err.fullMsg}"))
          case x =>
            logger.warn(
              "The application received an Authentication 'success' event with a parameter that is neither a principal nor some user details. I don't know how to log that event in database. Event parameter was: " + x
            )
        }

      case badLogin: AuthenticationFailureBadCredentialsEvent =>
        badLogin.getAuthentication.getPrincipal match {
          case u: String =>
            repository
              .saveEventLog(
                ModificationId(uuidGen.newUuid),
                BadCredentialsEventLog(
                  EventLogDetails(
                    modificationId = None,
                    principal = EventActor(u),
                    details = EventLog.emptyDetails,
                    reason = None
                  )
                )
              )
              .runNowLogError(err => logger.error(s"Error when saving user login event log: ${err.fullMsg}"))
          case x =>
            logger.warn(
              "The application received an Authentication 'bad credential' event with a parameter that is not the principal login. I don't know how to log that event in database. Event parameter was: " + x
            )
        }
      case x => // ignore
    }

  }
}
