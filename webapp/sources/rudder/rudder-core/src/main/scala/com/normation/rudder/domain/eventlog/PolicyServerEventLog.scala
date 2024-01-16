package com.normation.rudder.domain.eventlog

import com.normation.eventlog.EventLog
import com.normation.eventlog.EventLogDetails
import com.normation.eventlog.EventLogFilter
import com.normation.eventlog.EventLogType
import com.normation.rudder.domain.Constants
import scala.xml.Elem

/**
 * Update the policy server
 */

sealed trait PolicyServerEventLog extends EventLog

final case class UpdatePolicyServer(
    override val eventDetails: EventLogDetails
) extends PolicyServerEventLog {
  override val cause:            None.type                                                             = None
  override val eventType:        com.normation.rudder.domain.eventlog.UpdatePolicyServerEventType.type = UpdatePolicyServer.eventType
  override val eventLogCategory: com.normation.rudder.domain.eventlog.PolicyServerLogCategory.type     = PolicyServerLogCategory
}

final case class AuthorizedNetworkModification(
    oldNetworks: Seq[String],
    newNetworks: Seq[String]
)

object UpdatePolicyServer extends EventLogFilter {
  override val eventType: com.normation.rudder.domain.eventlog.UpdatePolicyServerEventType.type = UpdatePolicyServerEventType

  override def apply(x: (EventLogType, EventLogDetails)): UpdatePolicyServer = UpdatePolicyServer(x._2)

  def buildDetails(modification: AuthorizedNetworkModification): Elem = {
    EventLog.withContent {
      <changeAuthorizedNetworks fileFormat={Constants.XML_CURRENT_FILE_FORMAT.toString}>
        <oldAuthorizedNetworks>{
        modification.oldNetworks.map(net => <net>{net}</net>)
      }</oldAuthorizedNetworks>
        <newAuthorizedNetworks>{
        modification.newNetworks.map(net => <net>{net}</net>)
      }</newAuthorizedNetworks>
      </changeAuthorizedNetworks>
    }
  }

}

object PolicyServerEventLogsFilter {
  final val eventList: List[EventLogFilter] = List(
    UpdatePolicyServer
  )
}
