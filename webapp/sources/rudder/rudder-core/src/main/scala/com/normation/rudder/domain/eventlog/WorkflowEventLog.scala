package com.normation.rudder.domain.eventlog

import com.normation.eventlog._
sealed trait WorkflowEventLog extends EventLog {
  final override val eventLogCategory: com.normation.rudder.domain.eventlog.WorkflowLogCategory.type = WorkflowLogCategory
}

final case class WorkflowStepChanged(
    override val eventDetails: EventLogDetails
) extends WorkflowEventLog {
  override val cause:     None.type                                                              = None
  override val eventType: com.normation.rudder.domain.eventlog.WorkflowStepChangedEventType.type = WorkflowStepChanged.eventType
}

object WorkflowStepChanged extends EventLogFilter {
  override val eventType: com.normation.rudder.domain.eventlog.WorkflowStepChangedEventType.type = WorkflowStepChangedEventType

  override def apply(x: (EventLogType, EventLogDetails)): WorkflowStepChanged = WorkflowStepChanged(x._2)
}
