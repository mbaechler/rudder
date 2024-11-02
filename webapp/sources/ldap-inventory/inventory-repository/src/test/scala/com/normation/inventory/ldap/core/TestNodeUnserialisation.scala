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

package com.normation.inventory.ldap.core

import com.normation.inventory.domain.*
import com.normation.ldap.sdk.LDAPEntry
import com.normation.zio.ZioRuntime
import com.unboundid.ldap.sdk.DN
import com.unboundid.ldap.sdk.Entry
import org.junit.runner.*
import org.specs2.mutable.*
import org.specs2.runner.*

/**
 * Test node unserialisation frome entries, in particular
 * agent types unserialisation compatibility between
 * versions and OS
 */
@RunWith(classOf[JUnitRunner])
class TestNodeUnserialisation extends Specification {
  val mapper: InventoryMapper = {
    val softwareDN = new DN("ou=Inventories, cn=rudder-configuration")
    val acceptedNodesDitImpl: InventoryDit = new InventoryDit(
      new DN("ou=Accepted Inventories, ou=Inventories, cn=rudder-configuration"),
      softwareDN,
      "Accepted inventories"
    )
    val pendingNodesDitImpl:  InventoryDit = new InventoryDit(
      new DN("ou=Pending Inventories, ou=Inventories, cn=rudder-configuration"),
      softwareDN,
      "Pending inventories"
    )
    val removedNodesDitImpl = new InventoryDit(
      new DN("ou=Removed Inventories, ou=Inventories, cn=rudder-configuration"),
      softwareDN,
      "Removed Servers"
    )
    val inventoryDitService: InventoryDitService =
      new InventoryDitServiceImpl(pendingNodesDitImpl, acceptedNodesDitImpl, removedNodesDitImpl)

    new InventoryMapper(inventoryDitService, pendingNodesDitImpl, acceptedNodesDitImpl, removedNodesDitImpl)
  }

  val dsc61Ldif: String = {
    s"""dn: nodeId=aff80e6d-68fb-43dd-9a33-a5204b7e3153,ou=Nodes,ou=Accepted Inventories,ou=Inventories,cn=rudder-configuration
       |nodeId: aff80e6d-68fb-43dd-9a33-a5204b7e3153
       |objectClass: node
       |objectClass: top
       |objectClass: windowsNode
       |osName: Windows2012R2
       |windowsRegistrationCompany: Vagrant
       |windowsKey: PN79T-M7QXW-R8FVX-FDF89-7XKCB
       |windowsId: 00252-00105-69793-AA339
       |osFullName: Microsoft Windows Server 2012 R2 Standard
       |osVersion: N/A
       |osKernelVersion: 6.3.9600
       |localAdministratorAccountName: vagrant-2012-r2
       |nodeHostname: vagrant-2012-r2
       |keyStatus: undefined
       |policyServerId: root
       |ram: 535822336
       |osArchitectureType: x86_64
       |lastLoggedUser: vagrant
       |inventoryDate: 20180716104920.000Z
       |receiveDate: 20180716135035.945Z
       |agentName: {"agentType":"dsc","version":"6.1-1.9","securityToken": {"value":"certificate","type":"certificate"}}
       |timezoneName: Pacific Standard Time
       |timezoneOffset: -0700""".stripMargin
  }

  val linux61Ldif: String = {
    """dn: nodeId=root,ou=Nodes,ou=Accepted Inventories,ou=Inventories,cn=rudder-configuration
      |objectClass: top
      |objectClass: node
      |objectClass: unixNode
      |objectClass: linuxNode
      |nodeId: root
      |localAdministratorAccountName: root
      |policyServerId: root
      |osFullName: SUSE Linux Enterprise Server 11 (x86_64)
      |osServicePack: 3
      |ram: 1572864000
      |swap: 781189120
      |lastLoggedUser: root
      |osKernelVersion: 3.0.76-0.11-default
      |osName: Suse
      |osVersion: 11
      |keyStatus: certified
      |nodeHostname: orchestrateur-3.labo.normation.com
      |osArchitectureType: x86_64
      |timezoneOffset: +0200
      |timezoneName: Europe/Paris
      |agentName: {"agentType":"cfengine-community","version":"6.1.0","securityToken":{"value":"certificate","type":"certificate"},"capabilities":["https"]}
      |inventoryDate: 20180717000031.000Z
      |receiveDate: 20180717000527.050Z
      |lastLoggedUserTime: 20000714084300.000Z""".stripMargin
  }

  val linux70Ldif: String = {
    """dn: nodeId=root,ou=Nodes,ou=Accepted Inventories,ou=Inventories,cn=rudder-configuration
      |objectClass: top
      |objectClass: node
      |objectClass: unixNode
      |objectClass: linuxNode
      |nodeId: root
      |localAdministratorAccountName: root
      |policyServerId: root
      |osFullName: SUSE Linux Enterprise Server 11 (x86_64)
      |osServicePack: 3
      |ram: 1572864000
      |swap: 781189120
      |lastLoggedUser: root
      |osKernelVersion: 3.0.76-0.11-default
      |osName: Suse
      |osVersion: 11
      |keyStatus: certified
      |nodeHostname: orchestrateur-3.labo.normation.com
      |osArchitectureType: x86_64
      |timezoneOffset: +0200
      |timezoneName: Europe/Paris
      |agentName: {"agentType":"cfengine-community","version":"6.1.0","securityToken":{"value":"certificate","type":"certificate"},"capabilities":["https"]}
      |inventoryDate: 20180717000031.000Z
      |receiveDate: 20180717000527.050Z
      |lastLoggedUserTime: 20000714084300.000Z
      |softwareUpdate: {"name":"rudder-agent","version":"7.0.0-realease","from":"yum","arch":"x86_64","kind":"none","description":"Local privilege escalation in pkexec","severity":"low","date":"2022-01-26T00:00:00Z","ids":["RHSA-2020-4566","CVE-2021-4034"]}""".stripMargin
  }

  def node(ldif: String): NodeInventory = {
    val nodeEntry = new LDAPEntry(new Entry(ldif.split("\n").toSeq*))
    ZioRuntime.unsafeRun(mapper.nodeFromEntry(nodeEntry).either).getOrElse(throw new Exception("Error when getting node"))
  }

  "Agent type " should {
    "correctly unserialize Linux node from 4_3" in {
      node(linux61Ldif).agents(0) must beEqualTo(
        AgentInfo(AgentType.CfeCommunity, Some(AgentVersion("6.1.0")), Certificate("certificate"), Set(AgentCapability("https")))
      )
    }

    "correctly unserialize DSC node from 6_1" in {
      node(dsc61Ldif).agents(0) must beEqualTo(
        AgentInfo(AgentType.Dsc, Some(AgentVersion("6.1-1.9")), Certificate("certificate"), Set())
      )
    }

    "correctly unserialize software updates node from 7_0" in {
      val date = JsonSerializers.parseSoftwareUpdateDateTime("2022-01-26T00:00:00Z")
      (date must beRight) and (node(linux70Ldif).softwareUpdates(0) must beEqualTo(
        SoftwareUpdate(
          "rudder-agent",
          Some("7.0.0-realease"),
          Some("x86_64"),
          Some("yum"),
          SoftwareUpdateKind.None,
          None,
          Some("Local privilege escalation in pkexec"),
          Some(SoftwareUpdateSeverity.Low),
          date.toOption,
          Some(List("RHSA-2020-4566", "CVE-2021-4034"))
        )
      ))
    }
  }
}
