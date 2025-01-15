/*
 *************************************************************************************
 * Copyright 2021 Normation SAS
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

package com.normation.plugins

import better.files.File
import com.normation.errors.*
import com.normation.plugins.RudderPackageService.*
import com.normation.rudder.domain.logger.ApplicationLoggerPure
import com.normation.rudder.hooks.Cmd
import com.normation.rudder.hooks.CmdResult
import com.normation.rudder.hooks.RunNuCommand
import com.normation.utils.DateFormaterService
import com.normation.utils.Version
import enumeratum.Enum
import enumeratum.EnumEntry
import enumeratum.EnumEntry.*
import io.scalaland.chimney.Transformer
import io.scalaland.chimney.syntax.*
import java.time.ZonedDateTime
import java.util.Properties
import org.joda.time.DateTime
import zio.*
import zio.json.*
import zio.syntax.*

case class PluginSettings(
    url:           Option[String],
    username:      Option[String],
    password:      Option[String],
    proxyUrl:      Option[String],
    proxyUser:     Option[String],
    proxyPassword: Option[String]
) {
  def isDefined: Boolean = {
    // Also, the special case : username="username" is empty
    val hasDefaultUser = username.contains("username")
    !isEmpty && !hasDefaultUser
  }

  // Strings are in fact non-empty strings
  private def isEmpty = url.isEmpty &&
    username.isEmpty &&
    password.isEmpty &&
    proxyUrl.isEmpty &&
    proxyUser.isEmpty &&
    proxyPassword.isEmpty
}

/*
 * Information about registered plugins that can be used
 * for API and monitoring things.
 */
final case class JsonPluginsDetails(
    globalLimits: Option[JsonGlobalPluginLimits],
    // plugins should be sorted by id
    details:      Seq[JsonPluginDetails]
)
object JsonPluginsDetails {
  implicit val encoderJsonPluginsDetails: JsonEncoder[JsonPluginsDetails] = DeriveJsonEncoder.gen

  def buildDetails(plugins: Seq[JsonPluginDetails]): JsonPluginsDetails = {
    val limits = JsonGlobalPluginLimits.getGlobalLimits(plugins.flatMap(_.license))
    JsonPluginsDetails(limits, plugins)
  }
}

/*
 * Global limit information about plugins (the most restrictive)
 */
final case class JsonGlobalPluginLimits(
    licensees: Option[NonEmptyChunk[String]],
    // for now, min/max version is not used and is always 00-99
    startDate: Option[ZonedDateTime],
    endDate:   Option[ZonedDateTime],
    maxNodes:  Option[Int]
) {
  import JsonGlobalPluginLimits.*
  def combine(that: JsonGlobalPluginLimits): JsonGlobalPluginLimits = {
    // for efficiency : check equality and hash first before field comparison,
    // as it will mostly be the case because license information should be similar
    if (this == that) this
    else {
      JsonGlobalPluginLimits(
        comp[NonEmptyChunk[String]](this.licensees, that.licensees, _ ++ _),
        comp[ZonedDateTime](this.startDate, that.startDate, (x, y) => if (x.isAfter(y)) x else y),
        comp[ZonedDateTime](this.endDate, that.endDate, (x, y) => if (x.isBefore(y)) x else y),
        comp[Int](this.maxNodes, that.maxNodes, (x, y) => if (x < y) x else y)
      )
    }
  }
}
object JsonGlobalPluginLimits {
  import DateFormaterService.JodaTimeToJava
  import DateFormaterService.json.encoderZonedDateTime
  implicit val encoderGlobalPluginLimits: JsonEncoder[JsonGlobalPluginLimits] = DeriveJsonEncoder.gen

  def fromLicenseInfo(info: PluginLicenseInfo): JsonGlobalPluginLimits = {
    JsonGlobalPluginLimits(
      Some(NonEmptyChunk(info.licensee)),
      Some(info.startDate.toJava),
      Some(info.endDate.toJava),
      info.maxNodes
    )
  }

  def empty = JsonGlobalPluginLimits(None, None, None, None)
  // from a list of plugins, create the global limits
  def getGlobalLimits(licenses: Seq[PluginLicenseInfo]): Option[JsonGlobalPluginLimits] = {
    NonEmptyChunk
      .fromIterableOption(licenses)
      .map(getGlobalLimits(_))
      .flatMap(r => Option.when(r != empty)(r))
  }

  def getGlobalLimits(licenses: NonEmptyChunk[PluginLicenseInfo]): JsonGlobalPluginLimits = {
    val res                     = licenses.reduceMapLeft(fromLicenseInfo) { case (lim, lic) => lim.combine(fromLicenseInfo(lic)) }
    val sortedDistinctLicensees = res.licensees.map(_.sorted.distinct).flatMap(NonEmptyChunk.fromChunk)
    res.copy(licensees = sortedDistinctLicensees)
  }

  private def comp[A](a: Option[A], b: Option[A], compare: (A, A) => A): Option[A] = (a, b) match {
    case (None, None)       => None
    case (Some(x), None)    => Some(x)
    case (None, Some(y))    => Some(y)
    case (Some(x), Some(y)) => Some(compare(x, y))
  }
}

sealed trait PluginSystemStatus {
  def value: String
}
object PluginSystemStatus       {
  case object Enabled  extends PluginSystemStatus { override val value: String = "enabled"  }
  case object Disabled extends PluginSystemStatus { override val value: String = "disabled" }

  implicit val transformerJson: Transformer[PluginSystemStatus, JsonPluginSystemStatus] =
    Transformer.derive[PluginSystemStatus, JsonPluginSystemStatus]
}

final case class JsonPluginDetails(
    id:            String,
    name:          String,
    shortName:     String,
    description:   String,
    version:       String,
    status:        PluginSystemStatus,
    statusMessage: Option[String],
    license:       Option[PluginLicenseInfo]
)
object JsonPluginDetails        {
  implicit val encoderPluginSystemStatusRest: JsonEncoder[PluginSystemStatus] = JsonEncoder[String].contramap(_.value)
  implicit val encoderPluginDetails:          JsonEncoder[JsonPluginDetails]  = DeriveJsonEncoder.gen
}

/*
 * This object gives main information about license information.
 * It is designated to be read to the user. No string information
 * should be used for comparison.
 */
final case class PluginLicenseInfo(
    licensee:   String,
    softwareId: String,
    minVersion: String,
    maxVersion: String,
    startDate:  DateTime,
    endDate:    DateTime,
    maxNodes:   Option[Int],
    @jsonField("additionalInfo")
    others:     Map[String, String]
)
object PluginLicenseInfo {
  import DateFormaterService.json.encoderDateTime
  implicit val encoder: JsonEncoder[PluginLicenseInfo] = DeriveJsonEncoder.gen
}

trait PluginSettingsService {
  def checkIsSetup():       IOResult[Boolean]
  def readPluginSettings(): IOResult[PluginSettings]
  def writePluginSettings(settings: PluginSettings): IOResult[Unit]
}

class FilePluginSettingsService(pluginConfFile: File, readSetupDone: IOResult[Boolean], writeSetupDone: Boolean => IOResult[Unit])
    extends PluginSettingsService {

  /**
    * Watch the rudder_setup_done setting to see if the plugin settings has been setup.
    * It has the side effect of updating the `rudder_setup_done` setting.
    *
    * @return the boolean with the semantics of :
    *  rudder_setup_done && !(is_setting_default || is_setting_empty)
    * and false when the plugin settings are not set, and setup is not done
    */
  def checkIsSetup(): IOResult[Boolean] = {
    readSetupDone
      .flatMap(isSetupDone => {
        if (isSetupDone) {
          true.succeed
        } else {
          // we may need to update setup_done if settings are defined
          readPluginSettings().map(_.isDefined).flatMap {
            case true  =>
              ApplicationLoggerPure.info(
                s"Read plugin settings properties file ${pluginConfFile.pathAsString} with a defined configuration, rudder_setup_done setting is marked as `true`. Go to Rudder Setup page to change the account credentials."
              ) *> writeSetupDone(true).as(true)
            case false =>
              // the plugin settings are not set, setup is not done
              false.succeed
          }
        }
      })
      .tapError(err => ApplicationLoggerPure.error(s"Could not get setting `rudder_setup_done` : ${err.fullMsg}"))
  }

  def readPluginSettings(): IOResult[PluginSettings] = {

    val p = new Properties()
    for {
      _ <- IOResult.attempt(s"Reading properties from ${pluginConfFile.pathAsString}")(p.load(pluginConfFile.newInputStream))

      url            <- IOResult.attempt(s"Getting plugin repository url in ${pluginConfFile.pathAsString}") {
                          val res = p.getProperty("url", "")
                          if (res == "") None else Some(res)
                        }
      userName       <-
        IOResult.attempt(s"Getting user name for plugin download in ${pluginConfFile.pathAsString}") {
          val res = p.getProperty("username", "")
          if (res == "") None else Some(res)
        }
      pass           <-
        IOResult.attempt(s"Getting password for plugin download in ${pluginConfFile.pathAsString}") {
          val res = p.getProperty("password", "")
          if (res == "") None else Some(res)
        }
      proxy          <- IOResult.attempt(s"Getting proxy for plugin download in ${pluginConfFile.pathAsString}") {
                          val res = p.getProperty("proxy_url", "")
                          if (res == "") None else Some(res)
                        }
      proxy_user     <- IOResult.attempt(s"Getting proxy for plugin download in ${pluginConfFile.pathAsString}") {
                          val res = p.getProperty("proxy_user", "")
                          if (res == "") None else Some(res)
                        }
      proxy_password <- IOResult.attempt(s"Getting proxy for plugin download in ${pluginConfFile.pathAsString}") {
                          val res = p.getProperty("proxy_password", "")
                          if (res == "") None else Some(res)
                        }
    } yield {
      PluginSettings(url, userName, pass, proxy, proxy_user, proxy_password)
    }
  }

  def writePluginSettings(update: PluginSettings): IOResult[Unit] = {
    for {
      base <- readPluginSettings()
      _    <- IOResult.attempt({
                val settings = base.copy(
                  url = update.url orElse base.url,
                  username = update.username orElse base.username,
                  password = update.password orElse base.password,
                  proxyUrl = update.proxyUrl orElse base.proxyUrl,
                  proxyUser = update.proxyUser orElse base.proxyUser,
                  proxyPassword = update.proxyPassword orElse base.proxyPassword
                )
                pluginConfFile.write(s"""[Rudder]
                                     |url = ${settings.url.getOrElse("")}
                                     |username = ${settings.username.getOrElse("")}
                                     |password = ${settings.password.getOrElse("")}
                                     |proxy_url = ${settings.proxyUrl.getOrElse("")}
                                     |proxy_user = ${settings.proxyUser.getOrElse("")}
                                     |proxy_password = ${settings.proxyPassword.getOrElse("")}
                                     |""".stripMargin)
              })
    } yield {}
  }
}

sealed trait PluginType extends Lowercase
object PluginType       extends Enum[PluginType] {
  case object Webapp      extends PluginType
  case object Integration extends PluginType

  override def values: IndexedSeq[PluginType] = findValues
}

sealed trait JsonPluginSystemStatus extends Lowercase
object JsonPluginSystemStatus       extends Enum[JsonPluginSystemStatus] {
  case object Enabled     extends JsonPluginSystemStatus
  case object Disabled    extends JsonPluginSystemStatus
  case object Uninstalled extends JsonPluginSystemStatus

  override def values: IndexedSeq[JsonPluginSystemStatus] = findValues

  implicit val encoder: JsonEncoder[JsonPluginSystemStatus] = JsonEncoder[String].contramap(_.entryName)
}

/**
 * An error enumeration to identify plugin management errors and the associated messages
 */
sealed trait PluginManagementError {
  def kind:       PluginManagementError.Kind
  def displayMsg: String
}
object PluginManagementError       {
  sealed trait Kind extends EnumEntry with Dotcase
  object Kind       extends Enum[Kind] {
    case object LicenseNeededError         extends Kind
    case object LicenseExpiredError        extends Kind
    case object LicenseNearExpirationError extends Kind
    case object AbiVersionError            extends Kind
    override def values: IndexedSeq[Kind] = findValues
  }

  case object LicenseNeededError extends PluginManagementError {
    override def kind:       Kind.LicenseNeededError.type = Kind.LicenseNeededError
    override def displayMsg: String                       = "A license is needed for the plugin"
  }

  /**
    * Sum type for license expiration error with case disjunction
    */
  sealed trait LicenseExpirationError    extends PluginManagementError
  case object LicenseExpiredError        extends LicenseExpirationError {
    override def kind:       Kind.LicenseExpiredError.type = Kind.LicenseExpiredError
    override def displayMsg: String                        = "Plugin license error require your attention"
  }
  case object LicenseNearExpirationError extends LicenseExpirationError {
    override def kind:       Kind.LicenseNearExpirationError.type = Kind.LicenseNearExpirationError
    override def displayMsg: String                               = "Plugin license near expiration"
  }

  final case class RudderAbiVersionError(rudderFullVersion: String) extends PluginManagementError {
    override def kind:       Kind.AbiVersionError.type = Kind.AbiVersionError
    override def displayMsg: String                    =
      s"This plugin was not built for current Rudder ABI version ${rudderFullVersion}. You should update it to avoid code incompatibilities."
  }

  def fromRudderPackagePlugin(
      plugin: RudderPackagePlugin
  )(implicit rudderFullVersion: String, abiVersion: Version): List[PluginManagementError] = {
    List(
      validateAbiVersion(rudderFullVersion, abiVersion),
      validateLicenseNeeded(plugin.requiresLicense, plugin.license),
      plugin.license.flatMap(l => validateLicenseExpiration(l.endDate))
    ).flatten
  }

  private def validateAbiVersion(rudderFullVersion: String, abiVersion: Version): Option[RudderAbiVersionError] = {
    if (rudderFullVersion != abiVersion.toVersionString)
      Some(RudderAbiVersionError(rudderFullVersion))
    else
      None
  }

  private def validateLicenseNeeded(
      requiresLicense: Boolean,
      license:         Option[RudderPackagePlugin.LicenseInfo]
  ): Option[LicenseNeededError.type] = {
    if (requiresLicense && license.isEmpty)
      Some(LicenseNeededError)
    else
      None
  }

  /**
   * license near expiration : 1 month before now.
   */
  private def validateLicenseExpiration(endDate: DateTime): Option[LicenseExpirationError] = {
    if (endDate.isBeforeNow()) {
      Some(LicenseExpiredError)
    } else if (endDate.minusMonths(1).isBeforeNow())
      Some(LicenseNearExpirationError)
    else
      None
  }

}

case class PluginId(value: String) extends AnyVal
object PluginId                  {
  implicit val decoder:     JsonDecoder[PluginId]         = JsonDecoder[String].mapOrFail(parse)
  implicit val encoder:     JsonEncoder[PluginId]         = JsonEncoder[String].contramap(_.value)
  implicit val transformer: Transformer[PluginId, String] = Transformer.derive[PluginId, String]

  private val pluginIdRegex = """^(\p{Alnum}[\p{Alnum}-_]*)$""".r

  /**
    * Ensure that plugin ID is alpha-num and hyphen
    */
  def parse(s: String): Either[String, PluginId] = {
    s match {
      case pluginIdRegex(_) => Right(PluginId(s))
      case _                => Left(s"Invalid plugin ID: '$s'. Plugin ID must be alphanumeric with hyphens.")
    }
  }
}

final case class JsonPluginsSystemDetails(
    license: Option[JsonGlobalPluginLimits],
    plugins: Chunk[JsonPluginSystemDetails]
)
object JsonPluginsSystemDetails  {
  import JsonPluginSystemDetails.*

  implicit val encoder: JsonEncoder[JsonPluginsSystemDetails] = DeriveJsonEncoder.gen[JsonPluginsSystemDetails]

  def buildDetails(plugins: Chunk[JsonPluginSystemDetails]): JsonPluginsSystemDetails = {
    val limits = JsonGlobalPluginLimits.getGlobalLimits(plugins.flatMap(_.license))
    JsonPluginsSystemDetails(limits, plugins)
  }
}
final case class JsonPluginSystemDetails(
    id:            PluginId,
    name:          String,
    description:   String,
    version:       Option[String],
    status:        JsonPluginSystemStatus,
    statusMessage: Option[String],
    abiVersion:    Version,
    pluginType:    PluginType,
    errors:        List[JsonPluginManagementError],
    license:       Option[PluginLicenseInfo]
)

final case class JsonPluginManagementError(
    error:   String,
    message: String
)
object JsonPluginManagementError {
  implicit val transformer: Transformer[PluginManagementError, JsonPluginManagementError] = {
    Transformer
      .define[PluginManagementError, JsonPluginManagementError]
      .withFieldComputed(_.error, _.kind.entryName)
      .withFieldComputed(_.message, _.displayMsg)
      .buildTransformer
  }
}

object JsonPluginSystemDetails {
  implicit val encoderPluginId:              JsonEncoder[PluginId]                  = JsonEncoder[String].contramap(_.value)
  implicit val encoderPluginType:            JsonEncoder[PluginType]                = JsonEncoder[String].contramap(_.entryName)
  implicit val encoderPluginManagementError: JsonEncoder[JsonPluginManagementError] =
    DeriveJsonEncoder.gen[JsonPluginManagementError]
  implicit val encoderVersion:               JsonEncoder[Version]                   = JsonEncoder[String].contramap(_.toVersionString)
  implicit val encoderPluginSystemDetails:   JsonEncoder[JsonPluginSystemDetails]   = DeriveJsonEncoder.gen[JsonPluginSystemDetails]
}

@jsonMemberNames(SnakeCase)
final case class RudderPackagePlugin(
    name:            String,
    version:         Option[String],
    latestVersion:   Option[String],
    installed:       Boolean,
    enabled:         Boolean,
    webappPlugin:    Boolean,
    requiresLicense: Boolean,
    description:     String,
    license:         Option[RudderPackagePlugin.LicenseInfo]
)
object RudderPackagePlugin     {
  // types for passing implicits
  final case class Licensee(value: String)      extends AnyVal
  final case class SoftwareId(value: String)    extends AnyVal
  final case class MinVersion(value: String)    extends AnyVal
  final case class MaxVersion(value: String)    extends AnyVal
  final case class MaxNodes(value: Option[Int]) extends AnyVal

  // License representation is limited to these fields in rudder package
  @jsonMemberNames(SnakeCase)
  final case class LicenseInfo(
      startDate: DateTime,
      endDate:   DateTime
  )
  object LicenseInfo {
    import DateFormaterService.json.decoderDateTime
    implicit val decoder: JsonDecoder[LicenseInfo] = DeriveJsonDecoder.gen[LicenseInfo]
    implicit def transformer(implicit
        licensee:   Licensee,
        softwareId: SoftwareId,
        minVersion: MinVersion,
        maxVersion: MaxVersion,
        maxNodes:   MaxNodes
    ): Transformer[LicenseInfo, PluginLicenseInfo] = {
      Transformer
        .define[LicenseInfo, PluginLicenseInfo]
        .withFieldConst(_.licensee, licensee.value)
        .withFieldConst(_.softwareId, softwareId.value)
        .withFieldConst(_.minVersion, minVersion.value)
        .withFieldConst(_.maxVersion, maxVersion.value)
        .withFieldConst(_.maxNodes, maxNodes.value)
        .withFieldConst(_.others, Map.empty[String, String])
        .buildTransformer
    }
  }

  implicit val decoder: JsonDecoder[RudderPackagePlugin] = DeriveJsonDecoder.gen[RudderPackagePlugin]

  /**
    * When joining plugin information from rudder package and global information from registered plugins,
    * we can return needed plugin details
    */
  implicit def transformer(implicit
      rudderFullVersion: String,
      abiVersion:        Version,
      transformLicense:  Transformer[LicenseInfo, PluginLicenseInfo]
  ): Transformer[RudderPackagePlugin, JsonPluginSystemDetails] = {
    val _ = transformLicense // variable is used below
    Transformer
      .define[RudderPackagePlugin, JsonPluginSystemDetails]
      .withFieldComputed(_.id, p => PluginId(p.name))
      .withFieldComputed(
        _.status,
        p => {
          (p.installed, p.enabled) match {
            case (true, true)  => JsonPluginSystemStatus.Enabled
            case (true, false) => JsonPluginSystemStatus.Disabled
            case (false, _)    => JsonPluginSystemStatus.Uninstalled
          }
        }
      )
      .withFieldComputed(_.version, l => l.version.orElse(l.latestVersion)) // version : only when installed
      .withFieldConst(_.abiVersion, abiVersion) // field is computed upstream
      .withFieldComputed(_.pluginType, p => if (p.webappPlugin) PluginType.Webapp else PluginType.Integration)
      .withFieldConst(_.statusMessage, None)
      .withFieldComputed(
        _.errors,
        PluginManagementError.fromRudderPackagePlugin(_).map(_.transformInto[JsonPluginManagementError])
      )
      .buildTransformer
  }
}

/**
  * A service that encapsulate rudder package operations and its representation of plugins.
  */
trait RudderPackageService {
  import RudderPackageService.*

  def updateBase(): IOResult[Option[CredentialError]]

  def listAllPlugins(): IOResult[Chunk[RudderPackagePlugin]]

  def installPlugins(plugins: Chunk[String]): IOResult[Unit]

  def removePlugins(plugins:           Chunk[String]): IOResult[Unit]
  def changePluginSystemStatus(status: PluginSystemStatus, plugins: Chunk[String]): IOResult[Unit]
}

object RudderPackageService {

  val ERROR_CODE: Int = 1

  // see PluginSettings : the url and credentials configuration could cause errors :
  final case class CredentialError(msg: String) extends RudderError

  object CredentialError {
    private val regex = "^.*ERROR.* (Received an HTTP 401 Unauthorized error.*)$".r

    def fromResult(cmdResult: CmdResult): Option[CredentialError] = {
      (cmdResult.code, cmdResult.stderr.strip) match { // do not forget to strip stderr
        case (ERROR_CODE, regex(err)) => Some(CredentialError(err))
        case _                        => None
      }
    }
  }

}

/*
 * We assume that command in config is in format: `/path/to/main/command args1 args2 etc`
 */
class RudderPackageCmdService(configCmdLine: String) extends RudderPackageService {

  val configCmdRes = configCmdLine.split(" ").toList match {
    case Nil       => Left(Unexpected(s"Invalid command for rudder package from configuration: '${configCmdLine}'"))
    case h :: tail => Right((h, tail))
  }

  override def updateBase(): IOResult[Option[CredentialError]] = {
    // In case of error we need to check the result
    for {
      res          <- runCmd("update" :: Nil)
      (cmd, result) = res
      err           = CredentialError.fromResult(result)
      _            <- ZIO.when(result.code != 0 && err.isEmpty) {
                        Inconsistency(
                          s"An error occurred while updating plugins list with '${cmd.display}':\n code: ${result.code}\n stderr: ${result.stderr}\n stdout: ${result.stdout}"
                        ).fail
                      }
    } yield {
      err
    }
  }

  override def listAllPlugins(): IOResult[Chunk[RudderPackagePlugin]] = {
    for {
      result  <- runCmdOrFail("list" :: "--all" :: "--format=json" :: Nil)(
                   s"An error occurred while listing packages"
                 )
      plugins <- result.stdout
                   .fromJson[Chunk[RudderPackagePlugin]]
                   .toIO
                   .chainError("Could not parse plugins definition")
    } yield {
      plugins
    }
  }

  override def installPlugins(plugins: Chunk[String]): IOResult[Unit] = {
    runCmdOrFail("install" :: plugins.toList)(
      s"An error occurred while installing plugins"
    ).unit
  }

  override def removePlugins(plugins: Chunk[String]): IOResult[Unit] = {
    runCmdOrFail("remove")(
      s"An error occurred while removing plugins"
    ).unit
  }

  override def changePluginSystemStatus(status: PluginSystemStatus, plugins: Chunk[String]): IOResult[Unit] = {
    runCmdOrFail(status.value :: plugins.toList)(
      s"An error occurred while changin plugin status to ${status.value}"
    ).unit
  }

  private def runCmd(params: List[String]):                         IOResult[(Cmd, CmdResult)] = {
    for {
      configCmd   <- configCmdRes.toIO
      cmd          = Cmd(configCmd._1, configCmd._2 ::: params, Map.empty, None)
      packagesCmd <- RunNuCommand.run(cmd)
      result      <- packagesCmd.await
    } yield {
      (cmd, result)
    }
  }
  private def runCmdOrFail(params: String*)(errorMsg: String):      IOResult[CmdResult]        = {
    runCmdOrFail(params.toList)(errorMsg)
  }
  private def runCmdOrFail(params: List[String])(errorMsg: String): IOResult[CmdResult]        = {
    runCmd(params).reject {
      case (cmd, result) if result.code != 0 =>
        Inconsistency(
          s"${errorMsg} with '${cmd.display}':\n code: ${result.code}\n stderr: ${result.stderr}\n stdout: ${result.stdout}"
        )
    }.map(_._2)
  }
}

/**
  * A service to manage plugins, it is an abstraction over system administration of plugins
  */
trait PluginSystemService {

  def list(): IOResult[Either[CredentialError, Chunk[JsonPluginSystemDetails]]]

  def install(plugins:     Chunk[PluginId]): IOResult[Unit]
  def remove(plugins:      Chunk[PluginId]): IOResult[Unit]
  def updateStatus(status: PluginSystemStatus, plugins: Chunk[PluginId]): IOResult[Unit]

}

/**
  * Implementation for tests, will do any operation without any error
  */
class InMemoryPluginSystemService(ref: Ref[Map[PluginId, JsonPluginSystemDetails]]) extends PluginSystemService {
  override def list(): UIO[Either[CredentialError, Chunk[JsonPluginSystemDetails]]] = {
    ref.get.map(m => Right(Chunk.fromIterable(m.values)))
  }

  override def install(plugins: Chunk[PluginId]): UIO[Unit] = {
    updatePluginStatus(JsonPluginSystemStatus.Enabled, plugins)
  }

  override def remove(plugins: Chunk[PluginId]): UIO[Unit] = {
    updatePluginStatus(JsonPluginSystemStatus.Uninstalled, plugins)
  }

  override def updateStatus(status: PluginSystemStatus, plugins: Chunk[PluginId]): UIO[Unit] = {
    updatePluginStatus(status.transformInto[JsonPluginSystemStatus], plugins)
  }

  private def updatePluginStatus(status: JsonPluginSystemStatus, plugins: Chunk[PluginId]) = {
    ref.update(m => m ++ plugins.flatMap(id => m.get(id).map(p => id -> p.copy(status = status))))
  }
}

object InMemoryPluginSystemService {
  def make(initialPlugins: List[JsonPluginSystemDetails]): UIO[InMemoryPluginSystemService] = {
    for {
      ref <- Ref.make(initialPlugins.map(p => p.id -> p).toMap)
    } yield new InMemoryPluginSystemService(ref)
  }
}
