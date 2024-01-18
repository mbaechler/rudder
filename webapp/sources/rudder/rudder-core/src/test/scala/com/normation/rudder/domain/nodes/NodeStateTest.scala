package com.normation.rudder.domain.nodes

import java.util.Locale
import scala.annotation.nowarn
import zio.test.*
import zio.test.Assertion.*

object NodeStateTest extends ZIOSpecDefault {

  private val validNames = Seq(
    "enabled",
    "ignored",
    "empty-policies",
    "initializing",
    "preparing-eol"
  )

  private val caseGen = Gen.elements[String => String](_.toLowerCase(Locale.US), _.toUpperCase(Locale.US))

  private val validNameGen = Gen.elements(validNames*).flatMap(name => caseGen.map(_.apply(name)))

  private val invalidNameGen = Gen.oneOf(Gen.string).filterNot(name => validNames.contains(name.toLowerCase(Locale.US)))

  @nowarn("cat=scala3-migration")
  val spec = suite("NodeState")(
    test("parse should be able to parse valid known entries regardless of the case")(
      check(validNameGen) { name =>
        val actual = NodeState.parse(name)
        assert(actual)(isRight(anything))
      }
    ),
    test("parse should be return Left for any unknown name")(
      check(invalidNameGen) { name =>
        val actual = NodeState.parse(name)
        assert(actual)(isLeft(anything))
      }
    ),
    test("parse error should provide possible values")(
      check(invalidNameGen) { name =>
        val actual = NodeState.parse(name)
        assert(actual)(isLeft(validNames.foldLeft(isNonEmptyString)(_ && containsString(_))))
      }
    ),
    test("labeledPairs should be backward compatible")(
      assertTrue(
        NodeState.labeledPairs ==
        List(
          NodeState.Initializing  -> "node.states.initializing",
          NodeState.Enabled       -> "node.states.enabled",
          NodeState.EmptyPolicies -> "node.states.empty-policies",
          NodeState.Ignored       -> "node.states.ignored",
          NodeState.PreparingEOL  -> "node.states.preparing-eol"
        )
      )
    )
  )

}
