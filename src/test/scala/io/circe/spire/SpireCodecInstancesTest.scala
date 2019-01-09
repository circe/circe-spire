package io.circe.spire

import io.circe.testing.{ArbitraryInstances, CodecTests}
import org.scalatest.FlatSpec
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}
import org.typelevel.discipline.Laws
import spire.laws.arb._
import spire.math._

class SpireCodecInstancesTest extends FlatSpec with GeneratorDrivenPropertyChecks with SpireCodecs with ArbitraryInstances {

  def checkLaws(name: String, ruleSet: Laws#RuleSet): Unit = ruleSet.all.properties.zipWithIndex.foreach {
    case ((id, prop), 0) => name should s"obey $id" in Checkers.check(prop)
    case ((id, prop), _) => it should s"obey $id" in Checkers.check(prop)
  }

  checkLaws("Codec[UByte]", CodecTests[UByte].codec)
  checkLaws("Codec[UShort]", CodecTests[UShort].codec)
  checkLaws("Codec[UInt]", CodecTests[UInt].codec)
  checkLaws("Codec[ULong]", CodecTests[ULong].codec)

  checkLaws("Codec[Safelong]", CodecTests[SafeLong].codec)
  checkLaws("Codec[Natural]", CodecTests[Natural].codec)
  checkLaws("Codec[Rational]", CodecTests[Rational].codec)
  checkLaws("Codec[Algebraic]", CodecTests[Algebraic].unserializableCodec)

  //See https://github.com/non/spire/issues/756
  //checkLaws("Codec[Polynomial[SafeLong]]", CodecTests[Polynomial[Int]].codec)

}
