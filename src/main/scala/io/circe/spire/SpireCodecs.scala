package io.circe.spire

import io.circe.Decoder.Result
import io.circe._
import _root_.spire.math._
import _root_.spire.math.poly.{PolySparse, Term}
import _root_.spire.algebra.{Eq, Semiring}
import _root_.spire.ClassTag

trait SpireCodecs {
  implicit val safeLongEncoder: Encoder[SafeLong] = new Encoder[SafeLong] {
    def apply(a: SafeLong): Json = Json.fromBigInt(a.toBigInt)
  }

  implicit val naturalEncoder: Encoder[Natural] = new Encoder[Natural] {
    def apply(a: Natural): Json = Json.fromBigInt(a.toBigInt)
  }

  implicit val rationalEncoder: Encoder[Rational] =
    Encoder[(SafeLong, SafeLong)].contramap(r => (r.numerator, r.denominator))

  implicit def termEncoder[A: Encoder]: Encoder[Term[A]] =
    Encoder[(A, Int)].contramap(t => (t.coeff, t.exp))

  implicit def polynomialEncoder[A: Semiring: Eq: Encoder]: Encoder[Polynomial[A]] =
    Encoder[List[Term[A]]].contramap(_.terms)

  implicit val uByteEncoder: Encoder[UByte] =
    Encoder[Byte].contramap(_.signed)

  implicit val uShortEncoder: Encoder[UShort] =
    Encoder[Char].contramap(_.signed)

  implicit val uIntEncoder: Encoder[UInt] =
    Encoder[Int].contramap(_.signed)

  implicit val uLongEncoder: Encoder[ULong] =
    Encoder[Long].contramap(_.signed)

  implicit val safeLongDecoder: Decoder[SafeLong] =
    Decoder[BigInt].map(SafeLong.apply)

  implicit val naturalDecoder: Decoder[Natural] = new Decoder[Natural] {
    def apply(c: HCursor): Result[Natural] =
      Decoder[BigInt].apply(c)
        .right
        .flatMap(big => if (big < BigInt(0)) Left(DecodingFailure("Must be positive", Nil)) else Right(Natural(big)))
  }

  implicit val rationalDecoder: Decoder[Rational] =
    Decoder[(SafeLong, SafeLong)].map { case (numerator, denominator) => Rational(numerator, denominator) }

  implicit def termDecoder[A: Decoder]: Decoder[Term[A]] =
    Decoder[(A, Int)].map { case (a, i) => Term(a, i) }

  implicit def polynomialDecoder[A: Semiring: Eq: ClassTag: Decoder]: Decoder[Polynomial[A]] =
    Decoder[List[Term[A]]].map(terms => PolySparse(terms))

  implicit val uByteDecoder: Decoder[UByte] = Decoder[Byte].map(b => new UByte(b))

  implicit val uShortDecoder: Decoder[UShort] = Decoder[Char].map(c => new UShort(c))

  implicit val uIntDecoder: Decoder[UInt] = Decoder[Int].map(i => new UInt(i))

  implicit val uLongDecoder: Decoder[ULong] = Decoder[Long].map(l => new ULong(l))

}
