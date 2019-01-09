package io.circe.spire

import io.circe.Decoder.Result
import io.circe._
import _root_.spire.math._
import _root_.spire.implicits._
import _root_.spire.math.poly.{PolySparse, Term}
import _root_.spire.math.Algebraic.Expr._
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

  val algebraicExprEncoder: Encoder[Algebraic.Expr] = new Encoder[Algebraic.Expr] {
    def apply(a: Algebraic.Expr): Json = a match {
      case ConstantLong(n) => Json.obj("Long" -> Json.fromLong(n))
      case ConstantDouble(n) => Json.obj("Double" -> Json.fromDoubleOrNull(n))
      case ConstantBigDecimal(n) => Json.obj("BigDecimal" -> Json.fromBigDecimal(n))
      case ConstantRational(n) => Json.obj("Rational" -> rationalEncoder(n))
      case ConstantRoot(poly, i, lb, ub) => Json.obj("Root" -> Json.obj(
        "poly" -> polynomialEncoder[BigInt].apply(poly),
        "i" -> Json.fromInt(i),
        "lb" -> rationalEncoder(lb),
        "ub" -> rationalEncoder(ub)
      ))
      case Neg(n) => Json.obj("Neg" -> apply(n))
      case Add(a, b) => Json.obj("Add" -> Json.obj("a" -> apply(a), "b" -> apply(b)))
      case Sub(a, b) => Json.obj("Sub" -> Json.obj("a" -> apply(a), "b" -> apply(b)))
      case Mul(a, b) => Json.obj("Mul" -> Json.obj("a" -> apply(a), "b" -> apply(b)))
      case Div(a, b) => Json.obj("Dib" -> Json.obj("a" -> apply(a), "b" -> apply(b)))
      case KRoot(a, k) => Json.obj("KRoot" -> Json.obj("a" -> apply(a), "k" -> Json.fromInt(k)))
      case Pow(a, k) => Json.obj("Pow" -> Json.obj("a" -> apply(a), "k" -> Json.fromInt(k)))
    }
  }

  implicit val algebraicEncoder: Encoder[Algebraic] =
    algebraicExprEncoder.contramap(_.expr)


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

  implicit val algebraicExprDecoder: Decoder[Algebraic] =  {

    val constantRootDecoder = Decoder.instance(c => for {
      poly <- c.downField("poly").as[Polynomial[BigInt]]
      i <- c.downField("i").as[Int]
      lb <- c.downField("lb").as[Rational]
      ub <- c.downField("ub").as[Rational]
    } yield (poly, i, lb, ub))

    def twoExprDecoder(constructor: (Algebraic, Algebraic) => Algebraic) =
      Decoder.instance(c => for {
        a <- c.downField("a").as[Algebraic]
        b <- c.downField("b").as[Algebraic]
      } yield constructor(a, b))

    def exprIntDecoder(constructor: (Algebraic, Int) => Algebraic) =
      Decoder.instance(c => for {
        a <- c.downField("a").as[Algebraic]
        k <- c.downField("k").as[Int]
      } yield constructor(a, k))

    Decoder.instance(_.downField("Long").as[Long].map(Algebraic.apply)).or(
      Decoder.instance(_.downField("Double").as[Double].map(Algebraic.apply))).or(
      Decoder.instance(_.downField("BigDecimal").as[BigDecimal].map(Algebraic.apply))).or(
      Decoder.instance(_.downField("Rational").as[Rational].map(Algebraic.apply))).or(
      Decoder.instance(_.downField("Root").as(constantRootDecoder).map((Algebraic.unsafeRoot _).tupled))).or(
      Decoder.instance(_.downField("Neg").as[Algebraic])).or(
      Decoder.instance(_.downField("Add").as(twoExprDecoder(_ + _)))).or(
      Decoder.instance(_.downField("Sub").as(twoExprDecoder(_ - _)))).or(
      Decoder.instance(_.downField("Mul").as(twoExprDecoder(_ * _)))).or(
      Decoder.instance(_.downField("Div").as(twoExprDecoder(_ / _)))).or(
      Decoder.instance(_.downField("KRoot").as(exprIntDecoder(_ nroot _)))).or(
      Decoder.instance(_.downField("Pow").as(exprIntDecoder(_ pow _)))
    )
  }

  implicit def termDecoder[A: Decoder]: Decoder[Term[A]] =
    Decoder[(A, Int)].map { case (a, i) => Term(a, i) }

  implicit def polynomialDecoder[A: Semiring: Eq: ClassTag: Decoder]: Decoder[Polynomial[A]] =
    Decoder[List[Term[A]]].map(terms => PolySparse(terms))

  implicit val uByteDecoder: Decoder[UByte] = Decoder[Byte].map(b => new UByte(b))

  implicit val uShortDecoder: Decoder[UShort] = Decoder[Char].map(c => new UShort(c))

  implicit val uIntDecoder: Decoder[UInt] = Decoder[Int].map(i => new UInt(i))

  implicit val uLongDecoder: Decoder[ULong] = Decoder[Long].map(l => new ULong(l))

}
