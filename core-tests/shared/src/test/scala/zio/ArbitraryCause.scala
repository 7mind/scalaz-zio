package zio

import org.scalacheck.{ Arbitrary, Gen }
import zio.Cause.Traced

object ArbitraryCause {
  implicit val arbCauseDie: Arbitrary[Cause.Die] =
    Arbitrary(Arbitrary.arbitrary[String].map(s => Cause.Die(new RuntimeException(s))))

  implicit def arbCause[T](implicit arbT: Arbitrary[T]): Arbitrary[Cause[T]] =
    Arbitrary {
      Gen.oneOf(
        Gen.const(Cause.interrupt),
        arbCauseDie.arbitrary,
        arbT.arbitrary.map(Cause.fail),
        Gen.lzy {
          arbCause[T].arbitrary.map(Traced(_, ZTrace(0, Nil, Nil, None)))
        },
        Gen.lzy {
          for {
            left  <- arbCause[T].arbitrary
            right <- arbCauseDie.arbitrary
          } yield Cause.Then(left, right)
        },
        Gen.lzy {
          for {
            left  <- arbCause[T].arbitrary
            right <- arbCause[T].arbitrary
          } yield Cause.Both(left, right)
        }
      )
    }
}
