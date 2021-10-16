package io.sitiritis.data_types_a_la_carte

import cats.effect._
import cats.Functor
import cats.syntax.functor._
import cats.:<:
import cats.InjectK._
import cats.data.EitherK
import cats.data.EitherK._

object Main extends IOApp.Simple {
  object adt {
    case class Fix[F[_]](un: F[Fix[F]])

    case class ValF[T](int: Int)

    type Val = Fix[ValF]

    case class AddF[T](left: T, right: T)

    type Add = Fix[AddF]

    type Expr = Fix[EitherK[ValF, AddF, *]]
  }

  object typeclasses {
    trait Algebra[F[_], T] {
      def eval(value: F[T]): T
    }
  }

  object instances {

    import typeclasses._
    import adt._

    implicit val functorValF: Functor[ValF] = new Functor[ValF] {
      override def map[A, B](fa: ValF[A])(f: A => B): ValF[B] = ValF(fa.int)
    }

    implicit val functorAddF: Functor[AddF] = new Functor[AddF] {
      override def map[A, B](fa: AddF[A])(f: A => B): AddF[B] = AddF(
        f(fa.left),
        f(fa.right)
      )
    }

    implicit val algebraValInt: Algebra[ValF, Int] = new Algebra[ValF, Int] {
      override def eval(value: ValF[Int]): Int = value.int
    }

    implicit val algebraAddInt: Algebra[AddF, Int] = new Algebra[AddF, Int] {
      override def eval(value: AddF[Int]): Int = value.left + value.right
    }

    implicit def algebraCoProduct[F[_], G[_], T](
      implicit
      fAlgebra: Algebra[F, T],
      gAlgebra: Algebra[G, T],
    ): Algebra[EitherK[F, G, *], T] = new Algebra[EitherK[F, G, *], T] {
      override def eval(value: EitherK[F, G, T]): T = value.run match {
        case Left(l) => fAlgebra.eval(l)
        case Right(r) => gAlgebra.eval(r)
      }
    }
  }

  object functions {

    import typeclasses._
    import adt._

    def foldFix[F[_] : Functor, T](alg: F[T] => T)(fix: Fix[F]): T = {
      val unfixed: F[Fix[F]] = fix.un
      val foldedUnfixed: F[T] = unfixed.map(foldFix(alg))
      alg(foldedUnfixed)
    }

    def evalAlg[F[_] : Functor, T](fix: Fix[F])(implicit alg: Algebra[F, T]): T =
      foldFix(alg.eval)(fix)
  }

  object smartconstructors {

    import adt._

    def liftVal[F[_]](i: Int)(implicit ev: ValF :<: F): Fix[F] =
      Fix(ev.inj(ValF(i)))

    def liftAdd[F[_]](l: Fix[F], r: Fix[F])(implicit ev: AddF :<: F): Fix[F] =
      Fix(ev.inj(AddF(l, r)))

    object syntax {
      implicit class ExprOps[F[_]](private val l: Fix[F]) extends AnyVal {
        def :+:(r: Fix[F])(implicit ev: AddF :<: F): Fix[F] = liftAdd(l, r)
      }

      implicit class IntOps(private val i: Int) extends AnyVal {
        def v[F[_]](implicit ev: ValF :<: F): Fix[F] = liftVal(i)
      }

      def v[F[_]](i: Int)(implicit ev: ValF :<: F): Fix[F] = i.v
    }
  }

  object values {

    import adt._
    import smartconstructors.syntax._

    val addExampleManual: Expr = {
      val val118: ValF[Expr] = ValF(118)
      val val1219: ValF[Expr] = ValF(1219)

      val coprodVal118: EitherK[ValF, AddF, Expr] = left(val118)
      val fixVal118: Expr = Fix(coprodVal118)
      val coprodVal1219: EitherK[ValF, AddF, Expr] = left(val1219)
      val fixVal1219: Expr = Fix(coprodVal1219)

      val sum: AddF[Expr] = AddF(fixVal118, fixVal1219)
      val coprodSum: EitherK[ValF, AddF, Expr] = right(sum)
      val fixedSum: Expr = Fix(coprodSum)

      fixedSum
    }

    val addExample: Expr =
      (30000.v: Expr) :+: (1330.v: Expr) :+: (7.v: Expr)
  }

  import values._
  import functions.evalAlg
  import instances._

  override def run: IO[Unit] = {
    /*
    We are able to specify the algebra (evaluator) manually or we can omit this
    and just specify the resulting type of the expression
    */
    // implicit val algebraCoProductValIntAddInt: Algebra[CoProduct[ValF, AddF, *], Int] =
    //   algebraCoProduct(algebraValInt, algebraAddInt)

    val intEvaluationResultManual: Int = evalAlg(addExampleManual)
    val intEvaluationResult: Int = evalAlg(addExample)

    for {
      _ <- IO.delay(println(s"Integer evaluation result (manual):\n${intEvaluationResultManual}"))
      _ <- IO.delay(println(s"Integer evaluation result (smart constructors):\n${intEvaluationResult}"))
    } yield ()
  }
}
