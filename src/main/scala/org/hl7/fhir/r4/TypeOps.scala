package org.hl7.fhir.r4



import shapeless.{Coproduct, :+:, Inl, Inr, CNil}
import shapeless.{::, HList, HNil, DepFn1}



@annotation.implicitNotFound("Not all types in ${L} match ${F}")
sealed trait ForAll[L <: HList, F[_]]
object ForAll
{

  def apply[L <: HList, F[_]](implicit forAll: ForAll[L,F]) = forAll

  implicit def matched[H, T <: HList, F[_]](
    implicit
    hm: F[H],
    tm: ForAll[T,F]
  ): ForAll[H :: T,F] = 
    new ForAll[H :: T,F]{}

  implicit def terminal[F[_]]: ForAll[HNil,F] =
    new ForAll[HNil,F]{}

}


@annotation.implicitNotFound("${T} is not in ${C}")
trait IsIn[T, C <: Coproduct] extends DepFn1[C]{ type Out = Boolean }
object IsIn
{

  type ∈ [T, C <: Coproduct] = IsIn[T,C]

  def apply[T, C <: Coproduct](implicit isIn: IsIn[T,C]): IsIn[T,C] = isIn

  implicit def isInH[H, S <: H, T <: Coproduct]: IsIn[S,H :+: T] = 
    new IsIn[S,H :+: T]{
      def apply(c: H :+: T) =
        c match {
          case Inl(h) => true
          case Inr(t) => false
        }
    }

  implicit def isInT[H, T <: Coproduct, U](
    implicit isIn: IsIn[U,T]
  ): IsIn[U,H :+: T] = 
    new IsIn[U,H :+: T]{
      def apply(c: H :+: T) =
        c match {
          case Inl(h) => false
          case Inr(t) => isIn(t)
        }
    }

}


/*
trait Contains[S <: HList,T] extends DepFn1[S]{ type Out = Boolean }

object Contains
{

  def apply[L <: HList, S](implicit contains: Contains[L,S]): Contains[L,S] = contains

  implicit def select[H, T <: HList, S <: H]: Contains[H :: T, S] =
    new Contains[H :: T, S]{
      def apply(l: H :: T) = true 
    }

  implicit def recurse[H, T <: HList, U](
    implicit contains: Contains[T,U]
  ): Contains[H :: T, U] =
    new Contains[H :: T, U]{
      def apply(l: H :: T) = contains(l.tail)
    }

}
*/

