package org.hl7.fhir.r4


import java.time.Instant
import java.time.temporal.Temporal

import shapeless.{Coproduct, :+:, CNil}


trait CanHaveEffective[E <: Coproduct]
{

  type ValidEffective[X] = X IsIn E


  sealed trait effective[X]


  trait effectiveDateTime[D <: Temporal,C[_]] extends effective[D]{
    this: Event =>
    val effectiveDateTime: C[D]
  }

  trait effectivePeriod[P <: Period[_],C[_]] extends effective[P]{
    this: Event =>
    val effectivePeriod: C[P]
  }

  trait effectiveInstant[C[_]] extends effective[Instant]{
    this: Event =>
    val effectiveInstant: C[Instant]
  }


}
