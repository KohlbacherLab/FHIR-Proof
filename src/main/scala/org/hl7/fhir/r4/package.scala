package org.hl7.fhir


package object r4
{

  type Ref[+R <: Resource] = Reference[R]

  type Profile[+R <: Resource] = R


  type Must[+T]     = T
  type Can[+T]      = Option[T]
  type Required[+T] = T
  type Optional[+T] = Option[T]


  sealed trait Occurrence
  sealed trait MinOccurrence 
  sealed trait MaxOccurrence 
  sealed trait One       extends Occurrence with MaxOccurrence
  sealed trait Many      extends Occurrence with MaxOccurrence


}
