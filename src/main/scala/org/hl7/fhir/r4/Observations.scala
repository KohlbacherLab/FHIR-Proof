package org.hl7.fhir.r4


import java.time.Instant
import java.time.temporal.Temporal

import shapeless.{:+:,CNil}



abstract class SimpleObservation[+V: Observation.ValidValue]
extends ObservationSC
{

  this: Observation.value[V]
        with Observation.subject[_ <: Resource,Required] =>

}

