package dev.habla.optica
package triplet

sealed abstract class OpticType {
  val name: String 
  val from: String 
  val to: String
}

case class GetterType(name: String, from: String, to: String) extends OpticType

case class AffineFoldType(name: String, from: String, to: String) extends OpticType

case class FoldType(name: String, from: String, to: String) extends OpticType

