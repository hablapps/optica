package dev.habla.optica
package test

import concrete._
import symantics._

case class Couple(her: Person, him: Person)
case class Person(name: String, age: Int)

trait CoupleModel[Repr[_]] {
  def couples: Repr[Fold[Couples, Couple]]
  def her: Repr[Getter[Couple, Person]]
  def him: Repr[Getter[Couple, Person]]
  def name: Repr[Getter[Person, String]]
  def age: Repr[Getter[Person, Int]]
}

object CoupleModel {

  implicit object RCoupleModel extends CoupleModel[λ[x => x]] {
    val couples = Fold(identity)
    val her = Getter(_.her)
    val him = Getter(_.him)
    val name = Getter(_.name)
    val age = Getter(_.age)
  }
}

class CoupleLogic[Repr[_]](implicit O: Optica[Repr]) {
  import Optica.syntax._
  import O._

  def differences(implicit
      M: CoupleModel[Repr]): Repr[Fold[Couples, (String, Int)]] = {
    import M._
    couples >>> filtered((her >>> age) > (him >>> age)) >>>
                (her >>> name) *** ((her >>> age) - (him >>> age))
  }
}

