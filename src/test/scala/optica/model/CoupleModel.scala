package dev.habla
package optica
package model

import example._
import concrete._
import xquery._
import triplet._
import symantics._

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

  implicit object XQueryCoupleModel extends CoupleModel[λ[x => XQuery]] {
    val couples = Name("couple")
    val her = Name("her")
    val him = Name("him")
    val name = Name("name")
    val age = Name("age")
  }

  import Optica.TripletFunOptica.{base, entity}

  implicit object TripletFunCoupleModel extends CoupleModel[λ[x => TripletFun]] {
    val couples = entity(FoldType("couples", "Couples", "Couple"), "c")
    val her = entity(GetterType("her", "Couple", "Person"), "w")
    val him = entity(GetterType("him", "Couple", "Person"), "m")
    val name = base(GetterType("name", "Person", "String"))
    val age = base(GetterType("age", "Person", "Int"))
  }

  import symantics.interpreter._
  import ctx._

  case class CoupleRel(her: String, him: String)
  case class PersonRel(name: String, age: Int)

  val q: Quoted[Query[Couple]] = quote {
    for {
      c <- query[CoupleRel]
      w <- query[PersonRel]
      m <- query[PersonRel]
      if c.her == w.name && c.him == m.name
    } yield Couple(Person(w.name, w.age), Person(m.name, m.age))
  }

  implicit object QuillCoupleModel extends CoupleModel[QRep] {
    val couples = QFold(quote { _: Query[Couples] => q })
    val her = QGetter(quote { c: Couple => c.her })
    val him = QGetter(quote { c: Couple => c.him })
    val name = QGetter(quote { p: Person => p.name })
    val age = QGetter(quote { p: Person => p.age })
  }
}

class CoupleLogic[Repr[_], Obs[_]](implicit 
    O: Optica[Repr, Obs], 
    M: CoupleModel[Repr]) {
  import Optica.syntax._
  import O._, M._

  def simplestFl: Repr[Fold[Couples, (String, Int)]] = 
    couples >>> 
      filtered((her >>> age) > (him >>> age)) >>> 
      (her >>> name) *** ((her >>> age) - (him >>> age))

  def simplest: Obs[Couples => List[(String, Int)]] = simplestFl.getAll

  def differencesFl: Repr[Fold[Couples, (String, Int)]] =
    couples >>> filtered((her >>> age) > (him >>> age)) >>>
                (her >>> name) *** ((her >>> age) - (him >>> age))

  def differences: Obs[Couples => List[(String, Int)]] = differencesFl.getAll
}

