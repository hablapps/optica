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

  import quel._

  trait SymSchema[Repr[_]] {
    def her(c: Repr[CoupleRel]): Repr[String]
    def him(c: Repr[CoupleRel]): Repr[String]
    def name(p: Repr[PersonRel]): Repr[String]
    def age(p: Repr[PersonRel]): Repr[Int]
    def table_couple: Repr[List[CoupleRel]]
    def table_person: Repr[List[PersonRel]]
  }

  object SymSchema {
    
    implicit object RSymSchema extends SymSchema[λ[x => x]] {
      def her(c: CoupleRel) = c.her
      def him(c: CoupleRel) = c.him
      def name(p: PersonRel) = p.name
      def age(p: PersonRel) = p.age
      def table_person = List(
        PersonRel("Alex", 60), 
        PersonRel("Bert", 55), 
        PersonRel("Cora", 33),
        PersonRel("Drew", 31),
        PersonRel("Edna", 21),
        PersonRel("Fred", 60))
      def table_couple = List(
        CoupleRel("Alex", "Bert"),
        CoupleRel("Cora", "Drew"),
        CoupleRel("Edna", "Fred"))
    }
  }

  def differences[Repr[_]](implicit 
      Q: Quel[Repr],
      S: SymSchema[Repr]): Repr[List[(String, Int)]] = {
    import Q._, S._
    foreach(table_couple)(c =>
    foreach(table_person)(m =>
    foreach(table_person)(w =>
    where(and(
      and(equal(her(c), name(w)), equal(him(c), name(m))), 
      greaterThan(age(w), age(m))))(
    yields(product(
      name(w), 
      subtract(age(w), age(m))))))))
  }

  trait NestSchema[Repr[_]] {
    def Couple(her: Repr[Person], him: Repr[Person]): Repr[Couple]
    def Person(name: Repr[String], age: Repr[Int]): Repr[Person]
    def her(c: Repr[Couple]): Repr[Person]
    def him(c: Repr[Couple]): Repr[Person]
    def name(p: Repr[Person]): Repr[String]
    def age(p: Repr[Person]): Repr[Int]
  }

  object NestSchema {
    
    implicit object RNestSchema extends NestSchema[λ[x => x]] {
      def Person(name: String, age: Int) = example.Person(name, age)
      def Couple(her: Person, him: Person) = example.Couple(her, him)
      def her(c: Couple) = c.her
      def him(c: Couple) = c.him
      def name(p: Person) = p.name
      def age(p: Person) = p.age
    }
  }

  def model[Repr[_]](implicit 
      Q: Quel[Repr],
      S: SymSchema[Repr],
      N: NestSchema[Repr]): Repr[List[Couple]] = {
    import Q._, S._
    foreach(table_couple)(c =>
    foreach(table_person)(m =>
    foreach(table_person)(w =>
    where(and(equal(her(c), name(w)), equal(him(c), name(m))))(
    yields(N.Couple(N.Person(name(w), age(w)), N.Person(name(m), age(m))))))))
  }

  implicit def quelCoupleModel[Repr[_]](implicit 
      Q: Quel[Repr],
      N: NestSchema[Repr]) = new CoupleModel[Down[Repr, ?]] {
    import Q._
    val couples = DownFold(lam(identity))
    val her = DownGetter(lam(c => N.her(c)))
    val him = DownGetter(lam(c => N.him(c)))
    val name = DownGetter(lam(p => N.name(p)))
    val age = DownGetter(lam(p => N.age(p)))
  }
}

class CoupleLogic[Repr[_], Obs[_]](implicit 
    O: Optica[Repr, Obs], 
    M: CoupleModel[Repr]) {
  import Optica.syntax._
  import O._, M._

  def differencesFl: Repr[Fold[Couples, (String, Int)]] =
    couples >>> filtered((her >>> age) > (him >>> age)) >>>
                (her >>> name) *** ((her >>> age) - (him >>> age))

  def differences: Obs[Couples => List[(String, Int)]] = differencesFl.getAll
}

