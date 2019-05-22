package example.couple
package interpreter

import optica.quel._
import optica.symantics.interpreter._

case class CoupleRel(her: String, him: String)
case class PersonRel(name: String, age: Int)

class QuelModel[Repr[_]](implicit Q: Quel[Repr], N: Nested[Repr]) 
    extends Model[Down[Repr, ?]] {
  import Q._

  val couples = DownFold(lam(identity))

  val her = DownGetter(lam(c => N.her(c)))

  val him = DownGetter(lam(c => N.him(c)))

  val name = DownGetter(lam(p => N.name(p)))

  val age = DownGetter(lam(p => N.age(p)))
}
      
trait Schema[Repr[_]] {

  def table_couple: Repr[List[CoupleRel]]

  def her(c: Repr[CoupleRel]): Repr[String]
  
  def him(c: Repr[CoupleRel]): Repr[String]

  def table_person: Repr[List[PersonRel]]

  def name(p: Repr[PersonRel]): Repr[String]
  
  def age(p: Repr[PersonRel]): Repr[Int]
}

object Schema {
  
  implicit object RSchema extends Schema[λ[x => x]] {

    def table_person = List(
      PersonRel("Alex", 60), 
      PersonRel("Bert", 55), 
      PersonRel("Cora", 33),
      PersonRel("Drew", 31),
      PersonRel("Edna", 21),
      PersonRel("Fred", 60))

    def her(c: CoupleRel) = c.her

    def him(c: CoupleRel) = c.him

    def table_couple = List(
      CoupleRel("Alex", "Bert"),
      CoupleRel("Cora", "Drew"),
      CoupleRel("Edna", "Fred"))

    def name(p: PersonRel) = p.name

    def age(p: PersonRel) = p.age
  }

  implicit object ToStringSchema extends Schema[λ[x => Int => String]] {

    def table_couple = _ => "table_couple"
    
    def her(c: Int => String) = i => s"${c(i)}.her"

    def him(c: Int => String) = i => s"${c(i)}.him"

    def table_person = _ => "table_person"
    
    def name(p: Int => String) = i => s"${p(i)}.name"

    def age(p: Int => String) = i => s"${p(i)}.age"
  }
}

trait Nested[Repr[_]] {

  def Couple(her: Repr[Person], him: Repr[Person]): Repr[Couple]

  def her(c: Repr[Couple]): Repr[Person]

  def him(c: Repr[Couple]): Repr[Person]

  def Person(name: Repr[String], age: Repr[Int]): Repr[Person]

  def name(p: Repr[Person]): Repr[String]

  def age(p: Repr[Person]): Repr[Int]
}

object Nested {

  def apply[Repr[_]](implicit 
      Q: Quel[Repr],
      S: Schema[Repr],
      N: Nested[Repr]): Repr[Couples] = {
    import Q._, S._
    foreach(table_couple)(c =>
    foreach(table_person)(m =>
    foreach(table_person)(w =>
    where(and(equal(her(c), name(w)), equal(him(c), name(m))))(
    yields(N.Couple(N.Person(name(w), age(w)), N.Person(name(m), age(m))))))))
  }
  
  implicit object RNested extends Nested[λ[x => x]] {

    def Couple(her: Person, him: Person) = example.couple.Couple(her, him)

    def her(c: Couple) = c.her

    def him(c: Couple) = c.him

    def Person(name: String, age: Int) = example.couple.Person(name, age)

    def name(p: Person) = p.name

    def age(p: Person) = p.age
  }

  implicit object ToStringNested extends Nested[λ[x => Int => String]] {

    def Couple(her: Int => String, him: Int => String) = { i =>
      s"Couple(${her(i)}, ${him(i)})"
    }

    def her(c: Int => String) = i => s"${c(i)}.her"

    def him(c: Int => String) = i => s"${c(i)}.him"

    def Person(name: Int => String, age: Int => String) = { i =>
      s"Person(${name(i)}, ${age(i)})"
    }

    def name(p: Int => String) = i => s"${p(i)}.name"
    
    def age(p: Int => String) = i => s"${p(i)}.age"
  }
}

