package dev.habla
package optica
package model

import example._
import concrete._
import symantics._
import triplet._
import scalaz._, Scalaz._

trait StarWarsModel[Repr[_]] {
  def heroes: Repr[Fold[Query, Character]]

  def characterName: Repr[Getter[Character, String]]
  def characterFriends: Repr[Fold[Character, Character]]
  def characterHomeWorld: Repr[Getter[Character, Planet]]
  def characterSpecies: Repr[Getter[Character, Species]]

  def planetName: Repr[Getter[Planet, String]]
  def planetClimate: Repr[Getter[Planet, String]]

  def speciesName: Repr[Getter[Species, String]]
  def speciesLifespan: Repr[Getter[Species, Int]]
  def speciesOrigin: Repr[Getter[Species, Planet]]
}

object StarWarsModel {

  // implicit object ROrgModel extends OrgModel[λ[x => x]] {
  //   val departments = Fold(identity)
  //   val dpt = Getter(_.dpt)
  //   val employees = Fold(_.employees)
  //   val tasks = Fold(_.tasks)
  //   val tsk = Getter(_.tsk)
  // }

  import Optica.TripletFunOptica.{base, entity}

  implicit object TripletFunStarWarsModel extends StarWarsModel[λ[x => TripletFun]] {
    val heroes = entity(FoldType("heroes", "Query", "Character"), "c")

    val characterName = base(GetterType("characterName", "Character", "String"))
    val characterFriends = base(GetterType("characterName", "Character", "String"))
    val characterHomeWorld = entity(GetterType("characterHomeWorld", "Character", "Planet"), "p")
    val characterSpecies = entity(GetterType("characterSpecies", "Character", "Species"), "s")

    val planetName = base(GetterType("planetName", "Planet", "String"))
    val planetClimate = base(GetterType("planetClimate", "Planet", "String"))

    val speciesName = base(GetterType("speciesName", "Species", "String"))
    val speciesLifespan = base(GetterType("speciesLifespan", "Species", "Int"))
    val speciesOrigin = entity(GetterType("speciesOrigin", "Species", "Planet"), "p")
  }
}

class StarWarsLogic[Repr[_]](implicit O: Optica[Repr], SWM: StarWarsModel[Repr]) {

  import Optica.syntax._
  import O._, SWM._

  // Query #1
  // Give me all heroes (name) that have any friend whose homeWorld is Tatuin
  def query1(p: String): Repr[Fold[Query, String]] =
    heroes >>> filtered((characterFriends >>> characterHomeWorld >>> planetName).elem(p)) >>> characterName

}

object StarWarsSQL extends App {

  type SQL[X] = TripletFun

  val swl = new StarWarsLogic[SQL]()

  val keys = ==>>("Character" -> "characterName", "Planet" -> "planetName", "Species" -> "speciesName")

  println(s"QUERY: \n${swl.query1("Tatuin").toSQL(keys)}")

}
