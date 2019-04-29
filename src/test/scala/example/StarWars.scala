package dev.habla
package example

case class Query(heroes: List[Character])

case class Character(
  name: String,
  friends: List[Character],
  homeWorld: Planet,
  species: Species)

case class Planet(name: String, climate: String)

case class Species(name: String, lifespan: Int, origin: Planet)
