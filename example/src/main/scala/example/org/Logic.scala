package example.org

import optica._
import concrete._
import symantics._

class Logic[Repr[_], Obs[_]](implicit 
    O: Optica[Repr, Obs], 
    M: Model[Repr]) {
  import Optica.syntax._
  import O._, M._

  def expertiseFl(u: String): Repr[Fold[Org, String]] =
    departments >>> filtered(employees.all((tasks >>> tsk).elem(u))) >>> dpt

  def expertise(u: String): Obs[Org => List[String]] = expertiseFl(u).getAll
}

