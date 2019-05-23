package example.couple

import optica._
import concrete._
import symantics._

class Logic[Repr[_], Obs[_]](implicit O: Optica[Repr, Obs], M: Model[Repr]) {
  import Optica.syntax._
  import O._, M._

  def differencesFl: Repr[Fold[Couples, (String, Int)]] =
    couples >>> filtered((her >>> age) > (him >>> age)) >>>
                (her >>> name) *** ((her >>> age) - (him >>> age))

  def differences: Obs[Couples => List[(String, Int)]] = differencesFl.getAll
}

