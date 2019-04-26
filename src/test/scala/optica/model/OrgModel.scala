package dev.habla
package optica
package model

import example._
import concrete._
import xquery._
import symantics._

trait OrgModel[Repr[_]] {
  def departments: Repr[Fold[Org, Department]]
  def dpt: Repr[Getter[Department, String]]
  def employees: Repr[Fold[Department, Employee]]
  def tasks: Repr[Fold[Employee, Task]]
  def tsk: Repr[Getter[Task, String]]
}

object OrgModel {

  implicit object ROrgModel extends OrgModel[λ[x => x]] {
    val departments = Fold(identity)
    val dpt = Getter(_.dpt)
    val employees = Fold(_.employees)
    val tasks = Fold(_.tasks)
    val tsk = Getter(_.tsk)
  }

  implicit object XQueryOrgModel extends OrgModel[λ[x => XQuery]] {
    val departments = Name("department")
    val dpt = Name("dpt")
    val employees = Name("employee")
    val tasks = Name("task")
    val tsk = Name("tsk")
  }
}

class OrgLogic[Repr[_]](implicit O: Optica[Repr]) {
  import Optica.syntax._
  import O._

  def expertise(
      u: String)(implicit 
      M: OrgModel[Repr]): Repr[Fold[Org, String]] = {
    import M._
    departments >>> filtered(employees.all((tasks >>> tsk).elem(u))) >>> dpt
  }
}

