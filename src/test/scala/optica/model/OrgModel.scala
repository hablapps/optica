package dev.habla
package optica
package model

import example._
import concrete._
import xquery._
import triplet._
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

  import Optica.TripletFunOptica.{base, entity}

  implicit object TripletFunOrgModel extends OrgModel[λ[x => TripletFun]] {
    val departments = entity(FoldType("departments", "Org", "Department"), "d")
    val dpt = base(GetterType("dpt", "Department", "String"))
    val employees = entity(FoldType("employees", "Department", "Employee"), "e")
    val tasks = entity(FoldType("tasks", "Employee", "Task"), "t")
    val tsk = base(GetterType("tsk", "Task", "String"))
  }

  // import symantics.interpreter._
  // import ctx._

  // case class DepartmentRel(dpt: String)
  // case class EmployeeRel(emp: String, dpt: String)
  // case class TaskRel(tsk: String, emp: String)

  // val q: Quoted[Query[Department]] = quote {
  //   for {
  //     d <- query[DepartmentRel]
  //   } yield Department(d.dpt, 
  //       for {
  //         e <- query[EmployeeRel]
  //         if e.dpt == d.dpt
  //       } yield Employee(e.emp, 
  //           for {
  //             t <- query[TaskRel]
  //             if t.emp == e.emp
  //           } yield Task(t.tsk)))
  // }
}

class OrgLogic[Repr[_], Obs[_]](implicit 
    O: Optica[Repr, Obs], 
    M: OrgModel[Repr]) {
  import Optica.syntax._
  import O._, M._

  def expertiseFl(u: String): Repr[Fold[Org, String]] =
    departments >>> filtered(employees.all((tasks >>> tsk).elem(u))) >>> dpt

  def expertise(u: String): Obs[Org => List[String]] = expertiseFl(u).getAll
}

