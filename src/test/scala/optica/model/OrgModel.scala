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

  import symantics.interpreter._
  // import ctx._

  case class DepartmentRel(dpt: String)
  case class EmployeeRel(emp: String, dpt: String)
  case class TaskRel(tsk: String, emp: String)

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
  
  import quel._

  trait SymSchema[Repr[_]] {
    def dpt(d: Repr[DepartmentRel]): Repr[String]
    def emp(e: Repr[EmployeeRel]): Repr[String]
    def dpt_k(d: Repr[EmployeeRel]): Repr[String]
    def tsk(t: Repr[TaskRel]): Repr[String]
    def emp_k(t: Repr[TaskRel]): Repr[String]
    def table_department: Repr[List[DepartmentRel]]
    def table_employee: Repr[List[EmployeeRel]]
    def table_task: Repr[List[TaskRel]]
  }

  object SymSchema {
    
    implicit object RSymSchema extends SymSchema[λ[x => x]] {
      def dpt(d: DepartmentRel) = d.dpt
      def emp(e: EmployeeRel) = e.emp
      def dpt_k(d: EmployeeRel) = d.dpt
      def tsk(t: TaskRel) = t.tsk
      def emp_k(t: TaskRel) = t.emp
      def table_department = List(
        DepartmentRel("Product"),
        DepartmentRel("Quality"),
        DepartmentRel("Research"),
        DepartmentRel("Sales"))
      def table_employee = List(
        EmployeeRel("Alex", "Product"),
        EmployeeRel("Bert", "Product"),
        EmployeeRel("Cora", "Research"),
        EmployeeRel("Drew", "Research"),
        EmployeeRel("Edna", "Research"),
        EmployeeRel("Fred", "Sales"))
      def table_task = List(
        TaskRel("build", "Alex"),
        TaskRel("build", "Bert"),
        TaskRel("abstract", "Cora"),
        TaskRel("build", "Cora"),
        TaskRel("design", "Cora"),
        TaskRel("abstract", "Drew"),
        TaskRel("design", "Drew"),
        TaskRel("abstract", "Edna"),
        TaskRel("call", "Edna"),
        TaskRel("design", "Edna"),
        TaskRel("call", "Fred"))
    }
  }

  trait NestSchema[Repr[_]] {
    def Department(
      dpt: Repr[String], 
      employees: Repr[List[Employee]]): Repr[Department]
    def Employee(
      emp: Repr[String],
      tasks: Repr[List[Task]]): Repr[Employee]
    def Task(tsk: Repr[String]): Repr[Task]
    def dpt(d: Repr[Department]): Repr[String]
    def employees(d: Repr[Department]): Repr[List[Employee]]
    def emp(e: Repr[Employee]): Repr[String]
    def tasks(e: Repr[Employee]): Repr[List[Task]]
    def tsk(t: Repr[Task]): Repr[String]
  }

  object NestSchema {
    
    implicit object RNestSchema extends NestSchema[λ[x => x]] {
      def Department(dpt: String, employees: List[Employee]) =
        example.Department(dpt, employees)
      def Employee(emp: String, tasks: List[Task]) = 
        example.Employee(emp, tasks)
      def Task(tsk: String) = example.Task(tsk)
      def dpt(d: Department) = d.dpt
      def employees(d: Department) = d.employees
      def emp(e: Employee) = e.emp
      def tasks(e: Employee) = e.tasks
      def tsk(t: Task) = t.tsk
    }
  }

  def model[Repr[_]](implicit
      Q: Quel[Repr],
      S: SymSchema[Repr],
      N: NestSchema[Repr]): Repr[List[Department]] = {
    import Q._, S._
    foreach(table_department)(d =>
    yields(N.Department(dpt(d), foreach(table_employee)(e =>
      where(equal(dpt(d), dpt_k(e)))(
      yields(N.Employee(emp(e), foreach(table_task)(t =>     
        where(equal(emp(e), emp_k(t)))(
        yields(N.Task(tsk(t))))))))))))
  }

  implicit def quelOrgModel[Repr[_]](implicit
      Q: Quel[Repr],
      N: NestSchema[Repr]) = new OrgModel[Down[Repr, ?]] {
    import Q._
    def departments = DownFold(lam(identity))
    def dpt = DownGetter(lam(d => N.dpt(d)))
    def employees = DownFold(lam(d => N.employees(d)))
    def tasks = DownFold(lam(e => N.tasks(e)))
    def tsk = DownGetter(lam(t => N.tsk(t)))
  }
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

