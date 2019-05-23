package example.org
package interpreter

import optica.quel._
import optica.symantics.interpreter._

case class DepartmentRel(dpt: String)
case class EmployeeRel(emp: String, dpt: String)
case class TaskRel(tsk: String, emp: String)

class QuelModel[Repr[_]](implicit Q: Quel[Repr], N: Nested[Repr]) 
    extends Model[Down[Repr, ?]] {
  import Q._
  def departments = DownFold(lam(identity))
  def dpt = DownGetter(lam(d => N.dpt(d)))
  def employees = DownFold(lam(d => N.employees(d)))
  def tasks = DownFold(lam(e => N.tasks(e)))
  def tsk = DownGetter(lam(t => N.tsk(t)))
}

trait Schema[Repr[_]] {
  def dpt(d: Repr[DepartmentRel]): Repr[String]
  def emp(e: Repr[EmployeeRel]): Repr[String]
  def dpt_k(d: Repr[EmployeeRel]): Repr[String]
  def tsk(t: Repr[TaskRel]): Repr[String]
  def emp_k(t: Repr[TaskRel]): Repr[String]
  def table_department: Repr[List[DepartmentRel]]
  def table_employee: Repr[List[EmployeeRel]]
  def table_task: Repr[List[TaskRel]]
}

object Schema {

  implicit object RSchema extends Schema[λ[x => x]] {
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

  implicit object ToStringSchema extends Schema[λ[x => Int => String]] {
    def dpt(d: Int => String) = i => s"${d(i)}.dpt"
    def emp(e: Int => String) = i => s"${e(i)}.emp"
    def dpt_k(d: Int => String) = i => s"${d(i)}.dpt"
    def tsk(t: Int => String) = i => s"${t(i)}.tsk"
    def emp_k(t: Int => String) = i => s"${t(i)}.emp"
    def table_department = _ => "table_department"
    def table_employee = _ => "table_employee"
    def table_task = _ => "table_task"
  }
}

trait Nested[Repr[_]] {
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

object Nested {

  def apply[Repr[_]](implicit
      Q: Quel[Repr],
      S: Schema[Repr],
      N: Nested[Repr]): Repr[List[Department]] = {
    import Q._, S._
    foreach(table_department)(d =>
        yields(N.Department(dpt(d), foreach(table_employee)(e =>
            where(equal(dpt(d), dpt_k(e)))(
              yields(N.Employee(emp(e), foreach(table_task)(t =>     
                  where(equal(emp(e), emp_k(t)))(
                    yields(N.Task(tsk(t))))))))))))
  }

  implicit object RNested extends Nested[λ[x => x]] {
    def Department(dpt: String, employees: List[Employee]) =
      example.org.Department(dpt, employees)
    def Employee(emp: String, tasks: List[Task]) = 
      example.org.Employee(emp, tasks)
    def Task(tsk: String) = example.org.Task(tsk)
    def dpt(d: Department) = d.dpt
    def employees(d: Department) = d.employees
    def emp(e: Employee) = e.emp
    def tasks(e: Employee) = e.tasks
    def tsk(t: Task) = t.tsk
  }

  implicit object ToStringNested extends Nested[λ[x => Int => String]] {
    def Department(dpt: Int => String, employees: Int => String) = { i =>
      s"Department(${dpt(i)}, ${employees(i)})"
    }
    def Employee(emp: Int => String, tasks: Int => String) = { i =>
      s"Employee(${emp(i)}, ${tasks(i)})"
    }
    def Task(tsk: Int => String) = { i =>
      s"Task(${tsk(i)})"
    }
    def dpt(d: Int => String) = i => s"${d(i)}.dpt"
    def employees(d: Int => String) = i => s"${d(i)}.employees"
    def emp(e: Int => String) = i => s"${e(i)}.emp"
    def tasks(e: Int => String) = i => s"${e(i)}.tasks"
    def tsk(t: Int => String) = i => s"${t(i)}.tsk"
  }
}

