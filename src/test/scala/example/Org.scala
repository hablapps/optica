package example

case class Department(dpt: String, employees: List[Employee])

case class Employee(emp: String, tasks: List[Task])

case class Task(tsk: String)

