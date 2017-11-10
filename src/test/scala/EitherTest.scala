import scala.{Option=>_,Either=>_, _}
import org.scalatest.FlatSpec

class OptionTest extends FlatSpec {
  case class Employee(name: String, department: String, manager: Option[String])

  def lookupByNameViaEither(name: String): Either[String,Employee] = name match {
    case "Joe" => Right(Employee("Joe", "Finances", Some("Julie")))
    case "Mary" => Right(Employee("Mary", "IT", None))
    case "Izumi" => Right(Employee("Izumi", "IT", Some("Mary")))
    case _ => Left("Employee not found")
  }

  "Sequantitng over Employees" should " be Right-value only if all values are Right-values" in {
    assert((Either.sequence(employees) == Right(List(Employee("Joe","Finances",Some("Julie")),Employee("Mary","IT",None))))&& 
    (Either.sequence(employeesAndOutsources) == Left("Employee not found")))
  } 
  def getDepartment: (Either[String,Employee]) => Either[String,String] = _.map(_.department)

  "getDepartment for existing Employee or non-existing Employee" should "be their Department or Left-value respecrfully" in {
    assert((getDepartment(lookupByNameViaEither("Joe"))==Right("Finances")) &&
    (getDepartment(lookupByNameViaEither("Mary")) == Right("IT")) &&
    (getDepartment(lookupByNameViaEither("Foo")) == Left("Employee not found")))
  }
  def getManager(employee: Either[String, Employee]): Either[String, String] =
    employee.flatMap(e =>
      e.manager match {
        case Some(e) => Right(e)
        case _ => Left("Manager not found")
    })
  "getManager for existing Employee or non-existing Employee" should "be their Manager or Left-value respecrfully" in {
    assert((getManager(lookupByNameViaEither("Joe")) == Right("Julie")) && 
    (getManager(lookupByNameViaEither("Mary")) == Left("Manager not found")) && 
    (getManager(lookupByNameViaEither("Foo")) == Left("Employee not found")))
  } 

  "getManager for existing Employee or non-existing Employee" should "correctly work with orElse" in {
    assert((getManager(lookupByNameViaEither("Joe")).orElse(Right("CEO")) == Right("Julie")) && 
    (getManager(lookupByNameViaEither("Mary")).orElse(Right("CEO")) == Right("CEO")) && 
    (getManager(lookupByNameViaEither("Foo")).orElse(Right("CEO")) == Right("CEO")))
  }
  val employees = List(lookupByNameViaEither("Joe"), lookupByNameViaEither("Mary"))
  val employeesAndOutsources = employees :+ lookupByNameViaEither("Foo")

}
