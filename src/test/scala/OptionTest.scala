import org.scalatest.FlatSpec

class OptionTest extends FlatSpec {
  case class Employee(name: String, department: String, manager: Option[String])

  def lookupByName(name: String): Option[Employee] = name match {
    case "Joe" => Some(Employee("Joe", "Finances", Some("Julie")))
    case "Mary" => Some(Employee("Mary", "IT", None))
    case "Izumi" => Some(Employee("Izumi", "IT", Some("Mary")))
    case _ => None
  }

  def getDepartment: (Option[Employee]) => Option[String] = _.map(_.department)

  "getDepartment for existing Employee or non-existing Employee" should "be their Department or None respecrfully" in {
    assert((getDepartment(lookupByName("Joe"))==Some("Finances")) &&
    (getDepartment(lookupByName("Mary")) == Some("IT")) &&
    (getDepartment(lookupByName("Foo")) == None))
  }

}
