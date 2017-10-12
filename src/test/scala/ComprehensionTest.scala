import org.scalatest.FlatSpec

class ComprehensionTest extends FlatSpec {
  val xs = List(1,2,3,4,5)
  val ys = List(1)
  "List sum of (1,2,3,4,5)" should "be equal 15" in {
    assert(List.sum(xs)== (15))
  }
  "Mondic for comprehenshion" should "work on our List" in {
  val res = for{
    x <- xs if x % 2 == 0
    y <- ys
  } yield (x,y) 
  assert(res == List((2,1),(4,1)))
  }
  
  "Foreach with print " should "print 12345 to standart" in {
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {
      for (x<-xs) print(x)
    }
    assert(stream.toString == "12345") 
  }
}
