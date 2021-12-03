package bigvis

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.{PyQuote, SeqConverters}


class ScalaPyTest extends AnyFlatSpec with Matchers {
  behavior of "SearchInterval"

  it should "return contained interval" in {
    //python function calculate maximum imbalance takes in a list of floats called data
    //need to be able to pass in Scala List to python function

    //py"" string interpolator runs strings as Python code
    print(py"""def max_imbalance(data: List[float]) -> float:
  return (max(data) - min(data)).as[Double]
  print(max_imbalance(Seq(3.47, 3.46, 3.47, 3.62)))""")
    //.as methods convert Python values back into Scala


  }
}
