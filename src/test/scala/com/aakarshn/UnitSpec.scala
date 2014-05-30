
package com.aakarshn {

import org.scalatest._
import org.scalatest.FlatSpec
import org.scalatest.Assertions._

abstract class UnitSpec extends FlatSpec with Matchers with
  OptionValues with Inside with Inspectors


}
