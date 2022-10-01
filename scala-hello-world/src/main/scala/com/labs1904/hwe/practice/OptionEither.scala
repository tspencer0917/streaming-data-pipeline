package com.labs1904.hwe.practice

case class Item(description: String, price: Option[Int])

case class WeatherStation(name: String, temperature: Option[Int])

object OptionEither {
  /*
    Returns age of a dog when given a human age.
    Returns None if the input is None.
  */
  def dogAge(humanAge: Option[Int]): Option[Int] = humanAge match {
    case None => None
    case Some(age) => Some(age*7)
  }

  /*
    Returns the total cost af any item.
    If that item has a price, then the price + 7% of the price should be returned.
  */
  def totalCost(item: Item): Option[Double] = item.price match {
    case None => None
    case Some(price) => Some(price*1.07)
  }

  /*
    Given a list of weather temperatures, calculates the average temperature across all weather stations.
    Some weather stations don't report temperature
    Returns None if the list is empty or no weather stations contain any temperature reading.
   */
  def averageTemperature(temperatures: List[WeatherStation]): Option[Int] = {
    val actualTemps = temperatures.flatMap(_.temperature)
    actualTemps match {
      case Nil => None
      case _ => Some(actualTemps.sum / actualTemps.length)
    }
  }
}
