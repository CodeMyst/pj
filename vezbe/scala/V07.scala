object V07 {
  def main(args: Array[String]): Unit = {
    val schools = List(MiddleSchool("school 1", 69), SecondarySchool("school 2", 420))

    printAll(schools)

    println("---")

    printAll(onlyBig(schools))

    val bodies = List(Planet("oogabooga", 6000, hasWater = true), Satellite("ouhh", 2000, "oogabooga"))

    println("---")

    satellitesOrbitingAround(List(bodies.last.asInstanceOf[Satellite]), "oogabooga").foreach(println)

    println("---")

    filterz(bodies).foreach(println)
  }

  private def printAll(schools: List[School]): Unit = schools.foreach(println)

  private def onlyBig(schools: List[School]): List[School] = schools.filter(_.students >= 300)

  private def satellitesOrbitingAround(satellites: List[Satellite], planet: String): List[Satellite] = satellites.filter(_.orbitsAround.equals(planet))

  private def filterz(bodies: List[CelestialBody]): List[CelestialBody] = bodies.filter {
    case Planet(_, radius, hasWater) => radius >= 5500 && hasWater
    case Satellite(_, radius, _) => radius >= 1000
  }
}

abstract class School(val name: String, val students: Int)

case class MiddleSchool(override val name: String, override val students: Int)
  extends School(name, students) {

  override def toString: String = s"Middle school $name has $students students."
}

case class SecondarySchool(override val name: String, override val students: Int)
  extends School(name, students) {

  override def toString: String = s"Secondary school $name has $students students."
}

abstract class CelestialBody(val name: String, val radius: Float)

case class Planet(override val name: String, override val radius: Float, hasWater: Boolean)
  extends CelestialBody(name, radius) {

  override def toString: String = s"Planet $name, radius: ${radius}km, has water: $hasWater"
}

case class Satellite(override val name: String, override val radius: Float, orbitsAround: String)
  extends CelestialBody(name, radius) {

  override def toString: String = s"Satellite $name, radius: ${radius}km, orbits around: $orbitsAround"
}
