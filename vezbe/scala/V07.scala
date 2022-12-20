object V07 {
  def main(args: Array[String]): Unit = {
    val schools = List(new MiddleSchool("school 1", 69), new SecondarySchool("school 2", 420))

    printAll(schools)

    println("---")

    printAll(onlyBig(schools))

    val bodies = List(new Planet("oogabooga", 6000, true), new Satellite("ouhh", 2000, "oogabooga"))

    println("---")

    satellitesOrbitingAround(List(bodies.last.asInstanceOf[Satellite]), "oogabooga").foreach(println)

    println("---")

    filterz(bodies).foreach(println)
  }

  private def printAll(schools: List[School]): Unit = schools.foreach(println)

  private def onlyBig(schools: List[School]): List[School] = schools.filter(_.students >= 300)

  private def satellitesOrbitingAround(satellites: List[Satellite], planet: String): List[Satellite] = satellites.filter(_.orbitsAround.equals(planet))

  private def filterz(bodies: List[CelestialBody]): List[CelestialBody] = bodies.filter(
    _ match {
      case p: Planet => p.hasWater && p.radius >= 5500
      case s: Satellite => s.radius >= 1000
    }
  )
}

abstract class School(val name: String, val students: Int)

class MiddleSchool(override val name: String, override val students: Int)
  extends School(name, students) {

  override def toString: String = "Middle school " + name + " has " + students + " students."
}

class SecondarySchool(override val name: String, override val students: Int)
  extends School(name, students) {

  override def toString: String = "Secondary school " + name + " has " + students + " students."
}

abstract class CelestialBody(val name: String, val radius: Float)

class Planet(override val name: String, override val radius: Float, val hasWater: Boolean)
  extends CelestialBody(name, radius) {

  override def toString: String = "Planet " + name + ", radius: " + radius + "km, has water: " + hasWater
}

class Satellite(override val name: String, override val radius: Float, val orbitsAround: String)
  extends CelestialBody(name, radius) {

  override def toString: String = "Satellite " + name + ", radius: " + radius + "km, orbits around: " + orbitsAround
}
