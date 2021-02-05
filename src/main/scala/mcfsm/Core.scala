package mcfsm

import FBD._
import Configure._
import scala.xml.XML
import scala.util.matching.Regex

object Core {
  def getFBD() = {
    val appXML = Parser.loadApplication(f"${dir}/Test.xml")
    val subAppNXML = Parser.getSubAppNetwork(appXML)
    val fbd = Parser.convertSubAppNetwork(subAppNXML.head)
    fbd
  }

  def genTemplate(axis: String, ins: List[FBInstance], 
    conn: List[Connection]) = {

    def genEdge(start: Location, end: Location, 
      fins: List[FBInstance]): Edge = {

      // inputs of FB instance to updates of edge
      def param2Upt(params: List[String]) = {
        def matchParamater(name:String): Regex = {
          f"${name}=(.*?)".r
        }
        val matchPos = matchParamater("Position")
        val matchVel = matchParamater("Velocity")

        var uptLists = params.map(x => {
          x match {
            case matchPos(pos) => f"buf_position[${axis}]=${pos}"
            case matchVel(vel) => f"buf_velocity[${axis}]=${vel}"
            case _ => null
          }
        }).filter(x => x != null)

        uptLists = f"buf_accelerate[${axis}]=DEF_ACC" :: 
          f"buf_decelerate[${axis}]=DEF_DEC" :: uptLists
        uptLists
      }

      def genGuard(ins: FBInstance) = {
        var guards:List[String] = List.empty
        val preInses = conn.filter(x => x.dstIns.name.equals(ins.name))
        if (preInses.isEmpty) {
          val condition = ins.values.filter(x => x.startsWith("Execute"))
          if (!condition.isEmpty) {
            val mathchCondition = "Execute=(.*?)".r
            val mathchCondition(cnd) = condition(0)
            guards = cnd :: guards
          }
        } else {
          guards = preInses.map(x => {
            f"${x.srcIns.name}.${x.srcPort}"
          })
        }
        guards
      }

      var guards:List[String] = List.empty
      var updates:List[String] = List.empty
      var selections:List[String] = List.empty
      var sync:String = null

      if (start.name.equals("Init")) {
        // Init -> First instance
        sync = f"motion[${axis}]!"
        val curIns = fins.head

        // Update
        updates = f"${curIns.name}.active=true" :: 
          param2Upt(curIns.values)
        curIns.fbType.name match {
          case "MC_MoveAbsolute" => updates = f"disc_s[${axis}]=true" :: updates
          case "MC_MoveVelocity" => updates = f"cont_s[${axis}]=true" :: updates
          case _ => throw new ParserException.ParseValueNotFoundException
        }

        // Sync
        sync = f"motion[${axis}]!"

        // Guard
        guards = genGuard(curIns)

      } else if (start.name.endsWith("D")) {
        // Edge between Instances
        val curIns = fins.head

        // Update
        updates = f"${curIns.name}.active=true" :: 
          param2Upt(curIns.values)

        curIns.fbType.name match {
          case "MC_MoveAbsolute" => 
            updates = f"disc_s[${axis}]=true" :: updates
          case "MC_MoveVelocity" => 
            updates = f"cont_s[${axis}]=true" :: updates
          case _ => 
            throw new ParserException.ParseValueNotFoundException
        }

        // Sync
        sync = f"motion[${axis}]!"

        // Guard
        guards = genGuard(curIns)
        
      } else {
        // Edge in single Instance
        val curIns = fins.head

        // Update
        curIns.fbType.name match {
          case "MC_MoveAbsolute" => updates = 
            f"${curIns.name}.done=disc_d[${axis}]" ::
            f"disc_d[${axis}]=false" :: updates
          case "MC_MoveVelocity" => updates = 
            f"${curIns.name}.inVelocity=cont_i[${axis}]" :: 
            f"cont_i[${axis}]=false" :: updates
          case _ => 
            throw new ParserException.ParseValueNotFoundException
        }

        // Sync
        sync = f"done[${axis}]?"
      }

      Edge(start, end, selections, guards, updates, sync)
    }

    def genLoc(ins: FBInstance): (Location, Location) = {
      val index = ".*?([0-9]+).*?".r
      val index(partialName) = ins.name
      (
        Location(getId, f"M${partialName}", "M", List.empty, Position(0, 0)),
        Location(getId, f"M${partialName}D", "M", List.empty, Position(0, 0))
      )
    }

    // Filter and sort
    var insFilter = ins.filter(x => {
      x.values.contains(f"AxisI=${axis}")
    }).sortBy(x => x.order)

    val idleLoc = Location(getId, "Idle", "M", List.empty, Position(0, 0))
    val initLoc = Location(getId, "Init", "M", List.empty, Position(0, 0))
    val iiEdge = Edge(idleLoc, initLoc, List.empty, 
        List.empty, List.empty, "initial!")

    var locList:List[Location] = idleLoc :: initLoc :: List.empty
    var edgeList:List[Edge] = iiEdge :: List.empty
    var ccLoc = initLoc

    while(!insFilter.isEmpty) {
      val (mLoc, mdLoc) = genLoc(insFilter.head)
      val midEdge = genEdge(mLoc, mdLoc, insFilter)
      val conEdge = genEdge(ccLoc, mLoc, insFilter)
      ccLoc = mdLoc

      edgeList = conEdge :: midEdge :: edgeList
      locList = mLoc :: mdLoc :: locList

      insFilter = insFilter.tail
    }

    val endLoc = Location(getId, "End", null, List.empty, Position(0, 0))
    val lastEdge = Edge(ccLoc, endLoc, List.empty, List("Finish"), List.empty)

    locList = endLoc :: locList
    edgeList = lastEdge :: edgeList
    (idleLoc, locList, edgeList)

    // Construct Template
    val template = new Template
    template.locations = locList
    template.initLoc = idleLoc
    template.edges = edgeList
    template.name = f"FBDP${axis}M"
    template
  }

  def main(args: Array[String]) {
    println("mcfsm: start transformation !\n")

    println("The original FBD program")
    val (ins, conn) = getFBD

    println("List FB instances:\n")
    ins.foreach(x => println(f"${x.name}: ${x.fbType.name}"))
    println()

    println("List Connections:\n")
    conn.foreach(x => println(
      f"${x.srcIns.name}:${x.srcPort} -> " +
        f"${x.dstIns.name}:${x.dstPort}"))
    println()

    def outputTemplate(axis: String) = {
      val tx = genTemplate(axis, ins, conn)
      println(f"Translated Locations (axis ${axis}): \n")
      tx.locations.foreach(x => println(f"${x.name}:${x.id}"))
      println()

      println(f"Translated Edges (axis ${axis}): \n")
      tx.edges.foreach(x => println(
        f"${x.srcRef.name} -> " +
        f"${x.dstRef.name}"))
      println()

      println(f"Translated Uppaal template file (axis ${axis}): \n")
      println(tx.render)
      println()
    }

    List("X", "Y").foreach(outputTemplate)

  }
}
