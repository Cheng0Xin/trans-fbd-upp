package mcfsm

import scala.xml.Elem


class Uppaal {
  type LstString = List[String]

  val gloDecls: LstString = List.empty
  val templates: List[Template] = List.empty
  val defs: LstString = List.empty
}

case class Position(x: Int, y: Int)
case class Location(id: String, name: String, expRate: String,
  invExp: List[String] = List.empty, pos: Position = null)
case class Edge(srcRef: Location, dstRef: Location, sels: List[String] = List.empty,
  grds: List[String] = List.empty, upts: List[String] = List.empty,
  sync: String = null)

class Template {
  var name: String = null
  var params: List[String] = List.empty
  var decls: List[String] = List.empty
  var locations: List[Location] = List.empty
  var edges: List[Edge] = List.empty

  var initLoc: Location = null

  def concatExp(exps: List[String], seq: String): String = {
    exps match {
      case Nil => null
      case x::xs => xs.foldLeft(x)((acc, x) => {
        f"$acc $seq $x"
      })
    }
  }

  def renderLoc(loc: Location): Elem = {
    <location id={loc.id} x={loc.pos.x.toString} y={loc.pos.y.toString}>
      <name> {loc.name} </name>
      {
        if (loc.expRate != null) <label kind="exponentialrate">loc.expRate</label>
        else null
      }
      {
        if (!loc.invExp.isEmpty) {
          <label kind="invariant"> { concatExp(loc.invExp, "&&") } </label>
        } else null
      }
    </location>
  }

  def renderEdge(edge: Edge): Elem = {
    <transition>
      <source ref={ edge.srcRef.id } />
      <source ref={ edge.dstRef.id } />
      {
        if (!edge.sels.isEmpty) {
          <label kind="selection"> { concatExp(edge.sels, ",") } </label>
        } else null
      }
      {
        if (!edge.grds.isEmpty) {
          <label kind="guard"> { concatExp(edge.grds, "&&") } </label>
        } else null
      }
      {
        if (edge.sync != null) {
          <label kind="synchronisation"> { edge.sync } </label>
        } else null
      }
      {
        if (!edge.upts.isEmpty) {
          <label kind="assignment"> { concatExp(edge.upts, ",") } </label>
        } else null
      }
    </transition>
  }

  def render: Elem = {
    <template>
      <name>{ name }</name>
      {
        if (!locations.isEmpty) locations.map(renderLoc)
        else null
      }
      <init ref={initLoc.id} />
      {
        if (!edges.isEmpty) edges.map(renderEdge)
        else null
      }
    </template>
  }
}