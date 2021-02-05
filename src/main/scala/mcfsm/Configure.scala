package mcfsm


object Configure {
  val dir = "/home/xin/Workspace/Scala/mcfsm-transf/data/t-project"
  var id = 0
  def getId: String = {
    val strId = f"id${id}"
    id = id + 1
    strId
  }
}