/**
  * Created by buck on 5/1/16.
  */
case class DataStructure(name: String, val providedMethods: Set[Implementation]) {

}

object DataStructure {
  def easyCreate(name: String, methods: List[(String, BigOExpression)]): DataStructure = {
    lazy val res: DataStructure = DataStructure(name, methods.toList.map(x => InternalImplementation(Method(x._1), Map(), x._2, res)).toSet)
    res
  }
}

object DataStructures {
  val dataStructures = Set(
    DataStructure.easyCreate("LinkedList", List(
      ("getFirst", ConstantTime),
      ("getNext", ConstantTime))
    ),
    DataStructure.easyCreate("ArrayList", List(("getByIndex", ConstantTime))),
    DataStructure.easyCreate("TreeList", List(
      ("getByIndex", LogTime),
      ("getNext", ConstantTime),
      ("getFirst", ConstantTime)))
  )
}