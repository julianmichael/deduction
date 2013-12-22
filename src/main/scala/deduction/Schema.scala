package deduction 

trait Schema[A] {
  def matches(a: A): List[Map[String, Any]]
  def consistentNamings(listOfNamingLists: List[List[Map[String, Any]]]): List[Map[String, Any]] = {
    listOfNamingLists match {
      case Nil => Map[String, Any]() :: Nil
      case headNamings :: tail => {
        val tailNamings = consistentNamings(tail)
        val goodOnes = (for (names1 <- headNamings; names2 <- tailNamings) yield {
          nameAgreement(names1 :: names2 :: Nil)
        }).flatten
        goodOnes
      }
    }
  }
  def nameAgreement(nameMaps: List[Map[String, Any]]) = {
    val allNames = nameMaps.map(_.toList).reduce(_ ++ _)
    val groupList = allNames.groupBy(_._1).map(_._2).map(list => list.map(_._2))
    val namesAgree = groupList.forall(list => list.forall(_.equals(list(0))))
    val namesMap = nameMaps.reduce(_ ++ _)
    if (namesAgree) Some(namesMap)
    else None
  }
}
