package deduction

trait Schema[A] {
  def matches(a: A): List[Map[String, Any]]
  def hasMatch(a: A): Boolean
  def consistentNamings(listOfNamingLists: List[List[Map[String, Any]]]): List[Map[String, Any]] = {
    listOfNamingLists.foldLeft(Map.empty[String, Any] :: Nil) {
      case (prevNamings, headNamings) => for {
        names1 <- headNamings
        names2 <- prevNamings
        combined <- combineNamings(names1, names2)
      } yield combined
    }
  }
  def combineNamings(map1: Map[String, Any], map2: Map[String, Any]): Option[Map[String, Any]] = {
    val namesAgree = map1.keys.forall(k => !map2.contains(k) || map1(k) == map2(k))
    if(namesAgree) Some(map1 ++ map2)
    else None
  }
}
