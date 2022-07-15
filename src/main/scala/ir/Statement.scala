package ir

// SelectStatement:
final case class SelectStatement(projections: Array[Projection],
                                 projectionType: Option[ProjectionTypeEnum],
                                 selection: Option[Selection],
                                 groupBy: Option[GroupBy],
                                 window: Option[Window],
                                 orderBy: Option[OrderBy],
                                 limit: Option[Limit],
                                ) extends Node {
  override def emit: String =
    s"""
       SELECT ${if(projectionType.isDefined){s"${projectionType.get}"}else{""}}${projections.mkString(", ")}
       ${if(selection.isDefined){s"WHERE ${selection.get}"}else{""}}
       ${if (groupBy.isDefined) {s"GROUP BY ${groupBy.get}"}else{""}}
       ${if (window.isDefined) {s"WINDOW ${window.get}"}else{""}}
       ${if (orderBy.isDefined) {s"ORDER BY ${orderBy.get}"}else{""}}
       ${if (limit.isDefined) {s"LIMIT ${limit.get}"}else{""}}
       """.trim.replaceAll("(?:\\s*\\n){2,}","\n")

  def toIR = s"""SelectStatement(projections = {${projections.mkString(", ")}},
                |                                 projectionType = $projectionType,
                |                                 selection = $selection,
                |                                 groupBy = $groupBy,
                |                                 window = $window,
                |                                 orderBy = $orderBy,
                |                                 limit = $limit,
                |                                )""".stripMargin
}

enum ProjectionTypeEnum {
  case distinct, all
  
  override def toString: String = this match {
    case ProjectionTypeEnum.distinct => "DISTINCT"
    case ProjectionTypeEnum.all => "ALL"
  }
}