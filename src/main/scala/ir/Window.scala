package ir

// windows (probably) not necessary, development of all classes below on hold (!)

case class Window(windowNames: Array[String],
                  windowDefinitions: Array[WindowDefinition]
                 ) extends Node {
  override def emit = "WINDOW STILL STRING MISSING"//windowNames.foreach() mkString(" AS " + windowDefinitions.mkString(",")
}

case class WindowDefinition(baseWindowName: Option[String],
                            partitionExpressions: Option[Array[Expression]],
                            orderingTerms: Option[Array[OrderingTerm]],
                            frame: Option[Frame]
                           ) extends Node {
  override def emit: String = "( " + (if(baseWindowName.isDefined){baseWindowName.getOrElse("")}else{""}) + (if(partitionExpressions.isDefined){ " PARTITION BY " + partitionExpressions.mkString(", ")}else{""}) + (if(!orderingTerms.isEmpty){ " ORDER BY " + orderingTerms.mkString(", ")}else{""}) + (if(!frame.isEmpty){ frame.toString}else{""}) + " )"
}

case class Frame(frameType: FrameTypeEnum,
                 frameBoundary: Option[FrameBoundaryEnum],
                 precedingExpression: Option[Expression],
                 excludeType: Option[ExcludeEnum]
                ) extends Node {
  override def emit: String = frameType.toString + (if(frameBoundary.isDefined){frameBoundary.get}else{
    (if(precedingExpression.isDefined){precedingExpression.toString + " PRECEDING "}else{""})}) +
    (if(excludeType.isDefined){
      if(excludeType.get.ordinal == 0){" EXCLUDE NO OTHERS "}else
        if(excludeType.get.ordinal == 1){" EXCLUDE CURRENT ROW "}else
          if(excludeType.get.ordinal ==2){" EXCLUDE GROUP "}else
            if(excludeType.get.ordinal ==3){" EXCLUDE TIES "}else{""}
    }else{""})
}

enum FrameTypeEnum {
  case range, rows, groups

  override def toString: String = this match {
    case FrameTypeEnum.range => "RANGE"
    case FrameTypeEnum.rows => "ROWS"
    case FrameTypeEnum.groups => "GROUPS"
  }
}

enum FrameBoundaryEnum {
  case unboundedPreceding, unboundedFollowing, currentRow

  override def toString: String = this match {
    case FrameBoundaryEnum.unboundedPreceding => "UNBOUNDED PRECEDING"
    case FrameBoundaryEnum.unboundedFollowing => "UNBOUNDED FOLLOWING"
    case FrameBoundaryEnum.currentRow => "CURRENT ROW"
  }
}

enum ExcludeEnum {
  case excludeNoOthers, excludeCurrentRow, excludeGroup, excludeTies

  override def toString: String = this match {
    case ExcludeEnum.excludeNoOthers => "EXCLUDE NO OTHERS"
    case ExcludeEnum.excludeCurrentRow => "EXCLUDE CURRENT ROW"
    case ExcludeEnum.excludeGroup => "EXCLUDE GROUP"
    case ExcludeEnum.excludeTies => "EXCLUDE TIES"
  }
}

case class FrameBetween(frameType: FrameTypeEnum,
                        betweenBoundary: Option[FrameBoundaryEnum],
                        betweenPrecedingExpression: Option[Expression],
                        betweenFollowingExpression: Option[Expression],
                        andBoundary: Option[FrameBoundaryEnum],
                        andPrecedingExpression: Option[Expression],
                        andFollowingExpression: Option[Expression],
                        excludeType: Option[ExcludeEnum]
                       ) extends Node {
  override def emit: String = frameType.toString + " BETWEEN " + (if(betweenBoundary.isDefined){betweenBoundary.get}else{
    (if(betweenPrecedingExpression.isDefined){betweenPrecedingExpression.toString + " PRECEDING "}else{
      (if(betweenFollowingExpression.isDefined){betweenFollowingExpression.toString + " FOLLOWING "}else{""})})}) +
    " AND " +
    (if(andBoundary.isDefined){andBoundary.get}else{
      (if(andPrecedingExpression.isDefined){andPrecedingExpression.toString + " PRECEDING "}else{
        (if(andFollowingExpression.isDefined){andFollowingExpression.toString + " FOLLOWING "}else{""})})}) +
    (if(excludeType.isDefined){if(excludeType.get.ordinal == 0){" EXCLUDE NO OTHERS "}else
      if(excludeType.get.ordinal == 1){" EXCLUDE CURRENT ROW "}else
        if(excludeType.get.ordinal ==2){" EXCLUDE GROUP "}else
          if(excludeType.get.ordinal ==3){" EXCLUDE TIES "}else{""}
    }else{""})
}

