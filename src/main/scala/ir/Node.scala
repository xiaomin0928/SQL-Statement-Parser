package ir

abstract class IR

/**
 * Trait for a node in a SQL abstract syntax tree.
 */

trait Node extends IR {
  def emit: String
  override def toString: String = emit
}
