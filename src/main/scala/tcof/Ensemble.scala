package tcof

import tcof.InitStages.InitStages
import tcof.Utils._

import scala.collection.mutable

trait Ensemble extends WithConfig with WithName with WithUtility with WithEnsembleGroups with WithRoles with WithPermissions with CommonImplicits with Initializable {
  private[tcof] val _membershipClauseFuns = mutable.ListBuffer.empty[() => Logical]

  utility(_solverModel.sum(
    _roles.values.map(_.cardinality)
  ))

  def membership(clause: => Logical): Unit = {
    _membershipClauseFuns += clause _
  }

  private[tcof] def _buildEnsembleClause: Logical = {
    if (_membershipClauseFuns.nonEmpty)
      _buildEnsembleGroupClause && _solverModel.and(_membershipClauseFuns.map(_.apply()))
    else _buildEnsembleGroupClause
  }

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        for (role <- _roles.values) {
          if (role.minCardinality >= 0) {
            membership(role.cardinality >= role.minCardinality)
          }

          if (role.maxCardinality >= 0) {
            membership(role.cardinality <= role.maxCardinality)
          }
        }
      case _ =>
    }
  }


  override def toString: String =
    s"""Ensemble "$name":\n${indent(_roles.values.mkString(""), 1)}${indent(_ensembleGroups.mkString(""), 1)}"""

  def toStringWithUtility: String = {
    s"""Ensemble "$name" (utility: $solutionUtility):\n${indent(_roles.values.mkString(""), 1)}${indent(_ensembleGroups.mapValues(_.toStringWithUtility).mkString(""), 1)}"""
  }

  implicit def iterableToMembersStatic[ComponentType <: Component](components: Iterable[ComponentType]): RoleMembersStatic[ComponentType] = new RoleMembersStatic(components)
  implicit def ensembleGroupToMembers[EnsembleType <: Ensemble](group: EnsembleGroup[EnsembleType]): EnsembleGroupMembers[EnsembleType] = group.allMembers
}
