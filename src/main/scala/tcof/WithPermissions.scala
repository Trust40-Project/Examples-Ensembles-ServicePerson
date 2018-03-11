package tcof

import scala.collection.mutable

abstract class AllowPermission
case class AllowPermissionVV(subj: Role[_ <: Component], obj: Role[_ <: Component], action: String, privacyLevel: PrivacyLevel) extends AllowPermission
case class AllowPermissionVC(subj: Role[_ <: Component], obj: IndexedSeq[Component], action: String, privacyLevel: PrivacyLevel) extends AllowPermission
case class AllowPermissionCV(subj: IndexedSeq[Component], obj: Role[_ <: Component], action: String, privacyLevel: PrivacyLevel) extends AllowPermission
case class AllowPermissionCC(subj: IndexedSeq[Component], obj: IndexedSeq[Component], action: String, privacyLevel: PrivacyLevel) extends AllowPermission

abstract class PrivacyLevel
object PrivacyLevel {
  case object SECRET extends PrivacyLevel
  case object RESTRICTED extends PrivacyLevel
  case object PUBLIC extends PrivacyLevel
  case object UNDEFINED extends PrivacyLevel
}

trait WithPermissions {
  private[tcof] var _allowPermissions = mutable.ListBuffer.empty[AllowPermission]

  def allow(subj: Role[_ <: Component], obj: Role[_ <: Component], action: String): Unit =
    _allowPermissions += new AllowPermissionVV(subj, obj, action, PrivacyLevel.UNDEFINED)
  def allow(subj: Role[_ <: Component], obj: Component, action: String): Unit =
    _allowPermissions += new AllowPermissionVC(subj, Array(obj), action, PrivacyLevel.UNDEFINED)
  def allow(subj: Component, obj: Role[_ <: Component], action: String): Unit =
    _allowPermissions += new AllowPermissionCV(Array(subj), obj, action, PrivacyLevel.UNDEFINED)
  def allow(subj: Component, obj: Component, action: String): Unit =
    _allowPermissions += new AllowPermissionCC(Array(subj), Array(obj), action, PrivacyLevel.UNDEFINED)
  def allow(subj: Role[_ <: Component], obj: Role[_ <: Component], action: String, privacyLevel: PrivacyLevel): Unit =
    _allowPermissions += new AllowPermissionVV(subj, obj, action, privacyLevel)
  def allow(subj: Role[_ <: Component], obj: Component, action: String, privacyLevel: PrivacyLevel): Unit =
    _allowPermissions += new AllowPermissionVC(subj, Array(obj), action, privacyLevel)
  def allow(subj: Component, obj: Role[_ <: Component], action: String, privacyLevel: PrivacyLevel): Unit =
    _allowPermissions += new AllowPermissionCV(Array(subj), obj, action, privacyLevel)
  def allow(subj: Component, obj: Component, action: String, privacyLevel: PrivacyLevel): Unit =
    _allowPermissions += new AllowPermissionCC(Array(subj), Array(obj), action, privacyLevel)
}
