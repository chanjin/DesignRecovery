
/**
 * ClassInformation.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */


package bcel

import org.apache.bcel._
import org.apache.bcel.classfile._
import java.io.File

import org.apache.bcel.generic.ArrayType
import org.apache.bcel.generic.ObjectType
import org.apache.bcel.generic.Type



object ClassInformation {
  private var repository: org.apache.bcel.util.Repository = null
  def setRepo(repo: org.apache.bcel.util.Repository) = repository = repo

  def getOverridingMethods(cls: JavaClass, ancestor: JavaClass) = {
    def isNotConstrurctorNStatic(m: Method): Boolean =
      !m.isStatic && (m.getName != "<init>") // init

    val ancestorMethods = ancestor.getMethods.toList
    cls.getMethods.toList.filter(m => (ancestorMethods.contains(m) && isNotConstrurctorNStatic(m))).map(_.getName)
  }

  // constant pool dependencies including dependencies in method body
  def getConstPoolClasses(clazz: JavaClass): List[String] = {
    val depvisitor = new ConstantPoolVisitor(clazz, repository)
    depvisitor.start
    depvisitor.getClassNames.toList.remove(c => c == clazz.getClassName || ClassAnalysisRule.isExcluded(c))
  }

  // structural dependency types: inheritance, method declaration, field declaration 
  def getStructuralDependents(clazz: JavaClass): List[String] = {
    var deps = List[String]()

    deps = getAncestors(clazz).map(_.getClassName) ::: deps
    clazz.getMethods.toList.foreach(m => {
      deps = ClassAnalysisRule.collectObjectTypeClassNames(m.getReturnType :: m.getArgumentTypes.toList) ::: deps
    })
    deps = ClassAnalysisRule.collectObjectTypeClassNames(clazz.getFields.toList.map(_.getType)) ::: deps
    deps.remove(c => c == clazz.getClassName || ClassAnalysisRule.isExcluded(c))
  }

  def getSuperClass(clazz: JavaClass): JavaClass = {
    var parents = List[JavaClass]()
    try {
      if (clazz.getSuperClass == null || ClassAnalysisRule.isExcluded(clazz.getSuperClass.getClassName)) null
      else clazz.getSuperClass
    } catch {
      case ex: ClassNotFoundException => println("In " + clazz.getClassName + ": " + ex); null
    }
  }

  def getInterfaces(clazz: JavaClass): List[JavaClass] = {
    val _interfaces = clazz.getInterfaceNames.toList
    var classes = List[JavaClass]()
    for (intf <- _interfaces) {
      try {
        val cls = repository.loadClass(intf)
        if (cls != null && !ClassAnalysisRule.isExcluded(cls.getClassName)) classes = cls :: classes
      } catch {
        case ex: ClassNotFoundException => println("In " + clazz.getClassName + ": " + ex); null
      }
    }
    classes
  }

  def getAncestors(clazz: JavaClass): List[JavaClass] = {
    var ancestors = List[JavaClass]()
    var cls = clazz
    while (cls != null) {
      cls = getSuperClass(cls)
      if (cls != null) ancestors = cls :: ancestors
    }
    var queue = new org.apache.bcel.util.ClassQueue()

    queue.enqueue(clazz)
    while (!queue.empty) {
      cls = queue.dequeue
      val souper = getSuperClass(cls)
      val _interfaces = getInterfaces(cls)
      if (cls.isInterface())
        ancestors = cls :: ancestors
      else if (souper != null) queue.enqueue(souper)
      _interfaces.foreach(queue.enqueue(_))
    }

    ancestors.remove(_ == clazz)
  }
}

object ClassAnalysisRule {
  import java.util.regex.Pattern
  private var _exclude = List("java[.].*", "javax[.].*", "sun[.].*", "sunw[.].*",
    "com[.]sun[.].*", "org[.]omg[.].*", "org[.]w3c[.].*", "org[.]xml[.].*", "net[.]jini[.].*")
  def getExcludeTypes = _exclude
  def setExcludeTypes(v: Array[String]) = _exclude = v.toList
  def isExcluded(clsname: String) = _exclude.exists(ign => (Pattern.matches(ign, clsname)))

  def getObjectType(typ: Type): Type = {
    var t = typ
    if (t.isInstanceOf[ArrayType])
      t = t.asInstanceOf[ArrayType].getBasicType
    if (t.isInstanceOf[ObjectType]) t else null
  }

  def collectObjectTypeClassNames(types: List[Type]) = {
    types.foldLeft(List[ObjectType]())((l, t) => {
      val otype = getObjectType(t)
      if (otype != null) otype.asInstanceOf[ObjectType] :: l
      else l
    }).map(_.getClassName)
  }
}