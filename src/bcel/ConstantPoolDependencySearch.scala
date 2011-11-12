/**
 * ConstantPoolDependencySearch.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

package bcel

import java.util.Arrays
import java.util.regex.Pattern
import org.apache.bcel._
import org.apache.bcel.generic._
import org.apache.bcel.classfile._
import org.apache.bcel.util.ClassSet


class ConstantPoolDependencySearch(clazz: JavaClass, repo: org.apache.bcel.util.Repository) {
	private var _set = new ClassSet
	_set.add(clazz)
	def getClasses = _set.toArray
	def getClassNames = _set.getClassNames
	
	
	private val _cp: ConstantPool = clazz.getConstantPool()
	private val _gen = new ConstantPoolGen(_cp)
	

	
	private def add( class_name: String) : Unit = {
		var classname = class_name.replace('/', '.')
		if ( class_name.startsWith("[") ) {
			val idx = classname.lastIndexOf("[")
			if (  classname(idx+1) != 'L' ) return
			classname = classname.substring(idx+2, classname.length - 1)
		}
		if ( ClassAnalysisRule.getExcludeTypes.exists(ign =>  (Pattern.matches(ign, classname) ) ) ) return

				
		try {
			val clazz = repo.loadClass(classname)
			_set.add(clazz)
		}
		catch {
			case e: ClassNotFoundException => ()
			case e: IllegalArgumentException => ()
//				println("Missing class: " + classname )
		}
	}
	
	def checkDependency(cc: ConstantClass) {
		val class_name = cc.getConstantValue(_cp).asInstanceOf[String]
		add(class_name)
	}
	
	private def checkType(typ: Type) = {
		var t = typ
		if (t.isInstanceOf[ArrayType]) 
			t = t.asInstanceOf[ArrayType].getBasicType
			
		if (t.isInstanceOf[ObjectType]) 
			add(t.asInstanceOf[ObjectType].getClassName)
	}
	
	def checkDependency(ccp: ConstantCP, method: Boolean) {
		val class_name = ccp.getClass(_cp)
		add(class_name)
		
		val cnat = _cp.getConstant(ccp.getNameAndTypeIndex(),
				Constants.CONSTANT_NameAndType).asInstanceOf[ConstantNameAndType]
		val signature = cnat.getSignature(_cp)
		if (method) {
			val t = Type.getReturnType(signature)
			checkType(t)
			val types = Type.getArgumentTypes(signature)
			types.foreach(t => checkType(t))
		} else {
			checkType(Type.getType(signature))
		}
	}
	
	def checkDependency(cnat: ConstantNameAndType) {
	}
	
	def checkDependency(cutf8: ConstantUtf8) {
		var class_name = cutf8.getBytes()
		if ( class_name.startsWith("L") && class_name.endsWith(";") ) {
			class_name = "[" + class_name.substring(0, class_name.length - 1) + "]"
		}
		add(class_name)
	}
	
	def search() {
		val constants = _cp.getConstantPool().toList
		constants.foreach(constant => {
			if (constant != null) {
				constant.getTag() match { 
					case Constants.CONSTANT_Class => checkDependency(constant.asInstanceOf[ConstantClass])
//					case Constants.CONSTANT_Fieldref => checkDependency(constant.asInstanceOf[ConstantCP], false)
//					case Constants.CONSTANT_Methodref => checkDependency(constant.asInstanceOf[ConstantCP], true)
//					case Constants.CONSTANT_InterfaceMethodref => checkDependency(constant.asInstanceOf[ConstantCP], true)
//					case Constants.CONSTANT_NameAndType => checkDependency(constant.asInstanceOf[ConstantNameAndType])
//					case Constants.CONSTANT_Utf8 => checkDependency(constant.asInstanceOf[ConstantUtf8])
					case _ => ()
				}
			}
		})
	}
}