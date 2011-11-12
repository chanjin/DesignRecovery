
/**
 * ConstantPoolVisitor.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

package bcel

import java.util.Arrays
import java.util.regex.Pattern
import org.apache.bcel.Constants
import org.apache.bcel.Repository
import org.apache.bcel.classfile._
import org.apache.bcel.generic.ArrayType
import org.apache.bcel.generic.ObjectType
import org.apache.bcel.generic.Type
import org.apache.bcel.util.ClassQueue
import org.apache.bcel.util.ClassSet

class ConstantPoolVisitor(clazz: JavaClass, repo: org.apache.bcel.util.Repository) extends org.apache.bcel.classfile.EmptyVisitor {
    private var _set = new ClassSet
    _set.add(clazz)
    def getClasses = _set.toArray
    def getClassNames = _set.getClassNames

    private var _cp: ConstantPool = null

    def start = {
        _cp = clazz.getConstantPool()
        new org.apache.bcel.classfile.DescendingVisitor(clazz, this).visit()
    }

    private def add(class_name: String): Unit = {
        var classname = class_name.replace('/', '.')
        //println(classname + ": " +  _ignored)
        if (class_name.startsWith("[")) {
            val idx = classname.lastIndexOf("[")
            if (classname(idx + 1) != 'L') return
            classname = classname.substring(idx + 2, classname.length - 1)
            //println("****" + classname)
        }
        if (ClassAnalysisRule.isExcluded(classname)) return

        try {
            val clazz = repo.loadClass(classname)
            _set.add(clazz)
            //if ( _set.add(clazz) ) _queue.enqueue(clazz)
        } catch {
            case e: ClassNotFoundException => // throw new IllegalStateException("Missing class: " + classname + ": " +  e.toString())
                println("Missing: " + classname)
        }
    }

    override def visitConstantClass(cc: ConstantClass): Unit = {
        val class_name = cc.getConstantValue(_cp).asInstanceOf[String]
        add(class_name)

        //println("visit class: " + class_name)
    }



    private def visitRef(ccp: ConstantCP, method: Boolean) = {
        add(ccp.getClass(_cp)) //class_name
        val cnat = _cp.getConstant(ccp.getNameAndTypeIndex(),
            Constants.CONSTANT_NameAndType).asInstanceOf[ConstantNameAndType]
        val signature = cnat.getSignature(_cp)
        //println("visit ref signature: " + signature + " -- " + cnat.getName(_cp))
        if (method) {
            val types = Type.getReturnType(signature) :: Type.getArgumentTypes(signature).toList
            ClassAnalysisRule.collectObjectTypeClassNames(types).foreach(c => add(c))
        } else {
        	ClassAnalysisRule.collectObjectTypeClassNames(Type.getType(signature) :: List()).foreach(c => add(c))
        }
    }

    override def visitConstantMethodref(cmr: ConstantMethodref) = {
        visitRef(cmr, true)
    }

    override def visitConstantInterfaceMethodref(cimr: ConstantInterfaceMethodref) =
        visitRef(cimr, true)

    override def visitConstantFieldref(cfr: ConstantFieldref) =
        visitRef(cfr, false)

}
