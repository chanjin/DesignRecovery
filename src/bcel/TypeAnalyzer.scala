/**
 * TypeAnalyzer.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

package bcel

import java.io._
import java.util._
import bcel.typeanalysis._
import scala.collection.JavaConversions._

import org.apache.bcel.classfile._
import org.apache.bcel.generic._
import org.apache.bcel.util._


class TypeAnalyzer {
	var repo: Repository = null

	def getRepositoryJar(path: String ) = SepaBcelRepository.getInstance(new ClassPath(path));
	def  getRepositoryDir(path: String ):Repository = SyntheticRepository.getInstance(new ClassPath(path));
	
	@throws(classOf[ClassNotFoundException])
	def run(root: File): HashMap[Integer, TypeObject]  = {
		val loader = new ClassFileLoader();
		val classes: HashMap[Integer, TypeObject] = loader.getClassNames(root) // HashMap<Integer, TypeObject>
		val it = classes.values.iterator
		while (it.hasNext ) {
			val to =  it.next
			to.setJavaClass(repo.loadClass(to.getName)) 
		}
		repo = SyntheticRepository.getInstance(new ClassPath(root.getAbsolutePath()));
		classes
	}

	@throws(classOf[ClassNotFoundException])
	private def getClassArray(path: String, classes:HashMap[Integer, TypeObject] ): Array[Object] = {
		repo = SyntheticRepository.getInstance(new ClassPath(path));
		loadClasses(classes.values)
	}
	
	@throws(classOf[ClassNotFoundException])
	private def loadClasses(tos: java.util.Collection[TypeObject]) : Array[Object] = {
		var classarray = new Array[Object](tos.size())
		var i = 0;
		val it = tos.iterator
		while (it.hasNext ) {
			val to =  it.next
			to.setJavaClass(repo.loadClass(to.getName)) 
			classarray(i) = to.getJavaClass
			i = i + 1
		}
		classarray
	}
	
	@throws(classOf[ClassNotFoundException])
	private def loadClasses(tos: Array[Object]) : Array[Object] = {
		var classarray = new Array[Object](tos.size)
		var i = 0;
		val it = tos.iterator
		while (it.hasNext ) {
			val to =  it.next
			classarray(i) = repo.loadClass(to.asInstanceOf[String])
			i = i + 1
		}
		classarray
	}

	@throws(classOf[ClassNotFoundException])
	def getJavaClasses(root : File ) : Array[Object] = {
		getClassArray(root.getAbsolutePath, (new ClassFileLoader).getClassNames(root))
	}
	
	
	@throws(classOf[ClassNotFoundException])
	def getJavaClasses(roots: Array[File] ) : Array[Object] = {
		var classes = new HashMap[Integer, TypeObject]()
		roots.foreach(root => (new ClassFileLoader).getClassNames(root, classes))

		val buffer = new StringBuffer
		roots.foreach(root => {
			buffer.append(root.getAbsolutePath());
			buffer.append(File.pathSeparator);
		})
		getClassArray(buffer.toString(), classes)
	}

	@throws(classOf[ClassNotFoundException])
	def  getJavaClassesFromJar(jarFile: String): Array[Object] = {
		val repo1 = SepaBcelRepository.getInstance(new ClassPath(jarFile));
		repo1.loadClasses(jarFile);
		repo = repo1;
		loadClasses(repo1.getClassNames.toArray)
	}

	def  getRepository : Repository = repo
	
	@throws(classOf[ClassNotFoundException])
	def  getJavaClassesFromJar(jarFiles: Array[String]): Array[Object] = {
		val buffer = new StringBuffer
		jarFiles.foreach(jarFile => {
			buffer.append(jarFile);
			buffer.append(File.pathSeparator);
		})
		
		System.out.println(buffer.toString());
		val classPath = new ClassPath(buffer.toString());
		val repo1 = SepaBcelRepository.getInstance(classPath);
		jarFiles.foreach(jarFile =>repo1.loadClasses(jarFile))
		repo = repo1;

		loadClasses(repo1.getClassNames.toArray)
	}

	def  getJavaClass( name: String) : JavaClass = {
		var  found: JavaClass = null
		try {
			found = repo.loadClass(name);
		} catch {
			case ex: ClassNotFoundException => System.out.print(ex);
		}
		found;
	}

	@throws(classOf[ClassNotFoundException])
	def printTypes(root: File) = {
		val loader = new ClassFileLoader();
		val classes = loader.getClassNames(root).values.toArray.map(_.asInstanceOf[TypeObject])

		repo = SyntheticRepository.getInstance(new ClassPath(root.getAbsolutePath()))
		classes.foreach(to => {
			val clazz = repo.loadClass(to.getName)
			TypeAnalyzer.hierarchy(clazz)
			TypeAnalyzer.fields(clazz);
			TypeAnalyzer.printCode(clazz.getMethods);
		})
	}
}

object TypeAnalyzer {
	def  printCode(methods: Array[Method]) = {
		methods.foreach(m => {
			System.out.println("Methods >>>>>>>>>>>>>>>>>>>>>>>> ");
			System.out.println(m);

			val code = m.getCode();
			if (code != null) System.out.println(code);
			System.out.println("<<<<<<<<<<<<<<<<<<<<<< Methods");
		})
	}

	@throws(classOf[ClassNotFoundException])
	def hierarchy(clazz: JavaClass) {
		System.out.println("superclasses ==================");
		clazz.getSuperClasses().foreach(superClass => {
			System.out.println("Name: " + superClass.getClassName());
			System.out.println("Interface: " + superClass.isInterface());
		})
		System.out.println("interfaces ==================");

		clazz.getInterfaces.foreach( interf => {
			System.out.println("Name: " + interf.getClassName());
			System.out.println("Interface: " + interf.isInterface());
		})

		System.out.println("all interfaces ==================");
		clazz.getAllInterfaces.foreach(interf => {
			System.out.println("Name: " + interf.getClassName());
			System.out.println("Interface: " + interf.isInterface());
		})
		System.out.println("==================");
	}

	@throws(classOf[ClassNotFoundException])
	def fields(clazz : JavaClass) = {
		System.out.println("Fields >>>>>>>>>>>>>>>>>>>>>>>> ");
		clazz.getFields.foreach(field => { 
			val t = field.getType();
			if (t.isInstanceOf[ObjectType]) {
				val ot = t.asInstanceOf[ObjectType]
				System.out.println(ot.getSignature());
				System.out.println(ot.getClassName);
			}
		})
		System.out.println("<<<<<<<<<<<<<<<<<<<<<< Fields");
	}
}