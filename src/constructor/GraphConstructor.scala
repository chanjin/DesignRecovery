/**
 * GraphConstructor.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

package constructor
import module._
import config._
import org.apache.bcel.util.Repository


object GraphConstructor {

    private var _ignored = List("java[.].*", "javax[.].*", "sun[.].*", "sunw[.].*",
        "com[.]sun[.].*", "org[.]omg[.].*", "org[.]w3c[.].*", "org[.]xml[.].*", "net[.]jini[.].*")
    def getIgnored = _ignored
    def setIgnored(v: Array[String]) = _ignored = v.toList

    def constructTypeStructure(path: String): (TypeStructure, PackageStructure) = {
        var tdg = new TypeStructure

        import org.apache.bcel.classfile._
        import java.io.File
        import bcel._

        def isFromJar(path: String): Boolean = path.endsWith(".jar")
        val analyzer = new TypeAnalyzer
        val types: List[JavaClass] =
            (if (!isFromJar(path)) {
                val ts = analyzer.getJavaClasses(new File(path)).toList.map(_.asInstanceOf[JavaClass])
                val tsjars = if (Project.get.additionalJars.length > 0) {
                    val jars: Array[String] = Project.get.additionalJars.toArray
                    analyzer.getJavaClassesFromJar(jars).toList.map(_.asInstanceOf[JavaClass])
                } else List[JavaClass]()
                ts ::: tsjars
            } else {
                if (Project.get.additionalJars.length > 0) {
                    val jars: Array[String] = (path :: Project.get.additionalJars).toArray
                    println(jars(0) + ", " + jars(1))
                    analyzer.getJavaClassesFromJar(jars).toList.map(_.asInstanceOf[JavaClass])
                } else
                    analyzer.getJavaClassesFromJar(path).toList.map(_.asInstanceOf[JavaClass])
            }).filter(t => !ClassAnalysisRule.isExcluded(t.getClassName))

        val repository = analyzer.getRepository

        for (clazz <- types)
            tdg.addNode(clazz.getClassName)

        import bcel._
        val nodes2analyze = types.map(_.getClassName)

        ClassInformation.setRepo(repository)

        import TypeDepKind._
        for (clazz <- types) {
            val structuraldeps = ClassInformation.getStructuralDependents(clazz).filter(t => nodes2analyze.contains(t)).removeDuplicates

            val parentClass: JavaClass = {
                val parent = ClassInformation.getSuperClass(clazz)
                if (parent != null && nodes2analyze.contains(parent)) parent else null
            }
            /*if ( parent != null && nodes2analyze.contains(parent.getClassName) ) {
				val e = tdg.addTypeEdge(clazz.getClassName, parent.getClassName, TypeDepKind.DirectInhDep)
			}*/
            val parentInterfaces = ClassInformation.getInterfaces(clazz).toList.filter(cls => tdg.getNode(cls.getClassName) != null)
            //parentInterfaces.foreach(itf => 
            //	tdg.addTypeEdge(clazz.getClassName, itf.getClassName, TypeDepKind.DirectInhDep))

            val parents = (if (parentClass != null) parentClass.getClassName) :: parentInterfaces.map(_.getClassName)

            val cpdeps = ClassInformation.getConstPoolClasses(clazz).filter(t => nodes2analyze.contains(t)).removeDuplicates
            val dependents = (structuraldeps ::: cpdeps).removeDuplicates
            val ancestors = ClassInformation.getAncestors(clazz).filter(ancestor => nodes2analyze.contains(ancestor.getClassName)).removeDuplicates
            val overridings = ancestors.map(a => (a.getClassName, ClassInformation.getOverridingMethods(clazz, a))).filter(ov => ov._2.length > 0)
            val overridingAncestors = overridings.map(_._1)

            //println(clazz.getClassName)

            assert(structuraldeps.filter(dep => !dependents.contains(dep)).length == 0)
            assert(parentClass == null || (dependents.contains(parentClass) && structuraldeps.contains(parentClass)))

            //println(parentInterfaces.filter(dep => !structuraldeps.contains(dep)).map(_.getClassName))
            assert(parentInterfaces.forall(dep => structuraldeps.contains(dep.getClassName)))

            dependents.foreach(dep => {
                val edge = tdg.addTypeEdge(clazz.getClassName, dep, TypeDepKind.ByteCodeDep).asInstanceOf[TypeEdge]
                if (structuraldeps.contains(dep)) edge.addKind(TypeDepKind.StructuralDep)
                if (parents.contains(dep)) edge.addKind(TypeDepKind.DirectInhDep)
            })
            //(structuraldeps ::: dependents).removeDuplicates.filter(t => nodes2analyze.contains(t) && !overridingAncestors.contains(t)).foreach(dep => 
            //	tdg.addTypeEdge(clazz.getClassName, dep,  TypeDepKind.SimpleDep) )

            overridings.foreach(ov => tdg.addSubtypeEdge(clazz.getClassName, ov._1, ov._2))
        }

        println(tdg.edges.size)
        GraphFile.store(config.Project.get.getInputPath, GraphFile.storeTypeGraph(tdg))
        (tdg, PackageStructure.constructPackageStructure(tdg))
    }

    def toDot(node2id: scala.collection.mutable.Map[String, Int], edges: List[DepEdge], name: String): String = {
        val dotname = config.Project.get.getOutputPath + "/" + name
        val nodeshape = "node [shape=plaintext, fontname=Skia, fontsize=20];"
        val edgestyle = "edge [fontname=Skia, fontsize = 10, labelfontname=Skia];"
        def edgeSimple(e: DepEdge) = "[color=\"black\", arrowhead=\"vee\", label=\"" +
            (e.asInstanceOf[TypeEdge].getDepCode) + "\"]"
        def edgeSubtype(e: DepEdge) = "[color=\"red\",  arrowhead=\"odot\", style=\"dotted\", label = \"" +
            (e.asInstanceOf[SubtypeEdge].getMethods.mkString(",")) + "\"]"

        def edgeString(e: DepEdge): String = (node2id(e.getSource.toString) + "->" + node2id(e.getTarget.toString)) +
            (if (e.isSubtypeEdge) edgeSubtype(e) else edgeSimple(e))

        def edgeStr(edges: List[DepEdge]) = edges.foldLeft(Nil: List[String])((r, e) => edgeString(e) :: r)

        ("digraph " + dotname + " { \n" + nodeshape + "\n" + edgestyle + "\n"
            + (node2id.foldLeft(Nil: List[String])((s, n2id) => ((n2id._2 + " [label = \"" + n2id._1 + "\"") :: s))).mkString(";\n") + ";\n"
            + edgeStr(edges).mkString(";\n") + "\n}")
    }
}