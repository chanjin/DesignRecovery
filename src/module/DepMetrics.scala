/**
 * DepMetrics.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

package module
import util._
import config._

class BasicMetrics(dg: DependencyGraph) {
    val nodeCount = dg.nodes.size
    val edgeCount = dg.edges.length
    val edgeDensity = Util.percentage(edgeCount.asInstanceOf[Long], (nodeCount * (nodeCount - 1)).asInstanceOf[Long])

    val sccs = dg.detectSCC
    val sccAllNodes = sccs.flatMap(scc => scc)
    val maxSccNodes = sccs.foldLeft(List[NodeElem]())((max, scc) => if (scc.length > max.length) scc else max)
    val edgeDensitySccs = sccs.map(scc => {
        Util.percentage(dg.getEdges(scc).length.asInstanceOf[Long], (scc.length * (scc.length - 1))) + "(" + dg.getEdges(scc).length + ", " + scc.length + ")"
        //+ "\n" + dg.getEdges(scc)
    })

    def basicMetrics = List(
        ("# Nodes", nodeCount.toString),
        ("# Edges", edgeCount.toString),
        ("Edge density", edgeDensity.toString),
        ("# SCC", sccs.length.toString),
        ("Nodes in SCCs", sccAllNodes.length.toString),
        ("#Nodes in SCC", sccs.map(_.length).mkString(", ")),
        ("Max SCC nodes", maxSccNodes.length.toString),
        ("Edge Density in SCC", edgeDensitySccs.mkString(","))).map(str => str._1 + " - " + str._2).mkString("\n")
}

class TypeDepMetrics(tdg: TypeStructure, bm: BasicMetrics) {
    val ts: TypeStructure = tdg
    def getTypeCycles = bm.sccs
    val subtypeCount = ts.edges.filter(_.isSubtypeEdge).length
    val subtypeRatio = Util.percentage(subtypeCount.asInstanceOf[Long], bm.edgeCount.asInstanceOf[Long])
    override def toString: String = {
        bm.basicMetrics + "\n" +
            List[(String, String)](
                ("# Subtype Edges", subtypeCount.toString),
                ("SubtypeEdge Ratio", subtypeRatio.toString)).map(str => str._1 + " - " + str._2).mkString("\n")
    }
}

class ModuleDepMetrics(ms: ModuleStructure, bm: BasicMetrics) {
	val msccs = ms.detectSCC

    def moduleScc: String = {
        msccs.map(scc => scc.length + "(" +
            scc.foldLeft(0)((count, m) => count + m.asInstanceOf[ModuleNode].types.length) + ")").mkString(", ")
    }

    def interfaces: String = {
        val interfaceTypesCount = ms.nodes.values.foldLeft((0, 0))((count, n) => {
        	val mn = n.asInstanceOf[ModuleNode]
        	print(mn.value + "- ")
        	//println( mn.interfaceTypes.length + " : " + mn.types.length)
            (count._1 + mn.interfaceTypes.length, count._2 + mn.types.length)
        })

        println("Interface % 2")
        Util.percentage(interfaceTypesCount._1, interfaceTypesCount._2) + "(" + interfaceTypesCount._1 + ", " + interfaceTypesCount._2 + ")" + " - " +
            ms.nodes.values.map(n => n.asInstanceOf[ModuleNode].interfaceTypes.length).mkString(",")
    }

    override def toString: String = {
        bm.basicMetrics + "\n" +
            List[(String, String)](
                ("# Interface Types in each module", interfaces),
                ("# Nodes in SCC", moduleScc)).map(str => str._1 + " - " + str._2).mkString("\n")
    }
}


object DepMetrics {
    def run = {
    	  import Project.StructureKind._
    	val bm = new BasicMetrics(Project.results(Type))
    	val tm = new TypeDepMetrics(Project.results(Type).asInstanceOf[TypeStructure], bm)
        println("-------------")
        println("*** Type dependency metrics")
        println(tm.toString)

        println("*** Package dependency metrics - original")
        val opm = new ModuleDepMetrics(Project.results(PackageNoSimplification).asInstanceOf[PackageStructure], bm)
        println(opm)

        println("*** Package dependency metrics")
        val pm = new ModuleDepMetrics(Project.results(Package).asInstanceOf[PackageStructure], bm)
        println(pm)

        println("*** Layered Package Module dependency metrics")
        val lpm = new ModuleDepMetrics(Project.results(Layer).asInstanceOf[LayerModuleStructure], bm)
        println(lpm)

        println("*** Compacted Layered Package Module  dependency metrics")
        val clpm = new ModuleDepMetrics(Project.results(CompactLayer).asInstanceOf[LayerModuleStructure], bm)
        println(clpm)

        if ( Project.results.contains(PackageRestructured)) {
	        println("*** New Package  dependency metrics")
	        val npm = new ModuleDepMetrics(Project.results(PackageRestructured).asInstanceOf[PackageStructure], bm)
	        println(npm)
        }
        println("-------------")
    }
}

