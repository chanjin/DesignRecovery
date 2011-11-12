
/**
 * Graph2File.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */
package module
import util._


object Graph2File {
    def toFileType(g: DependencyGraph, dotfilename: String): Unit = {
        toFileType(g, dotfilename, g.nodes.values.toList)
    }

    def toFileType(g: DependencyGraph, dotfilename: String, nodelist: List[NodeElem]): Unit = {
        val dotname = if (dotfilename.indexOf("/") != -1) dotfilename.substring(dotfilename.lastIndexOf("/") + 1) else dotfilename
        val nodes = g.nodes
        val edges = g.edges

        def edgefilter(e: DepEdge) = (nodelist.contains(e.getSource) && nodelist.contains(e.getTarget))
        val edgeStrs = g.edges.filter(edgefilter(_)).sort((e1, e2) => e1.getSource.value.compareTo(e2.getSource.value) < 0).
            map(e => e.getSource + "\t" + e.getTarget + "\t" + e.toDot)
        FileOut.saveFile(dotfilename + ".txt", edgeStrs.mkString("\n"))
    }

    def toFileModule(g: ModuleStructure, dotfilename: String): Unit = {
        toFileModule(g, config.Project.get.outpath + "/" + dotfilename, g.nodes.values.toList)
    }

    // toDot filtered by nodelist
    def toFileModule(g: ModuleStructure, dotfilename: String, nodelist: List[NodeElem]): Unit = {
        val dotname = if (dotfilename.indexOf("/") != -1) dotfilename.substring(dotfilename.lastIndexOf("/") + 1) else dotfilename
        val nodes = g.nodes
        val edges = g.edges
        val mns = nodes.values.toList.filter(nodelist.contains(_)).sort(_.value < _.value)
        val mnsDetails = mns.sort(_.value < _.value).map(n => {
            val mn = n.asInstanceOf[ModuleNode]
            val types = mn.types.filter(!_.value.contains("$")).sort(_.value < _.value).toList
            val typesInner = mn.types.filter(_.value.contains("$")).sort(_.value < _.value).toList

            n.value + "(" + (types.size + typesInner.size) + ")\n" +
                types.map(t => t.value.mkString(", ")) + "\n\t" +
                typesInner.map(t => t.value.substring(if (mn.packageName.length > 0) mn.packageName.length + 1 else 0)).mkString(", ")
        })

        val mnsStr = mns.map(n => {
            n.outedges.sort(_.getSource.value < _.getSource.value).
                map(e => (e.getSource.value + "\t" + e.getTarget.value)).remove(_.length == 0).mkString("\n")
        })

        def testr(te: DepEdge, srcprefix: String, dstprefix: String) = {
        	te.getSource.value + "\t" + te.getTarget.value + "\t" + te.toDot;
        }
        
        val mnsStrDetails = mns.map(n => {
            val srcprefix = n.asInstanceOf[ModuleNode].packageName
            n.outedges.sort(_.getSource.value < _.getSource.value).map(e => {
                val dstprefix = e.getTarget.asInstanceOf[ModuleNode].packageName
                
                "***" + e.getSource.value + "->" + e.getTarget.value + "\n" +
                    e.asInstanceOf[ModuleEdge].getTypeEdges.sort(_.getSource.value < _.getSource.value)
                    .map(te => testr(te, srcprefix, dstprefix)).mkString("\n")
            }).mkString("\n")
        })

        FileOut.saveFile(dotfilename + ".txt",
            mnsDetails.mkString("\n") + mnsStr.mkString("\n") + "\n-----------\n" + mnsStrDetails.mkString("\n------------\n"))
    }

}