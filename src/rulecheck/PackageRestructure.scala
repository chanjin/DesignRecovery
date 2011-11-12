package rulecheck

import module._
import config._

/**
 * PackageRestructure.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

object PackageRestructure {
	
    def makeNewPackageStructure(projconf: ProjectConfiguration, pdg: PackageStructure, lps: LayerModuleStructure): PackageStructure = {

        def newPackageKey(pkg: String, layers: List[Int]) = pkg + ":" + layers.mkString(",")
        val filtermap = projconf.ignores //  List[((String, List[Int]), (String, List[Int]))]()
        
        def isIgnoreModuleEdges(e: DepEdge): Boolean = {
            val src = e.getSource.asInstanceOf[LayerModuleNode]
            val sc = filtermap.filter(k => (k._1._1 == src.pkgname) && (k._1._2.contains(src.rank) || k._1._2.contains(-1)))
            if (sc.isEmpty) return false

            val dst = e.getTarget.asInstanceOf[LayerModuleNode]
            val dc = filtermap.filter(v => (v._2._1 == dst.pkgname) && (v._2._2.contains(dst.rank) || v._2._2.contains(-1)))
            if (dc.isEmpty) return false

            println("FILTERED: " + e)
            true
        }

        val newtdg = if (projconf.ignores.size > 0) {
            val refEdges = lps.edges.filter(e => isIgnoreModuleEdges(e)).flatMap(_.asInstanceOf[ModuleEdge].getTypeEdges)
            println("FILTERED edges: " + refEdges.size + "\n" + refEdges.mkString("\n"))
            println("FILTERED nodes: " + refEdges.map(_.getTarget).removeDuplicates.size + "\n" +
                refEdges.map(_.getTarget).removeDuplicates.mkString("\n"))

            println("FILTERED nodes (source): " + refEdges.map(_.getSource).removeDuplicates.size + "\n" +
                refEdges.map(_.getSource).removeDuplicates.mkString("\n"))
            TypeStructure.makeStructureWithoutEdges(lps.tdg, e => refEdges.contains(e))
        } else lps.tdg

        def getTypeNodes(lnode: ModuleNode): List[TypeNode] = {
            lnode.types.map(n => newtdg.getNode(n.value).asInstanceOf[TypeNode])
        }

        val restructurePkgs = projconf.seps 
        val newpdg = new PackageStructure(newtdg)
        pdg.nodes.values.foreach(n => {
            val pn = n.asInstanceOf[ModuleNode]
            if (restructurePkgs.contains(pn.packageName)) {
                val layers = restructurePkgs(pn.packageName) // ((0, 1, 2), (3, 4, 5, 6)) List[List[Int]]
                layers.foreach(ln => {
                    var lnodes = ln.map(number => lps.nodes(lps.moduleKey(pn.packageName, number)))
                    val newpn = newpdg.addNode(newPackageKey(pn.packageName, ln))
                    newpdg.addTypeNodes(newpn, lnodes.flatMap(lnode => getTypeNodes(lnode.asInstanceOf[ModuleNode])))
                })
            } else {
                val newpn = newpdg.addNode(pn.packageName)
                val lnodes = lps.nodes.values.filter(_.asInstanceOf[ModuleNode].packageName == pn.packageName).toList
                newpdg.addTypeNodes(newpn, lnodes.flatMap(lnode => getTypeNodes(lnode.asInstanceOf[ModuleNode])))
            }
        })
        newpdg.liftEdges
        val scc = newpdg.detectSCC
        println("SCC in new PDS : " + scc.size + "\n" + scc.mkString("\n"))
        newpdg
    }
}