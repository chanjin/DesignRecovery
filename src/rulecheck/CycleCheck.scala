package rulecheck
import config._
import module._



/**
 * CycleCheck.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

class CycleCheck {
    import Project.StructureKind._
    val lps = Project.results(Layer).asInstanceOf[LayerModuleStructure]
    val pdg = Project.results(Package).asInstanceOf[PackageStructure]
    val prefix = Project.prefix

    def getLayerNodesInCycle(packages: List[String]): List[NodeElem] = {
        lps.edges.filter(e => {
            packages.contains(e.getSource.asInstanceOf[ModuleNode].pkgname) &&
                packages.contains(e.getTarget.asInstanceOf[ModuleNode].pkgname)
        }).foldLeft(List[NodeElem]())((ns, e) => e.getSource :: e.getTarget :: ns)
    }

    def getLayerNodesInCycle(pkg: String, pkg2: String): List[NodeElem] = {
        lps.edges.filter(e => {
            val src = e.getSource.asInstanceOf[ModuleNode].pkgname
            val dst = e.getTarget.asInstanceOf[ModuleNode].pkgname
            (src == pkg && dst == pkg2) || (src == pkg2 && dst == pkg)
        }).foldLeft(List[NodeElem]())((ns, e) => e.getSource :: e.getTarget :: ns)
    }

    /**
     * generates dot files showing a set of package SCCs
     * each dot file corresponds a SCC 
     * @param dotprefix
     * @return
     */
    def checkPackageCycles(dotprefix: String) = {
        pdg.detectSCC.foreach(ms => {
            val pkgs = ms.map(mn =>
                Graph2Dot.dotNamePackage(mn.value)).sort((m1, m2) => m1.toString.compareTo(m2.toString) < 0)
            val dotname1 = pkgs.mkString("_")
            val dotname2 = pkgs.map(s => if (s.length > 3) s.substring(0, 3) else s).mkString("_")

            val dotname = dotprefix + "SCC_" + (if (dotname1.length > 32) dotname2 else dotname1)
            Graph2Dot.toDot(lps, dotname, getLayerNodesInCycle(ms.map(_.value))) // subgraph of lps
            Graph2Dot.toDot(pdg, dotname + "_pdg", ms)
        })
    }

    /**
     * generate dot files in which violations in cycled edges are shown 
     * violation에 해당하는 type structure를 생성
     * @param violations
     * @return
     */

    def checkPackageDependencyViolaitons(violations: Map[String, List[String]]): List[DepEdge] = {
        val cycles: List[(NodeElem, List[NodeElem])] = pdg.directCycles
        //val lmi = new LayeredModuleInvestigator(lps)
        cycles.foreach(cycle => {
            //println("***" + cycle)
            val pkg = cycle._1.value
            //val packages = (cycle._1 :: cycle._2).map(_.value)
            val dotname = Graph2Dot.dotNamePackage(pkg)
            cycle._2.foreach(p => {
                val pkg2 = p.value
                Graph2Dot.toDot(lps, dotname + "_2_" + Graph2Dot.dotNamePackage(pkg2),
                    getLayerNodesInCycle(pkg, pkg2))
            })

            Graph2Dot.toDot(pdg, dotname + "_pdg", cycle._1 :: cycle._2)
            //lmi.howLayered(pkg)
            //lmi.howCoupled(pkg, pkg :: cycle._2.map(_.value), pdg)
        })

        var vedgesall = List[DepEdge]()
        cycles.foreach(cycle => {
            val pkg = cycle._1.value
            if (violations.contains(pkg)) {
                violations(pkg).foreach(pkg2 => {
                    // remove violation type edges
                    val depedges = lps.edges.filter(e =>
                        (e.getSource.asInstanceOf[ModuleNode].pkgname == pkg && e.getTarget.asInstanceOf[ModuleNode].pkgname == pkg2)).
                        flatMap(e => e.asInstanceOf[ModuleEdge].getTypeEdges)

                    vedgesall = depedges ::: vedgesall
                    val tns = depedges.foldLeft(List[NodeElem]())((ns, e) => e.getSource :: e.getTarget :: ns)
                    val dotname = Graph2Dot.dotNamePackage(pkg) + "_2_" + Graph2Dot.dotNamePackage(pkg2) + "_tdg"
                    Graph2Dot.toDotType(lps.tdg, dotname, tns)
                })
            }
        })

        println("Violating edges: " + vedgesall.removeDuplicates.length)
        val vnodes = vedgesall.map(_.getTarget).removeDuplicates
        println("Violating nodes (targets): " + vnodes.length + "\n\t" + vnodes.mkString("\n\t"))
        vedgesall
    }
}