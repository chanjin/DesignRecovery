package module

/**
 * LayeringAlgorithm.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

object LayeringAlgorithm {
    class LNode(n: List[NodeElem]) {
        var rank = 0
        def nodes = n
        var inedges: List[DepEdge] = nodes.flatMap(n => n.inedges)

        override def toString = n.mkString("-") + "(" + rank + ")"
    }

    private def layeringTypes(tdg: TypeStructure): Map[NodeElem, Int] = {
        val lscc = tdg.detectSCC // List[List[NodeElem]]
        // if level of a class is modified & the class is in SCC, all of the classes in SCC are modified also. level propagation 
        var n2ln = lscc.foldLeft(Map[NodeElem, LNode]())((m, scc) => {
            val lnodeScc = new LNode(scc)
            scc.foldLeft(m)((m1, n) => m1 + (n -> lnodeScc))
        })

        // for other nodes than scc
        n2ln = tdg.nodes.values.foldLeft(n2ln)((m, n) => {
            if (!n2ln.contains(n)) m + (n -> new LNode(List(n)))
            else m
        })

        var worklist = n2ln.values.toList // List[LNode]
        while (!worklist.isEmpty) {
            var changed: List[LNode] = Nil
            for (work <- worklist) {
                for (edge <- work.inedges) {
                    var from = n2ln(edge.getSource)
                    if (from != work) {
                        if (edge.isSubtypeEdge) {
                            if (from.rank < (work.rank + 1)) {
                                from.rank = work.rank + 1
                                changed = from :: changed
                            }
                        } else {
                            if (from.rank < work.rank) {
                                from.rank = work.rank
                                changed = from :: changed
                            }
                        }
                    }
                }
            }
            worklist = changed.removeDuplicates
            //println(worklist.size)
        }

        n2ln.foldLeft(Map[NodeElem, Int]())((m, entry) => m + (entry._1 -> entry._2.rank))
    }

    /**
     * 
     * @param pdg
     * @return newly constructed LayerModuleStructure based on tdg, pdg
     */
    def constructLayerModuleStructure(pdg: PackageStructure): LayerModuleStructure = {
        val tdg = pdg.tdg

        val lps = new LayerModuleStructure(tdg)

        val n2ranks = layeringTypes(tdg)
        val lpnodes = n2ranks.foldLeft(scala.collection.mutable.Map[(String, Int), List[TypeNode]]())((m, n2r) => {
            val tn = n2r._1.asInstanceOf[TypeNode]
            val key = (pdg.mapT2M(tn).value, n2r._2)
            if (!m.contains(key)) m += (key -> List(tn))
            else m(key) = tn :: m(key)
            m
        })

        lpnodes.foreach(lpn => {
            val ((pkg, rank), tnodes) = lpn
            val lmn = lps.getNode(pkg, rank)
            lps.addTypeNodes(lmn, tnodes)
            tnodes.foreach(_.rank = rank)
        })

        assert(pdg.nodes.values.foldLeft(0)((c, n) => c + n.asInstanceOf[ModuleNode].types.length) == lps.nodes.values.foldLeft(0)((c, n) => c + n.asInstanceOf[ModuleNode].types.length))
        lps.liftEdges
        lps
    }

    /** nodes in SCC are considered as a separate layer module node in which 
     *   nodes of different packages can be included 
     * @param pdg
     */
    /*def layeringPackagesWithSccs(tdg: TypeStructure, pdg: PackageStructure) = {
        val lnlist = layeringTypes(tdg)
        val lps = new LayerModuleStructure(tdg)
        def getModule(ln: LNode): LayerModuleNode = {
            val newpkgname = ln.nodes.map(n => pdg.mapT2M(n.asInstanceOf[TypeNode]).value).removeDuplicates.mkString("-")
            lps.getNode(newpkgname, ln.rank)
        }

        lnlist.foreach(ln => {
            lps.addTypeNodes(getModule(ln), ln.nodes.map(_.asInstanceOf[TypeNode]))
        })
        lps.liftEdges
        lps
    }*/
}