package module

/**
 * ModuleCompactor.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

abstract class ModuleCompactor[S <: ModuleStructure, T <: ModuleNode] {

    protected def getDirectUpperNode(n: T): T
    protected def getDirectLowerNode(n: T): T
    private def subsumeIncomingEdges(n1: T, n2: T) = {
        n2.inedges.map(_.getSource).remove(_ == n1).forall(node => n1.inedges.map(_.getSource).contains(node))
    }

    /**
     * Constraint for edges of LP node n : outedges(n).forall(e => e.getTarget.level <= n.level)
     * Iceberg pattern condition: n is hidden by iceberg pattern <=>
     * 0. n has direct upper node of same package, upn
     * 1-1. n has no incoming edges except from direct upper node
     * 1-2. n has incoming edges, (subsumed by upn) n.inedges.map(_.getSource).forAll(node => upn.inedges.map(_.getSource).contains(node))
     * -. n's outedges do not affect hiding algorithm
     **/
    private def iceberg(n: T): Boolean = {
        val lun = getDirectUpperNode(n)
        if (lun == n) false
        else {
            if (n.inedges.map(_.getSource).remove(_ == lun).length == 0) true
            else
                subsumeIncomingEdges(lun, n)
        }
    }

    /** Fall-through pattern condition: n is hidden by fall-through pattern<=>
     * 0. n has greatest lower node of same package, gln	
     * 1. outedges(n).forall(e => e.getTarget.level <= gln.level) -- n's target nodes will become gln.target nodes
     * 2-1. n has no incoming edges
     * 2-2. n has incoming edges, (subsumed by gln) n.inedges.map(_.getSource).forAll(node => unn.inedges.map(_.getSource).contains(node)
     **/
    protected def satisfyLayerConstraint(n: T, gln: T): Boolean
    private def fallthrough(n: T): Boolean = {
        val gln = getDirectLowerNode(n)
        if (gln == n) false
        else {
            if (!satisfyLayerConstraint(n, gln)) false
            else {
                if (n.inedges == 0) true
                else subsumeIncomingEdges(gln, n)
            }
        }
    }

    protected def retains(key: String): Boolean
    private def compact(ms: S): Map[T, T] = {
        val ibpairs: Map[T, T] = ms.nodes.values.foldLeft(Map[T, T]())((result, n) => {
            if (retains(n.value)) result
            else {
                val ln = n.asInstanceOf[T]
                if (iceberg(ln)) result + (ln -> getDirectUpperNode(ln))
                else result
            }
        })

        def adjustPairs(pairs: Map[T, T]) = {
            pairs.map(pair => {
                var nTo: T = pair._2
                while (pairs.contains(nTo)) nTo = pairs(nTo)
                (pair._1, nTo)
            })
        }
        // ib constraint: pair is increasing (1, 2) & (2, 3) => (1, 2, 3), but (1, 4) & (2, 3) impossible
        // (1, 2) & (2, 3) & (3, 4) => (1, 3) & (2, 4) & (3, 4)
        val newibpairs = adjustPairs(ibpairs)

        val ftpairs: Map[T, T] = ms.nodes.values.foldLeft(Map[T, T]())((result, n) => {
            if (retains(n.value)) result
            else {
                val ln = n.asInstanceOf[T]
                if (fallthrough(ln)) result + (ln -> getDirectLowerNode(ln))
                else result
            }
        })
        // ft constraint: pair is decreasing (3, 2) & (2, 1) => (1, 2, 3), but (4, 1) & (3, 2) impossible ... = 
        val newftpairs = adjustPairs(ftpairs).foldLeft(Map[T, T]())((result, pair) => {
            // newibparts' keys should not be included in its values
            // condition:
            assert(newibpairs.values.toList.forall(n => !newibpairs.contains(n)))
            if (!newibpairs.contains(pair._2) && !newibpairs.contains(pair._1)) result + pair //&& !newibpairs.contains(pair._1)
            else
                result
        })

        print("iceburg compaction: ")
        println(newibpairs.mkString(", "))
        print("fallthrough compaction: ")
        println(newftpairs.mkString(", "))
        newftpairs.foldLeft(newibpairs)((result, pair) => {
            assert(!newibpairs.contains(pair._1))
            result + pair
        })
    }

    protected def createNode(n: T): T
    protected def createStructure(tdg: TypeStructure): S
    private def merge(mergeList: Map[T, T], ms: S): S = {
        val newlps = createStructure(ms.tdg) //new S(ms.tdg)
        ms.nodes.values.foreach(n => {
            val ln = n.asInstanceOf[T]
            if (mergeList.contains(ln)) {
                val lnTo = mergeList(ln)
                val mergeTo = newlps.nodes.getOrElse(lnTo.value, newlps.addNode(createNode(lnTo)))
                //newlps.addNode(new LayerModuleContainerNode(lnTo.value, lnTo.rank, lnTo.pkgname, List(lnTo))));
                mergeTo.asInstanceOf[CompositeModuleNode[T]].addModule(ln)
            } else {
                if (!newlps.nodes.contains(ln.value))
                    newlps.addNode(createNode(ln)) // new LayerModuleContainerNode(ln.value, ln.rank, ln.pkgname, List(ln)))
            }
        })
        newlps.nodes.values.foreach(n =>
            newlps.addTypeNodes(n.asInstanceOf[T], n.asInstanceOf[CompositeModuleNode[T]].getTypeNodes));

        newlps.liftEdges
        
        assert(ms.mapT2M.size == newlps.mapT2M.size)
        newlps
    }

    protected def reflectNewStructure(newstructure: S): Unit
    def run(ms: S): S = {
        reflectNewStructure(ms)

        var cms = ms // compact module structure
        var done = false
        while (!done) {
            val cms1 = merge(compact(cms), cms)
            if (cms1.nodes.size == cms.nodes.size) done = true
            else {
                cms = cms1
                reflectNewStructure(cms1)
            }
        }
        cms
    }
}

object ModuleCompactor {
    def retainPackages = config.Project.get.retainPackages
}
