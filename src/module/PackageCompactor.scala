package module

/**
 * PackageCompactor.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

class PackageCompactor extends ModuleCompactor[PackageStructure, PackageNode] {
	
	var hierarchy: PackageHierarchy = null
    def reflectNewStructure(newstructure: PackageStructure): Unit = {
        hierarchy = PackageHierarchy.getHiearchy(newstructure)
    }

    // least upper bound, leastUpperNode
    def getDirectUpperNode(n: PackageNode): PackageNode = {
    	val hn = hierarchy.pkg2hn(n.value)
    	if ( hn.parent == hierarchy.treeroot ) n else hn.parent.pkgnode 
    }
        
    //greatestLowerNode
    // only in the case that node has only one child
    def getDirectLowerNode(n: PackageNode): PackageNode = {
    	val hn = hierarchy.pkg2hn(n.value)
    	if ( hn.leaves.length != 1 ) n else hn.leaves.head.pkgnode 
    }
        

    def satisfyLayerConstraint(n: PackageNode, gln: PackageNode): Boolean = true
        //n.outedges.forall(e => e.getTarget.asInstanceOf[PackageNode].rank <= gln.rank)

    def createNode(n: PackageNode): PackageNode = new PackageContainerNode(n.value,  List(n))
    def createStructure(tdg: TypeStructure): PackageStructure = new PackageStructure(tdg)
    def retains(key: String): Boolean = {
    	//println(key + ": " + config.Project.get.retainPackages)
    	config.Project.get.retainPackages.contains(key)
    }
}

object PackageCompactor {
    def compact(ms: PackageStructure): PackageStructure = {
        (new PackageCompactor).run(ms)
    }
}