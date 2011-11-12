package module

/**
 * LayerModuleCompactor.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

class LayerModuleCompactor extends ModuleCompactor[LayerModuleStructure, LayerModuleNode] {
	var p2ns: String => List[LayerModuleNode] = null
	def reflectNewStructure(newstructure: LayerModuleStructure) = {
    	p2ns = packageToNodes(newstructure)
    }
	
    // least upper bound, leastUpperNode
	// There's no least upper node, return itself 
    def getDirectUpperNode(n: LayerModuleNode): LayerModuleNode =
        p2ns(n.packageName).foldLeft(p2ns(n.pkgname).last)((upn, node) => if (node.rank > n.rank && node.rank < upn.rank) node else upn);

    //greatestLowerNode
    def getDirectLowerNode(n: LayerModuleNode): LayerModuleNode =
        p2ns(n.packageName).foldLeft(p2ns(n.pkgname).head)((lon, node) => if (node.rank < n.rank && node.rank > lon.rank) node else lon);

    def satisfyLayerConstraint(n: LayerModuleNode, gln: LayerModuleNode): Boolean =
        n.outedges.forall(e => e.getTarget.asInstanceOf[LayerModuleNode].rank <= gln.rank)

    def createNode(n: LayerModuleNode): LayerModuleNode = new LayerModuleContainerNode(n.value, n.rank, n.pkgname, List(n))
    def createStructure(tdg: TypeStructure): LayerModuleStructure = new LayerModuleStructure(tdg)
    def retains(key: String): Boolean  = false

    private def packageToNodes(ms: LayerModuleStructure) = ms.nodes.values.foldLeft(scala.collection.mutable.Map[String, List[LayerModuleNode]]())((m, n) => {
            val packageName = n.asInstanceOf[LayerModuleNode].packageName
            if (m.contains(packageName)) {
                m(packageName) = n.asInstanceOf[LayerModuleNode] :: m(packageName)
                m
            } else m + (packageName -> List(n.asInstanceOf[LayerModuleNode]))
        }).map(kv => (kv._1 -> kv._2.sort((n1, n2) => n1.rank < n2.rank)))

}

object LayerModuleCompactor {
    def compact(ms: LayerModuleStructure): LayerModuleStructure = {
        (new LayerModuleCompactor).run(ms)
    }
}