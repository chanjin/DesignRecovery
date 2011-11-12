/**
 * PackageHierarchy.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */
package module

/**
 * HierarchyNode - Node in package ownership hierarchy
 * @author chanjinpark
 *
 */
class HierarchyNode(pn: PackageNode) {
    def pkgnode = pn
    var parent: HierarchyNode = null
    var leaves = List[HierarchyNode]()
    override def toString = {
    	( if ( pkgnode == null) "-" else pkgnode.value ) 
    	/*+ "\n" +
    	"parent - " + parent + "\n" +
    	"leaves - " + leaves.mkString(",")*/
    }
    def toValue = {
    	( if ( pkgnode == null) "-" else pkgnode.value ) + "\n" +
    	"parent - " + parent + "\n" +
    	"leaves - " + leaves.mkString(",")
    }
}

object HierarchyNode {
    def getParentStr(p: String) = if (p.indexOf('.') != -1) p.substring(0, p.lastIndexOf(".")) else "-"
    	
    def ancestors(r: HierarchyNode): List[HierarchyNode] =
        if (r.parent == null) List[HierarchyNode]()
        else r.parent :: ancestors(r.parent)

    def descendants(p: HierarchyNode): List[HierarchyNode] =
        if (p.leaves.length == 0) p.leaves
        else p.leaves ::: p.leaves.flatMap(descendants(_))

    def makeHierarchy(pdg: PackageStructure, treeroot: HierarchyNode): Map[String, HierarchyNode] = {
        var pkg2hn = pdg.nodes.values.foldLeft(Map[String, HierarchyNode]())((result, pn) =>
            result + (pn.value -> new HierarchyNode(pn.asInstanceOf[PackageNode])))
        pkg2hn += ("-" -> treeroot)

        def getParent(pkgname: String): HierarchyNode = {
            def parent(p: String): HierarchyNode = {
                val index = p.lastIndexOf(".")
                if (index == -1) treeroot
                else {
                    val p1 = p.substring(0, p.lastIndexOf("."))
                    if (pkg2hn.contains(p1)) pkg2hn(p1)
                    else parent(p1)
                }
            }
            if ( pkgname == "-" ) null else parent(pkgname)
        }

        pkg2hn.foreach(p2hn => {
            val parent = getParent(p2hn._1)
            if ( parent != null ) {
            	p2hn._2.parent = parent
            	parent.leaves = p2hn._2 :: parent.leaves
            }
        })
        
        //println(pkg2hn.mkString("\n"))
        
        pkg2hn
    }
}



class PackageHierarchy(pdg: PackageStructure) {
    val treeroot = new HierarchyNode(null) // imaginary root node
    val pkg2hn = HierarchyNode.makeHierarchy(pdg, treeroot)
    def isRoot(p: String) = (p == "-")
    
    //println(pkg2hn.mkString("\n"))

    def toDotPackageHierarchy(dotname: String) = {
        val hierarchy = new DependencyGraph() {
            def addEdge(from: String, to: String): DepEdge = {
                addEdge(new DepEdge(nodes(from), nodes(to)))
            }
        }
        pkg2hn.foreach(s2np => hierarchy.addNode(new NodeElem(s2np._1)))
        
        def addEdgeR(hn: HierarchyNode): Unit = {
        	hn.leaves.foreach(leaf => { 
        		hierarchy.addEdge( (if (hn == treeroot) "-" else hn.pkgnode.value), leaf.pkgnode.value); addEdgeR(leaf) 
        	}) 
        }
        addEdgeR(treeroot)

        val project = config.Project.get
        Graph2Dot.toDotType(hierarchy, dotname)
    }
}

object PackageHierarchy {
    def getHiearchy(pdg: PackageStructure) = {
    	(new PackageHierarchy(pdg)) 
    }
}