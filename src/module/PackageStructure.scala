/**
 * PackageStructure.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */
package module;

class PackageNode(value: String) extends ModuleNode(value) {
    override def toString = value /*+ (if (modules1.length > 0) ":" + modules1.length else "") */ + "(" + types.length + ")"
    override def toDot: String =   toString
    override def packageName = value
}

class PackageContainerNode(qn: String, ms: List[PackageNode])
    extends PackageNode(qn) with CompositeModuleNode[PackageNode] {
    modules = getModules(ms)
    override def toDot = {
        def commonprefix(s: String, s1: String, idx: Int): Int = {
            if (s.length == 0 || s1.length == 0 || s.head != s1.head ) return idx
            commonprefix(s.tail, s1.tail, idx + 1)
        }
        
        def suffixes = modules.remove(_.value == this.value).sort((m1, m2) => m1.toString.compareTo(m2.toString) < 0).map(m => {
            if (m.toString.startsWith(value)) m.toString.substring(value.length  + 1) 
            else "-" + m.toString.substring(commonprefix(m.toString, value, 0))
            
        })

        formattedStr(this, suffixes)
    }
}



class PackageStructure(graph: TypeStructure) extends ModuleStructure(graph) {
    def addNode(mvalue: String) = { //mvalue - package name
        if (!nodes.contains(mvalue))
            nodes = Map(mvalue -> new PackageNode(mvalue)) ++ nodes
        nodes(mvalue).asInstanceOf[ModuleNode]
    }

    def removePackages(pkgs: List[String]) = {
        var ms = List[ModuleNode]()
        for (ps <- pkgs)
            ms = nodes(ps).asInstanceOf[ModuleNode] :: ms

        removeNodes(ms)
        ms.flatMap(_.types)
    }

}

object PackageStructure {
    def constructPackageStructure(tdg: TypeStructure): PackageStructure = {
        val pdg = new PackageStructure(tdg)
        tdg.nodes.values.foreach(node =>
            pdg.addTypeNode(pdg.addNode(node.asInstanceOf[TypeNode].pkgname), node.value))
        pdg.liftEdges.asInstanceOf[PackageStructure]
    }

    def clonePackageStructure(pdg: PackageStructure): PackageStructure = {
        var newpdg = new PackageStructure(pdg.tdg)
        pdg.nodes.values.foreach(p => {
            val newpn = newpdg.addNode(p.value)
            p.asInstanceOf[PackageNode].types.foreach(tn => newpdg.addTypeNode(newpn, tn.value))
        })
        newpdg.liftEdges
        newpdg
    }

}
