package module

/**
 * LayerModuleStructure.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

class LayerModuleNode(qn: String, r: Int, pkg: String) extends ModuleNode(qn) {
    pkgname = pkg
    val rank = r
    override def toDot = toString
}

class LayerModuleContainerNode(qn: String, r: Int, pkg: String, ms: List[LayerModuleNode])
    extends LayerModuleNode(qn, r, pkg) with CompositeModuleNode[LayerModuleNode] {
	
	modules = getModules(ms)

    override def toDot = {
        def suffixes = modules.remove(_.value == this.value).sort(_.rank < _.rank).map(m =>
            (Graph2Dot.namePackage(m.toString)))
        formattedStr(this, suffixes)
    }
	
}




class LayerModuleStructure(tdg: TypeStructure) extends ModuleStructure(tdg) {
    def moduleKey(p: String, r: Int) = p + ":" + r
    def getNode(pkg: String, r: Int): LayerModuleNode = {
        val key = moduleKey(pkg, r)
        if (!nodes.contains(key)) nodes += key -> new LayerModuleNode(key, r, pkg)
        nodes(key).asInstanceOf[LayerModuleNode];
    }
    
     def printLayerDetails = {
        println("----------------")
        nodes.values.toList.sort((n1, n2) => n1.toString.compareTo(n2.toString) < 0).foreach(n => {
            print("** " + n.toString + ": ")
            println(n.asInstanceOf[LayerModuleNode].types.sort((t1, t2) => t1.value.compareTo(t2.value) < 0).
                map(t => t.value.substring(t.value.lastIndexOf(".") + 1)).mkString(", "))
        })
        println("----------------")
        /*print interface classes of lpdg, the meaning of each lp module is determined by interface classes ignoring internal classes
		inter package dependency only */
        val m2interfaceTypes = getInterfaceClasses
        m2interfaceTypes.foreach(m2i => {
            val prefix = m2i._1.packageName
            println(m2i._1.value + "'s interface classes (" + m2i._2.length + ") are\n" +
                m2i._2.map(t => t.value.substring(if (t.value.length > prefix.length) prefix.length + 1 else 0)).mkString(","))
        })
        println("----------------")
    }
}
