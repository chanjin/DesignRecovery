/**
 * PackageSubtypeAnalysis.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

package experiments

import module._


class PackageSubtypeAnalysis (lps : LayerModuleStructure) {
	val lpstruct = lps

	class CommonSubtypeDep (t: ModuleNode, es: List[DepEdge]) {
		val to = t
		var subtypes = List[(TypeNode, List[TypeNode])]()
		val from = es.map(_.getSource).removeDuplicates
		var heteroPackages = false
		analyze
		
		private def analyze = {
		  val tes = es.flatMap(_.asInstanceOf[ModuleEdge].getTypeEdges.filter(_.isSubtypeEdge ))
		  var stmap = scala.collection.mutable.Map[TypeNode, List[TypeNode]]()
		  for ( te <- tes) {
			  if ( !stmap.contains(te.getTarget.asInstanceOf[TypeNode]) ) stmap.put(te.getTarget.asInstanceOf[TypeNode], te.getSource.asInstanceOf[TypeNode] :: Nil )
			  else stmap(te.getTarget.asInstanceOf[TypeNode]) = te.getSource.asInstanceOf[TypeNode] :: stmap(te.getTarget.asInstanceOf[TypeNode])
		  }
		  for ( st <- stmap.keys) {
			  if (stmap(st).length > 1 ) {
				  val modules = stmap(st).map(t => lpstruct.mapT2M(t).asInstanceOf[LayerModuleNode].pkgname)
				  if ( modules.remove(_ == to.asInstanceOf[LayerModuleNode].pkgname).removeDuplicates.length > 0 ) {
					  heteroPackages = true
					  subtypes = (st, stmap(st)) :: subtypes
				  }
			  }
		  }
		}
		
		override def toString = t + "<-- " +  from.map(m=> m.asInstanceOf[LayerModuleNode].pkgname).removeDuplicates + " : " + from+  " \n" + 
			subtypes.map(p => "**" +  p._1 + ", " + p._2.length + ": " + p._2).mkString("\n")
	}
 
	def getInterPackageSubtypings : List[ModuleEdge]= {
		val stedges = lpstruct.edges.filter(edge => edge.isSubtypeEdge )
		
		for ( n <- lpstruct.nodes.values ) {
			val stedges2n = stedges.filter(_.getTarget == n)
			println("------------------")
			if ( stedges2n.map(_.getSource).removeDuplicates.length > 1 ) {
				val subtypeInfo = new CommonSubtypeDep(n.asInstanceOf[ModuleNode], stedges2n)
				if ( subtypeInfo.heteroPackages )	println(subtypeInfo)
			}
		}
		Nil
	}
 
 	def getCommonSubtypingsPairs() = {
		var result: List[List[(TypeNode, List[TypeNode])]] = Nil
		for ( mn <- lpstruct.nodes.values.toList ) {
			val commonSubtypings = getCommonSubtypings(mn.asInstanceOf[ModuleNode])
			if ( commonSubtypings.length > 0 )
				result =  commonSubtypings :: result
		}
		println("STSTST----")
		result.foreach(println(_))
		result
	}
 
	def getCommonSubtypings(mn: ModuleNode) : List[(TypeNode, List[TypeNode])] = {
		val incstedges = mn.inedges.flatMap(_.asInstanceOf[ModuleEdge].getTypeEdges).filter(_.isSubtypeEdge)
		var p2childs = Map[TypeNode, List[TypeNode]]()
		
		for ( e <- incstedges ) {
			val source = e.getSource.asInstanceOf[TypeNode]
			val target = e.getTarget.asInstanceOf[TypeNode]                                    
		    if ( !p2childs.contains(target) ) p2childs += (target -> (source :: List[TypeNode]())) 
            else p2childs += (target -> (source :: p2childs(target)))
		}
        
		var p2clist = List[(TypeNode, List[TypeNode])]()
        
        for ( entry <- p2childs ) 
           if ( entry._2.length > 1 ) p2clist = (entry._1, entry._2) :: p2clist
        println(mn + ":" + incstedges.length + " - " + p2clist.map(_._2.length) + "-" + p2childs)
        p2clist
	}
	
	def run = {
		getInterPackageSubtypings
	}
}
