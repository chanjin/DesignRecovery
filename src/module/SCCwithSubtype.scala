package module
import config._

// report sccs including subtype dependency
/**
 * SCCwithSubtype.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

object SCCwithSubtype {
	import TypeDepKind._
	
	def run(tdg: TypeStructure, edgeKind: TypeDepKind = TypeDepKind.ByteCodeDep) = {
		val sccsWithSubtype = TypeStructure.getNewTypeStrcuture(tdg, edgeKind).detectSCC.filter(sccnodes =>
			tdg.filterEdges(sccnodes).exists(edge => edge.isSubtypeEdge) )
		var count = 0
		sccsWithSubtype.foreach( scc => { 
			Graph2Dot.toDotType(tdg, "SCCWithSubtypes_" +edgeKind +"_"+ count +"_" + scc.length, scc)
			count += 1
		} )
	}
}