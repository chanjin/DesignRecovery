
package rulecheck

import module._

/**
 * LayeredModuleInvestigator.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

class LayeredModuleInvestigator(lps: LayerModuleStructure) {
	val prefix = config.Project.prefix
	
	private def getModulesOfPackage(pkgname: String) : List[LayerModuleNode]= {
		lps.nodes.values.filter(m => (m.asInstanceOf[LayerModuleNode].pkgname == pkgname)).toList.
				map(_.asInstanceOf[LayerModuleNode]).sort((m1, m2) => m1.asInstanceOf[LayerModuleNode].rank > m2.asInstanceOf[LayerModuleNode].rank)
	}
	
	
	def howCoupled(pkg: String, pkgs: List[String], pdg: PackageStructure) = {
		// pkgs should contain pkg
		val tnodes = pdg.nodes.values.filter(p => pkgs.contains(p.value)).flatMap(_.asInstanceOf[ModuleNode].types)
		val internaledges = pdg.edges.filter(e => (pkgs.contains(e.getSource.value) && pkgs.contains(e.getTarget.value))).
											map(_.asInstanceOf[ModuleEdge])
		val tedges = internaledges.flatMap(_.getTypeEdges)
		
		val lms = getModulesOfPackage(pkg) //layer modules of pkg
		val lmsedges = lps.edges.filter(e => (lms.contains(e.getSource) && lms.contains(e.getTarget))).
										flatMap(_.asInstanceOf[ModuleEdge].getTypeEdges)
		
		// subgraph <= module node
		val pms = pdg.nodes.values.filter(p => (pkgs.contains(p.value) && p.value  != pkg )).map(_.asInstanceOf[ModuleNode]).toList
		Graph2Dot.toDotTypeGrouped(lms ::: pms, tedges ::: lmsedges, 
				 pkg.replace(".", "_") + "_layered")
		
		howSubtypeLayerPackages(pkg, pkgs)
	}
	
	private def hasDependencyWith(lm: LayerModuleNode, pkg: String) : Boolean= {
		lm.outedges.map(_.getTarget).exists(t => t.asInstanceOf[LayerModuleNode].pkgname == pkg) ||
									lm.inedges.map(_.getSource).exists(s => s.asInstanceOf[LayerModuleNode].pkgname == pkg)
	}
	
	def howSubtypeLayerPackages(pkg: String, pkgs: List[String]) = {
		val pkgsall = if ( pkgs.contains(pkg)) pkgs.remove(_ == pkg) else pkgs
		val lmspkg = getModulesOfPackage(pkg)
		val lmsall = lmspkg ::: pkgsall.foldLeft(List[LayerModuleNode]())( (lms, p) =>  getModulesOfPackage(p) ::: lms).
			filter(lm => hasDependencyWith(lm, pkg))
		
		val tnodes = lmsall.flatMap(_.types)
		val edges = lps.tdg.edges.filter(e => (e.isSubtypeEdge && tnodes.contains(e.getSource) && tnodes.contains(e.getTarget)))
		
		Graph2Dot.toDotTypeGrouped(lmsall, edges, Graph2Dot.dotNamePackage(pkg)+ "_subtype")
	}
	
	
	def howLayered(pkgname: String) = {
		val ms = getModulesOfPackage(pkgname)
		ms.foreach({ m=>
			println(m.types.sort((c1, c2) => c1.value.compareTo(c2.value) < 0).mkString(", "))
		})
	}
	
}

object LayeredModuleInvestigator {
	
}