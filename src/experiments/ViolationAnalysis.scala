/**
 * ViolationAnalysis.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

package experiments

import module._


object ViolationAnalysis {

	private def layeringPackages(tdg: module.TypeStructure, pdg: module.PackageStructure)  = {
		//val lpdg = LayerModuleStructure.constructLayerModuleStructure(tdg, pdg)
		//val clpdg = LayerModuleCompactor.compact(lpdg)
		

		
		println("********* 4 Focusing on violation modules")
		/*val violationModules = getModuleNodes(constructor.getFocusingModulePrefixes, clpdg)
		if ( violationModules.length > 0 )
			clpdg.toDot("CLPfocused", violationModules, PkgNodeColor.layerPackageNodeColor)*/
	
			// TODO: class diagrams related with violation edges, get types violations, draw tdg subgraph using dot's subgraph representation
			/*
			 val pkgtypegraph = new TypeStructurePackaged
			 val violationEdges = getModuleEdges(constructor.getViolations, pdg)
	
			 val vtedges = violationEdges.flatMap(_.asInstanceOf[ModuleEdge].getTypeEdges)
			 val vtnodes = vtedges.foldLeft(List[TypeNode]()) ((list, e) => e.getSource.asInstanceOf[TypeNode] :: e.getTarget.asInstanceOf[TypeNode] :: list)
			 pkgtypegraph.constructStructure(tdg, vtnodes, vtedges, clpdg)
			 pkgtypegraph.toDot("TSViolation", layerPackageNodeColor)
			 */
	
			println("********* 5 Focusing violation edges")
			/* 
			val violationEdges = getModuleEdges(constructor.getViolations, pdg)
			 if ( violationEdges.length > 0 ) {
				 pdg.removeEdges(violationEdges)
				 pdg.toDot("Package1", packageNodeColor)
				  val lpdg1 = new  LayeredPackageStructure(tdg)
				 lpdg1.layeringPackages(pdg)
				 lpdg1.liftEdges
				 val clp1 =  lpdg1.mergeIceburgs.mergeFallthrough
				 clp1.toDot("CLP1", layerPackageNodeColor)
				 println("CLP1CLP1" + clp1)
			 }
			 */
			// TODO : table view ....
	
	}
	
	def main(args : Array[String]) : Unit = {
		val dgs = constructor.GraphFile.load("")
		layeringPackages(dgs._1, dgs._2)
	}
}
