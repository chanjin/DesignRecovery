/**
 * PackageAnalysis.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

package experiments
import config._
import module._
import java.io.File
import constructor._
import rulecheck._


object PackageAnalysis {

    def toDotNewStructures(tdg: TypeStructure, name: String, checker: CycleCheck): PackageStructure = {
        val pdg = PackageStructure.constructPackageStructure(tdg)
        val spdg = PackageCompactor.compact(pdg)
        Graph2Dot.toDot(pdg, name + "PDG")
        Graph2Dot.toDot(spdg, name + "SPDG")

        val lps = LayeringAlgorithm.constructLayerModuleStructure(pdg)
        Graph2Dot.toDot(lps, name + "LP")

        val clps = LayerModuleCompactor.compact(lps)
        Graph2Dot.toDot(clps, name + "CLP")

        checker.checkPackageCycles(name)
        spdg
    }
    
    import Project.StructureKind._
    def basicAnalysis(projconf: ProjectConfiguration) = {
    	val project = projconf.basicInfo

        util.FileOut.removeFiles(project.getOutputPath, "dot")

        Graph2Dot.prefix = project.prefix
        var dgs: (TypeStructure, PackageStructure) = null
        if (project.requireReanalyze) {
            println("generate layer class information")
            dgs = GraphConstructor.constructTypeStructure(project.rootpath)
        } else {
            dgs = GraphFile.load(project.getOutputPath)
        }
        

        val tdg = dgs._1
        val pdg = dgs._2

        Project.results += (Type -> tdg)
        Project.results += (PackageNoSimplification -> pdg)
        println("PDG has following cycles")
        println("------")
        pdg.detectSCC.foreach(lstcycle => println(lstcycle.mkString(",")))
        println("------")
        Graph2Dot.toDot(pdg, "BasicPDG")
        PackageHierarchy.getHiearchy(pdg).toDotPackageHierarchy("BasicPHG")
        SCCwithSubtype.run(tdg)
        Graph2Dot.toDotType(TypeStructure.getNewTypeStrcuture(tdg, TypeDepKind.OverridingDep), "BasicSubtypeGraph")

        val spdg = PackageCompactor.compact(pdg)
        Project.results += (Package -> spdg)
        Graph2Dot.toDot(spdg, "BasicSPDG")
        PackageHierarchy.getHiearchy(spdg).toDotPackageHierarchy("BasicSPHG")

        println("Layering start")
        val lps = LayeringAlgorithm.constructLayerModuleStructure(spdg)
        //lps.printLayerDetails

        Project.results += (Layer -> lps)

        Graph2Dot.toDot(lps, "BasicLMS" + (if (lps.nodes.size > 40) "_" + lps.nodes.size else ""))
        Graph2File.toFileModule(lps, "BasicLMS")

        println("Compact layer structure")
        val clps = LayerModuleCompactor.compact(lps)
        Graph2Dot.toDot(clps, "BasicCLPS")
        Project.results += (CompactLayer -> clps)

        assert(spdg.tdg.edges.size == lps.tdg.edges.size)
        assert(spdg.tdg.edges.size == clps.tdg.edges.size)
    }

    def violationAnalysis(projconf: ProjectConfiguration) = {
        println("Rule check start")
        val lps = Project.results(Layer).asInstanceOf[LayerModuleStructure]
        val spdg = Project.results(Package).asInstanceOf[PackageStructure]
        
        var tdgRule = lps.tdg
        val checker = new CycleCheck
        checker.checkPackageCycles("PackageCycle_")

        val dredges = checker.checkPackageDependencyViolaitons(projconf.violations) // design rule
        val drclasses = projconf.gtypes
        
        if (dredges.length > 0) {
            println("Rule violations")
            val newtdg = TypeStructure.makeStructureWithoutEdges(tdgRule, e => dredges.contains(e))
            println("=== " + tdgRule.edges.length + " => " + newtdg.edges.length + " : " + (tdgRule.edges.length - newtdg.edges.length))
            if (drclasses.length == 0) {
                val newpdg = toDotNewStructures(newtdg, "LMSEdgesIgnore_", checker)
                Project.results += (PackageRestructured -> newpdg)
            }
            tdgRule = newtdg
        }

        if (drclasses.length > 0) {
            println("Rule ground rule classes")
            val newtdg = TypeStructure.makeStructureWithoutEdges(tdgRule, e => drclasses.contains(e.getTarget.value))
            println("=== " + tdgRule.edges.length + " => " + newtdg.edges.length + " : " + (tdgRule.edges.length - newtdg.edges.length))
            val newpdg = toDotNewStructures(newtdg, "GlobalTypes_", checker)
            Project.results += (PackageRestructured -> newpdg)
            tdgRule = newtdg
        }
        
        val restdtdg = TypeStructure.makeStructureWithoutEdges(lps.tdg, e => dredges.contains(e) || drclasses.contains(e))
        val restpdg = toDotNewStructures(restdtdg, "Restructuring_", checker)
        
        Graph2Dot.toDot(rulecheck.PackageRestructure.makeNewPackageStructure(projconf, restpdg, lps), "ResturctureFinalPDG")

        DepMetrics.run
    }
    
    def analyze(projconf: ProjectConfiguration) = {
    	basicAnalysis(projconf)
    	violationAnalysis(projconf)
    }

    def main(args: Array[String]): Unit = {
        val conf: ProjectConfiguration = Project.parse("Project/bcel.in") //bcel.in	android.in layering.in 	junit4.in	hadoop.in 	hibernate.in	lucene.in
        basicAnalysis(conf)
        violationAnalysis(conf)
    }
}