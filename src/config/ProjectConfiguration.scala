
/**
 * ProjectConfiguration.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */


package config
import scala.io._
import java.io._


class ProjectConfiguration(bi: ProjectBasicInfo) {
    def basicInfo = bi
    var violations = Map[String, List[String]]()
    var gtypes = List[String]()
    var ignores = List[((String, List[Int]), (String, List[Int]))]()
    var seps = Map[String, List[List[Int]]]()
}

class ProjectBasicInfo(n: String, root: String, out: String, pre: String, reanal: Boolean) {
    def name = n
    def rootpath = root
    def outpath = out
    def prefix = pre
    def reanalyze = reanal
    var additionalJars = List[String]()
    var retainPackages = List[String]()
    var removeFewerEdges = false
    var excludeTypes = List[String]()
    val isJar = rootpath.toLowerCase.endsWith(".jar")
    

    def requireReanalyze = {
        !(new File(outpath+ "/TypeStructure.lci")).exists || reanalyze
    }
    def getInputPath = {
        if (isJar) rootpath.substring(0, rootpath.lastIndexOf("/"))
        else rootpath
    }
    def getOutputPath = outpath
    
    override def toString = {
    	name + "\n" +
    	"Input path: " + rootpath + "\n" +
    	"Output path: " + outpath + "\n" +
    	"Prefix: " + prefix + "\n" + 
    	"Classes to exclude: " + excludeTypes.mkString(", ") + "\n" +
    	"Packages to retain: " + retainPackages.mkString(", " )
    }
    
    def toFile = {
    	"project {\n" + 
    	"\tname = \"" + name + "\"\n" +
    	"\trootpath = \"" + rootpath + "\"\n" +
    	"\toutputpath = \"" + outpath + "\"\n" + 
    	"\tprefix = \"" + prefix + "\"\n" +
    	"\treanalysis = \"" + (if (reanalyze) "true" else "false") + "\"\n" +
    	"\tadditionalJars = (" + additionalJars.map(jar => if ( jar.length > 0) "\"" + jar + "\"" else "").mkString(", ") + ")\n" + 
    	"\tremoveFewerEdges = \""+ (if (removeFewerEdges) "true" else "false") + "\"\n" +
    	"\texcludeTypes = (" + excludeTypes.map(t => if ( t.length > 0 ) "\"" + t + "\"" else "").mkString(", ") + ")\n" +
    	"}"
    }
}

object Project {
/*    def main(args: Array[String]): Unit = {
        import experiments._
        val selproject = CommandLineMenus.selectProject
        PackageAnalysis.run(new StudyConfiguration(parse(selproject)))
    }*/

    private var proj: ProjectBasicInfo = null

    def parse(filename: String): ProjectConfiguration = {
        val parser = new ProjectDSL
        parser.parse(Source.fromFile(new File(filename)).mkString)
        proj = parser.configuration.basicInfo
        parser.configuration
    }

    def setLoadedProject(p: ProjectBasicInfo) = proj = p

    def get: ProjectBasicInfo = proj
    def prefix = proj.prefix

    object StructureKind extends Enumeration {
        type StructureKind = Value
        val Type, Package, PackageNoSimplification, Layer, CompactLayer, PackageRestructured = Value
    }

    import StructureKind._
    import module._
    var results = Map[StructureKind, DependencyGraph]() //Map[StructureKind, module.DependencyGraph]()
   
    def getStructure: (TypeStructure, PackageStructure, PackageStructure, LayerModuleStructure) = {
        (results(Type).asInstanceOf[TypeStructure],
            results(PackageNoSimplification).asInstanceOf[PackageStructure],
            results(Package).asInstanceOf[PackageStructure],
            Project.results(Layer).asInstanceOf[LayerModuleStructure])
    }
}
