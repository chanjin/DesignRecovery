/**
 * Main.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

package experiments
import java.io._
import config._
import bcel._


object Main {
	
	def readInput: String = {
		var input: String = null
		val br = new BufferedReader(new InputStreamReader(System.in))
        try {
        	print("> ")
        	input = br.readLine()
        } catch {
            case e: IOException => { System.out.println("IO error, redefine in-file"); input = "-1" }
        }
        input
	}
	
	def basicAnalysis: (String, ProjectConfiguration, Boolean) = {
		println("--------------")
		println("input in-file or -1 if you redefine the file")
		val input = readInput
		var  infile = new String
		var projconf = 
			if ( input != "-1" )  defineProjectInfo(input)
			else {
				val basicInfo = readProjectInfo
				Project.setLoadedProject(basicInfo)
				infile = "./" + basicInfo.name + ".in"
				util.FileOut.saveFile(infile, basicInfo.toFile)
				new ProjectConfiguration(basicInfo)
			}
		
		println("--------------")
		println(projconf.basicInfo)
		println("Start analysis .... Package Compaction, Layered Package Structure, LPS Compaction"  )
		PackageAnalysis.basicAnalysis(projconf)
		
		println("Check dot files in " + projconf.basicInfo.outpath)
		if ( input != -1 ) (input, projconf, true)
		else (infile, projconf, false)
	}
	
	def main(args : Array[String]): Unit = {
		var (infile, projconf, existViolationAnalysisInfo) = basicAnalysis
		var reanal = new String
		do {
			println("Do you need to reanalysis? If you want, edit generated in-file & type y -   y or n")
			reanal = readInput
			if ( reanal == "y" ) {
				projconf = defineProjectInfo(infile)
			}
		} while ( reanal != "n")
			
		// violation analysis
		do {
			if (existViolationAnalysisInfo) {
				PackageAnalysis.violationAnalysis(projconf)
			}
			println("Set up violation analysis? edit generated in-file, " + infile + "\n y or n")
			reanal = readInput
			if ( reanal == "y" ) {
				projconf = defineProjectInfo(infile)
				PackageAnalysis.violationAnalysis(projconf)
			}
		} while ( reanal != "n")
	}
	
	def defineProjectInfo(filename: String): ProjectConfiguration = {
		// check file exist
		val conf: ProjectConfiguration = Project.parse(filename) //bcel.in
		conf
	}
	
	def readProjectInfo : ProjectBasicInfo = {
		println("project file name: ")
		val projname = readInput
		println("input path: ")
		val inputpath = readInput
		println("additional jars (comma separated): ")
		val jars = readInput.split(",").toList
		println("output path: ")
		val outputpath = readInput
		println("prefix (e.g. org.apache.bcel: ")
		val prefix = readInput
		println("classes to exclude (req exps comma separated e.g. java[.].*, junit[.].*): ")
		val excludeTypes = readInput.split(",")
		println("packages to retain ( package name without prefix e.g. generic, util ) : ")
		val retainPackages = readInput.split(",").toList
		
		// in-file generation
		
		/*println("types considered as globally known ( space separated, ground rule type e.g. accounts.Account view.KeyEvent ): ")
		val globalTypes = */
		var basicInfo =new ProjectBasicInfo(projname, inputpath, outputpath, prefix, true)
		basicInfo.additionalJars = jars
		basicInfo.retainPackages = retainPackages
		ClassAnalysisRule.setExcludeTypes(excludeTypes)
		
		basicInfo
	}
}