/**
 * CommandLineMenus.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

package experiments
import config._



object CommandLineMenus {
    def main(args: Array[String]): Unit = {
        val selproject = CommandLineMenus.selectProject
        val projconf = Project.parse(selproject)
        PackageAnalysis.basicAnalysis(projconf)
        PackageAnalysis.violationAnalysis(projconf)
        while (true) cmdOption
    }

    private def select(options: List[String]): Int = {
        import java.io._
        var selected = -1
        var count = 0
        options.foreach(o => { println(count + ": " + o); count = count + 1 })
        val br = new BufferedReader(new InputStreamReader(System.in))
        try {
            var done = false
            do {
                print("> ")
                selected = br.readLine().toInt
                if (!(selected >= 0 && selected < options.length)) {
                    if (selected == -1) System.exit(1)
                    println("retry: valule should be greater than 0 and less than " + options.length + "\n enter -1 to exit")
                } else {
                    done = true
                }
            } while (!done)
        } catch {
            case e: IOException => { System.out.println("IO error trying to read your choice!"); System.exit(1); }
        }
        selected
    }

    def selectProject: String = {
        val input = List("android", "argouml", "awt", "bcel", "findbugs", "hadoop", "hibernate", "jfreechart", "jhotdraw6",
            "junit3", "junit4", "layering", "lucene", "pattern", "spring", "struts2", "swing", "wala", "traceview")
        println("Select project number")

        "Project/" + input(select(input)) + ".in"
    }

    def cmdOption = {
        import Project.StructureKind._
        var selstruct: StructureKind = Project.results.keys.toList(select(Project.results.keys.map(_.toString).toList))
        import module._
        val pkglist = Project.results(PackageNoSimplification).nodes.values.map(_.value).toList
        selstruct match {
            case Type => {
                val ts = Project.results(Type).asInstanceOf[TypeStructure];
                val pkgname = pkglist(select(pkglist))
                Graph2Dot.toDotType(ts, "CMDTS" + pkgname.replace(".", "_"), pkgname)
            }
            case _ => {
                println("unimplemented yet")
            }
        }
    }
}