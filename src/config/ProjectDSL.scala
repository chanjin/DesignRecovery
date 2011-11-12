/**
 * ProjectDSL.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

package config
import scala.util.parsing.combinator._


class ProjectDSL extends JavaTokenParsers {
    def parse(desc: String) = {
        println(parseAll(projectDef, desc))
    }

    var configuration: ProjectConfiguration = null
    var prefix: String = ""
    def fullname(s: String) = if (s == ".") prefix else prefix + "." + s

    def projectDef: Parser[Any] = rep(basicInfoDef | violationRuleDef | globalTypeRuleDef | ignoreEdgeRuleDef | separationActionDef) ^^ {

        case defs => {
            if (prefix.length > 0) {
                configuration.violations = configuration.violations.map(v => (fullname(v._1)  -> v._2.map(fullname(_) )))
                configuration.gtypes = configuration.gtypes.map(prefix + "." + _)
                configuration.ignores = configuration.ignores.map(i => (( fullname(i._1._1), i._1._2), (fullname(i._2._1), i._2._2)))
                configuration.seps = configuration.seps.map(s => ( fullname(s._1) -> s._2))
            }
            defs
        }
    }

    def basicInfoDef: Parser[ProjectBasicInfo] = "project" ~ "{" ~ rep(keyValDef | keyListDef) ~ "}" ^^ {
        case "project" ~ "{" ~ entries ~ "}" => {
            var kvmap = entries.filter(_._2.isInstanceOf[String]).foldLeft(Map[String, String]())((m, e) =>
                m + (e._1 -> e._2.asInstanceOf[String]))
            var klvmap = entries.filter(e => e._2.isInstanceOf[List[String]]).foldLeft(Map[String, List[String]]())((m, e) =>
                m + (e._1 -> e._2.asInstanceOf[List[String]]));

            prefix = if (kvmap("prefix").length > 0) kvmap("prefix") else ""

            val reanalysis = if (kvmap.contains("reanalysis")) kvmap("reanalysis") == "true" else false
            if (!kvmap.contains("reanalysis"))
                println("If you don't want to reanalyze byte codes, write \"reanalysis = \"true\"\" in Project input file")

            var basicInfo = new ProjectBasicInfo(kvmap("name"), kvmap("rootpath"), kvmap("outputpath"), kvmap("prefix"), reanalysis)

            if (kvmap.contains("removeFewerEdges")) basicInfo.removeFewerEdges = (kvmap("removeFewerEdges") == "true")

            basicInfo.additionalJars = if (klvmap.contains("additionalJars")) klvmap("additionalJars") else List[String]()
            basicInfo.retainPackages = if (klvmap.contains("retainPackages")) klvmap("retainPackages").map(fullname(_)) else List[String]()
            if ( klvmap.contains("excludeTypes") ) bcel.ClassAnalysisRule.setExcludeTypes(klvmap("excludeTypes").toArray[String])
            
            configuration = new ProjectConfiguration(basicInfo)
            basicInfo
        }
    }

    def violationRuleDef: Parser[Map[String, List[String]]] = "rule" ~ "violation" ~ "{" ~ rep(keyListMapDef) ~ "}" ^^ {
        case "rule" ~ "violation" ~ "{" ~ entries ~ "}" => {
            
            var violations = scala.collection.mutable.Map[String, List[String]]()
            for (e: (String, List[String]) <- entries ) {
            	if ( violations.contains(e._1)) violations(e._1) = violations(e._1) ::: e._2
            	else violations += (e._1 -> e._2)
            }
            
            configuration.violations = violations.toMap
            configuration.violations
        }
    }

    def globalTypeRuleDef: Parser[List[String]] = "rule" ~ "globalTypes" ~ "{" ~ rep(stringLiteral) ~ "}" ^^ {
        case "rule" ~ "globalTypes" ~ "{" ~ types ~ "}" => {
            configuration.gtypes = types.map(t => t.substring(1, t.length - 1));
            configuration.gtypes
        }
    }

    def ignoreEdgeRuleDef: Parser[List[((String, List[Int]), (String, List[Int]))]] = "rule" ~ "ignore" ~ "{" ~ rep(mod2modDef) ~ "}" ^^ {
        case "rule" ~ "ignore" ~ "{" ~ entries ~ "}" => {
            configuration.ignores = entries
            entries
        }
    }

    def mod2modDef: Parser[((String, List[Int]), (String, List[Int]))] = stringLiteral ~ ":" ~ repsep(wholeNumber, ",") ~ "->" ~ stringLiteral ~ ":" ~ repsep(wholeNumber, ",") ^^ {
        case pkg1 ~ ":" ~ layers1 ~ "->" ~ pkg2 ~ ":" ~ layers2 => {
            val pl1: (String, List[Int]) = (pkg1.substring(1, pkg1.length - 1), layers1.map(_.toInt))
            val pl2: (String, List[Int]) = (pkg2.substring(1, pkg2.length - 1), layers2.map(_.toInt))
            (pl1, pl2)
        }
    }

    def separationActionDef: Parser[Map[String, List[List[Int]]]] = "action" ~ "separation" ~ "{" ~ rep(actionDef) ~ "}" ^^ {
        case "action" ~ "separation" ~ "{" ~ actions ~ "}" => {
            configuration.seps = actions.foldLeft(Map[String, List[List[Int]]]())((m, a) => m + (a._1 -> a._2))
            configuration.seps
        }
    }

    def actionDef: Parser[(String, List[List[Int]])] = stringLiteral ~ "->" ~ "(" ~ repsep(intListDef, ",") ~ ")" ^^ {
        case pkg ~ "->" ~ "(" ~ layers ~ ")" => (pkg.substring(1, pkg.length - 1), layers)
    }

    def intListDef: Parser[List[Int]] = "(" ~ repsep(wholeNumber, ",") ~ ")" ^^
        { case "(" ~ numbers ~ ")" => numbers.map(_.toInt) }

    def keyValDef: Parser[(String, String)] = ident ~ "=" ~ stringLiteral ^^ {
        case name ~ "=" ~ value => { (name, value.substring(1, value.length - 1)) }
    }
    def keyListDef: Parser[(String, List[String])] = ident ~ "=" ~ "(" ~ repsep(stringLiteral, ",") ~ ")" ^^ {
        case name ~ "=" ~ "(" ~ values ~ ")" => (name, values.map(v => v.substring(1, v.length - 1)))
    }
    def keyListMapDef: Parser[(String, List[String])] = stringLiteral ~ "->" ~ "(" ~ repsep(stringLiteral, ",") ~ ")" ^^ {
        case name ~ "->" ~ "(" ~ values ~ ")" => (name.substring(1, name.length - 1), values.map(v => v.substring(1, v.length - 1)))
    }
}

