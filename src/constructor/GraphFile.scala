/**
 * GraphFile.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

package constructor

import bcel._
import org.apache.bcel._
import org.apache.bcel.classfile._
import java.io.File
import java.io.{ FileOutputStream, PrintWriter }
import module._
import scala.xml.XML
import config._


object GraphFile {

    private var _filename: String = null

    def store(inputpath: String, writeFtn: PrintWriter => Unit) = {
        // tdg and pdg
        _filename = inputpath + "/TypeStructure.lci"
        var fos: FileOutputStream = null
        var writer: PrintWriter = null
        try {
            fos = new FileOutputStream(_filename)
            writer = new PrintWriter(fos, true)
            writeFtn(writer)
        } finally {
        	println(_filename)
            writer.close()
            fos.close()
        }
    }

    def load(inputpath: String): (TypeStructure, PackageStructure) = {
        import scala.io.Source
        val filename = inputpath + "/TypeStructure.lci"
        val src = Source.fromFile(new File(filename))
        var tdg = new TypeStructure

        object mode extends Enumeration {
            val Initial, Project, Node, Edge = Value
        }
        var modeinload = mode.Initial
        var id2type = Map[Int, String]()
        src.getLines.foreach(line => {
            line match {
                case "nodes" => modeinload = mode.Node
                case "edges" => modeinload = mode.Edge
                case _ => {
                    modeinload match {
                        case mode.Node => { //8:pattern.CompositeFigure
                            val id = Integer.parseInt(line.substring(0, line.indexOf(":")))
                            val typename = line.substring(line.indexOf(":") + 1)

                            if (!ClassAnalysisRule.isExcluded(typename)) {
	                            tdg.addNode(typename)
	                            id2type += (id -> typename)
                            }

                            //println( id + ", " + typename)
                            //tdg.addNode(line.substring())
                        }
                        case mode.Edge => { //2->4:S, I
                            val from = Integer.parseInt(line.substring(0, line.indexOf("->")))
                            val to = Integer.parseInt(line.substring(line.indexOf("->") + 2, line.indexOf(":")))
                            if ( id2type.contains(from) && id2type.contains(to) ) {
	                            val attr = line.substring(line.indexOf(":") + 1)
	                            import java.util.StringTokenizer
	                            val st = new StringTokenizer(attr, ",")
	                            import TypeDepKind._
	                            var kinds = List[TypeDepKind]()
	                            while (st.hasMoreTokens) {
	                                val dep = st.nextToken
	                                kinds = TypeDepKind.kindFromStr(dep) :: kinds
	                            }
	
	                            //println(from + "->" + to + ": " + attr)
	                            val newedge = if (kinds.contains(TypeDepKind.OverridingDep))
	                                tdg.addSubtypeEdge(id2type(from), id2type(to), Nil)
	                            else
	                                tdg.addTypeEdge(id2type(from), id2type(to), kinds.head)
	                            newedge.addKinds(kinds)
                            }
                        }
                        case _ => println("Legacy")
                    }
                }
            }
        })

        
        
        (tdg, PackageStructure.constructPackageStructure(tdg))
    }

    def getEdgeAttr(e: DepEdge): String = e.asInstanceOf[TypeEdge].getDepCode

    def storeTypeGraph(tdg: TypeStructure)(w: PrintWriter) = {
        val nodes = tdg.nodes
        val edges = tdg.edges

        val (node2id, count) =
            nodes.foldLeft((scala.collection.mutable.Map[String, Int](), 0))((m, n) => ((m._1 + (n._1 -> m._2)), m._2 + 1))
        w.println("nodes")
        node2id.foreach(n2i => w.println(n2i._2 + ":" + n2i._1))
        w.println("edges")

        edges.foreach(e => w.println(node2id(e.getSource.toString) + "->" + node2id(e.getTarget.toString) + ":" + getEdgeAttr(e)))
    }
}