/**
 * Graph2Dot.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

package module
import util._
import config._




object Graph2Dot {
	
	 def dotNamePackage(pkg: String) = {
		 val prefix = Project.prefix
        (if (pkg.startsWith(prefix) && pkg.length > prefix.length) pkg.substring(prefix.length + 1) else pkg).replace(".", "_")
     }
	 
	 def namePackage(pkg: String) = {
		 val prefix = Project.prefix
        (if (pkg.startsWith(prefix) && pkg.length > prefix.length) pkg.substring(prefix.length + 1) else pkg)
     }
	 
	 def dotNamePacakge(pkgs: List[String]) = {
		 
	 }
	
	var prefix: String = null
	val edgeSimple =  "[color=\"black\", arrowhead=\"vee\""
	val edgeSubtype = "[color=\"red\",  arrowhead=\"odot\", style=\"dotted\"" 

	val nodeshape = "node [shape=plaintext, fontname=Skia, fontsize=20];"
	val edgestyle = "edge [fontname=Skia, fontsize = 10];"
		
	
	def nodeStringType(n: NodeElem, node2id: String => Int) = 
		node2id(n.toString) + "[ label=\"" + namePackage(n.toDot) + "\"]"
	def nodeStringModule(n: NodeElem, node2id: String => Int) = 
		node2id(n.toString) + "[ label=\"" + n.toDot + "\"]"  //+ nodeColor(n)  + ", style = filled]"
	
	def edgeStringType(e: DepEdge, node2id: String => Int) : String = 
		(node2id(e.getSource.toString) + "->" + node2id(e.getTarget.toString)) + 
		( if (e.isSubtypeEdge  )  edgeSubtype  else edgeSimple ) + ", label = \"" + e.toDot + "\"]"  
	
	def edgeStringModule(e: DepEdge, node2id: String => Int, g: DependencyGraph) : String = (node2id(e.getSource.toString) + "->" + node2id(e.getTarget.toString)) + 
		       	( if (e.isSubtypeEdge  )  edgeSubtype else edgeSimple ) + 
		       	( if (e.isInstanceOf[ModuleEdge]) ( ", label = \"" + e.toDot + "\"]" ) else "]" )
	
	//def edgeSubtypeLabelType(e: DepEdge) =  edgeSubtype + ", label = \"" + e.toDot + "\"]" 
   
   	def toDotType(g: DependencyGraph, dotfilename: String): Unit = 
		toDotType(g, dotfilename, g.nodes.values.toList)
	def toDotType(g: DependencyGraph, dotfilename: String, pkg: String): Unit = {
		val nodes = g.nodes.values.filter(_.asInstanceOf[TypeNode].pkgname == pkg)
		toDotType(g, dotfilename, nodes.toList)
	}
	
	def toDotType(g: DependencyGraph, name: String, nodelist: List[NodeElem]): Unit = {
		//val dotname = if ( dotfilename.indexOf("/") != -1) dotfilename.substring(dotfilename.lastIndexOf("/") + 1) else dotfilename
		val dotname = config.Project.get.getOutputPath + "/" + name
		val nodes = g.nodes
		val edges = g.edges
  		// nodename -> id map
		val (node2id, count) = 
			nodelist.foldLeft((scala.collection.mutable.Map[String, Int](), 0)) ((m, node) => ((m._1 + (node.toString -> m._2)), m._2 + 1))
	    
		def edgefilter(e: DepEdge) = ( nodelist.contains(e.getSource) && nodelist.contains(e.getTarget) ) 
		val edgeStrs = g.edges.filter(edgefilter(_)).foldLeft(Nil: List[String]) ( (r, e) => edgeStringType(e, node2id) :: r )
	     FileOut.saveFile(dotname + ".dot", 
	                   "digraph " + name + " { \n" + nodeshape +  "\n" + edgestyle + "\n"
	                   + nodelist.foldLeft(Nil: List[String]) ((s, v) => (nodeStringType(v, node2id) :: s)).mkString(";\n") + ";\n"
	                   + edgeStrs.mkString(";\n") + "\n}")
   }
	

	
	def toDot(g: DependencyGraph, dotfilename: String): Unit = {
		toDot(g, dotfilename, g.nodes.values.toList)
    }

	// toDot filtered by nodelist
   def toDot(g: DependencyGraph, name: String, nodelist: List[NodeElem]): Unit = {
	   //val dotname = if ( dotfilename.indexOf("/") != -1) dotfilename.substring(dotfilename.lastIndexOf("/") + 1) else dotfilename
	   val dotname = config.Project.get.getOutputPath + "/" + name
	   	val nodes = g.nodes
		val edges = g.edges
		 val mns = nodes.values.toList.filter(nodelist.contains(_))
	     var (node2id, count) = 
	    	 mns.foldLeft((scala.collection.mutable.Map[String, Int](), 0)) ((m, node) => ((m._1 + (node.toString -> m._2)), m._2 + 1))

		 def edgefilter(e: DepEdge) = ( nodelist.contains(e.getSource) && nodelist.contains(e.getTarget) ) 
	   
		 val edgeStrs = g.edges.filter(edgefilter(_)).foldLeft(Nil: List[String]) ( (r, e) => edgeStringModule(e, node2id, g) :: r )

	     FileOut.saveFile(dotname + ".dot", 
	                   "digraph " + name + " { \n node [shape=plaintext, fontname=Skia, fontsize=16] \n" + edgestyle + "\n" 
	                   + mns.foldLeft(Nil: List[String]) ((s, v) => (nodeStringModule(v, node2id) :: s)).mkString(";\n") + ";\n"
	                   + edgeStrs.mkString(";\n") + "\n}")
   }
   
   
	def toDotTypeGrouped(mns: List[ModuleNode], edges: List[DepEdge], name: String): Unit = {
		//val dotname = if ( dotfilename.indexOf("/") != -1) dotfilename.substring(dotfilename.lastIndexOf("/") + 1) else dotfilename
		val dotname = config.Project.get.getOutputPath + "/" + name
		
		def nodeStringTypeNameOnly(n: NodeElem, node2id: String => Int) = 
			node2id(n.toString) + "[ label=\"" + n.toString.substring(n.toString.lastIndexOf(".")+1) + "\"]"
		val nodes = edges.foldLeft(List[NodeElem]())((l, e) => e.getSource :: e.getTarget :: l) 
	  	var (node2id, count) = nodes.foldLeft((scala.collection.mutable.Map[String, Int](), 0)) ((m, node) => ((m._1 + (node.toString -> m._2)), m._2 + 1))

		 def subgraphname(n: String) = n.replace('.', '_')
         def nodestrlist(ts: List[TypeNode]) =  ts.foldLeft(Nil: List[String]) ((s, v) => (nodeStringTypeNameOnly(v, node2id) :: s)) 
         
         val listm2ts = mns.map(mn => (mn, (mn.types.filter(t => nodes.contains(t))))) 
         val nodesOuter = nodes.filter(n => !listm2ts.flatMap(_._2 ).contains(n))
         
		 def moduleString(m2ts: (ModuleNode, List[TypeNode]), count: Int) = 
			 "subgraph cluster" + count + 
		 		"	{  \n" + "node [shape=plaintext, fontname=Skia, fontsize=16];" + 
		 				"\nlabel=\"" + m2ts._1 + "\";\n" + 
		 				//(if (m2ts._1.isInstanceOf[LayerModuleNode]) "style=filled;\n" else "") + 
		 				nodestrlist(m2ts._2).mkString(";\n") + ";\n"+ 
		 		"}\n"                                                     
		 
		 val nodeStrs = 
			listm2ts.foldLeft( (Nil: List[String], 0) ) ( ( liststr, m2ts) => (moduleString(m2ts, liststr._2) :: liststr._1,liststr._2 + 1))

	     val edgeStrs = edges.foldLeft(Nil: List[String]) ( (r, e) => edgeStringType(e, node2id) :: r )
	     
	     FileOut.saveFile(dotname + ".dot", 
	                   "digraph " + name + " { \n" 
	                   + "fontname=Skia; \n fontsize=16; \n"
	                   + edgestyle 
	                   + "\nnode [shape=plaintext, fontname=Skia, fontsize=18];\n"
	                   + nodeStrs._1.mkString(" ")
	                   + nodesOuter.foldLeft(Nil: List[String]) ((s, v) => (nodeStringType(v, node2id) :: s)).mkString(";\n") + ";\n"
	                   + edgeStrs.mkString(";\n") + "\n}")
	}
	
	
	def toDotTypeSubtype(mns: List[ModuleNode], edges: List[DepEdge], overridedNodes: List[NodeElem], name: String): Unit = {
		//val dotname = if ( dotfilename.indexOf("/") != -1) dotfilename.substring(dotfilename.lastIndexOf("/") + 1) else dotfilename
		val dotname = config.Project.get.getOutputPath + "/" + name
		
		def nodeStringTypeNameOnly(n: NodeElem, node2id: String => Int) =  {
			node2id(n.toString)  + "[ label=\""  +
				(if ( overridedNodes.contains(n))  n.toString.substring(n.toString.lastIndexOf(".")+1)
				  else  node2id(n.toString)) + "\"]"
		}
		val nodes = edges.foldLeft(List[NodeElem]())((l, e) => e.getSource :: e.getTarget :: l) 
	  	var (node2id, count) = nodes.foldLeft((scala.collection.mutable.Map[String, Int](), 0)) ((m, node) => ((m._1 + (node.toString -> m._2)), m._2 + 1))

		 def subgraphname(n: String) = n.replace('.', '_')
         def nodestrlist(ts: List[TypeNode]) =  ts.foldLeft(Nil: List[String]) ((s, v) => (nodeStringTypeNameOnly(v, node2id) :: s)) 
         
         val listm2ts = mns.map(mn => (mn, (mn.types.filter(t => nodes.contains(t)))))
         
		 def moduleString(m2ts: (ModuleNode, List[TypeNode]), count: Int) = 
			 "subgraph cluster" + count + 
		 		"	{  \n" + "node [shape=plaintext, fontname=Skia, fontsize=10];" + 
		 				"\nlabel=\"" + m2ts._1.toDot + "\";\n" + 
		 					(if (m2ts._1.isInstanceOf[LayerModuleNode]) "style=filled;\n" else "") + 
		 				nodestrlist(m2ts._2).mkString(";\n") + ";\n"+ 
		 		"}\n"                                                     
		 
		 val nodeStrs = 
			listm2ts.foldLeft( (Nil: List[String], 0) ) ( ( liststr, m2ts) => (moduleString(m2ts, liststr._2) :: liststr._1,liststr._2 + 1))

	     val edgeStrs = edges.foldLeft(Nil: List[String]) ( (r, e) => edgeStringType(e, node2id) :: r )
	     
	     FileOut.saveFile(dotname + ".dot", 
	                   "digraph " + name + " { \n" 
	                   + "fontname=Skia; \n fontsize=16; \n"
	                   + edgestyle 
	                   + nodeStrs._1.mkString(" ")
	                   + edgeStrs.mkString(";\n") + "\n}")
	     println("ID \tType")
	     val lst = node2id.map(entry => (entry._2, entry._1)).toList
	     lst.sort(_._1 < _._1 ).foreach(entry => println(entry._1 + "\t" + entry._2))
	}
	
}