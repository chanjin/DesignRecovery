package module

/**
 * ModuleStructure.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

class ModuleNode(qn: String) extends NodeElem(qn) {
	var types = List[TypeNode]()
	//def addTypeNode(t: TypeNode) = t :: types
	//def getTypes = types
 	override def toString = value + "(" + types.length + ")"
 	var pkgname: String = null
 	def packageName = pkgname
 	def interfaceTypes = types.filter(t => t.inedges.exists( e => !types.contains(e.getSource)))
}


class ModuleEdge(m1: ModuleNode, m2: ModuleNode) extends DepEdge(m1, m2) {
	private var typeedges = List[DepEdge]()
	private var hasSubtype = false
	def addTypeEdge(e : DepEdge ) {
    	typeedges = e :: typeedges
    	if ( !hasSubtype && e.isSubtypeEdge ) hasSubtype = true
  	}
  	override def isSubtypeEdge: Boolean = hasSubtype
  	def getTypeEdges = typeedges
  	def edgeCount = typeedges.length
  	def subtypeEdgeCount = 
    	typeedges.filter(_.isSubtypeEdge).length
    override def toDot: String = {
  		val pkg = getTarget.asInstanceOf[ModuleNode].packageName
    	edgeCount + ":" + subtypeEdgeCount + ":" + typeedges.map(_.getTarget).removeDuplicates.length // +
    	//typeedges.map(_.getTarget).removeDuplicates.map(n => n.value.substring(if (pkg.length < n.value.length ) pkg.length + 1 else 0)).mkString(",")
    }
}


class ModuleStructure(g: TypeStructure) extends DependencyGraph {
  	def tdg = g
  	var mapT2M: Map[TypeNode, ModuleNode] = Map()
   
   def addTypeNode(module: ModuleNode, tstr: String) = {
	  var tn = tdg.nodes(tstr).asInstanceOf[TypeNode]
	  mapT2M += tn -> module
	  module.types = tn :: module.types
	  module
   }
  	
  	def addTypeNodes(module: ModuleNode, ts: List[TypeNode]) = {
  		ts.foreach(t => mapT2M += (t -> module))
  		module.types = ts ::: module.types
  		module
  	}

 
 	def liftEdges(): ModuleStructure = {
	  for ( module <- nodes.values.toList ) {
	    val m1 = module.asInstanceOf[ModuleNode]
	    var medges: Map[ModuleNode, ModuleEdge] = Map()
	    val outEdges: List[DepEdge] = ( m1.types.flatMap(_.outedges) ) 
	    for ( e <- outEdges ) {
	    	var m2: ModuleNode = mapT2M(e.getTarget.asInstanceOf[TypeNode])
	    	if ( m1 != m2 ) {
		    	if ( !medges.contains(m2) ) {
		    	  medges += (m2 -> (addEdge(new ModuleEdge(m1, m2))).asInstanceOf[ModuleEdge]) 
		    	}
		    	medges(m2).addTypeEdge(e)
	    	}
	    }
     }
	   this
	}
  
  def getEdgesUnderThreshold( ) : List[DepEdge] = {
	  // define edge density, mean & min(lower 10%, 5)
	  // in only package diagram, exclude the edges after lift edges
	  if ( edges.length == 0 ) return Nil
	  
	  def min(x: Int, y: Int) = if ( x > y ) y else x
	  val es = edges
	  val sumOfEdges: Int = es.foldLeft(0)((sum, e) => sum + (e.asInstanceOf[ModuleEdge].edgeCount))
	  val mean =sumOfEdges / es.length 
	  var variation = ( es.foldLeft(0)((sum, e) => (sum + (e.asInstanceOf[ModuleEdge].edgeCount - mean)*(e.asInstanceOf[ModuleEdge].edgeCount -mean)) ) /  (es.length * es.length))
	  val threshold = min ( ( mean - 2.33* Math.sqrt(variation) ).toInt, 3)
	  println("m: " + mean + ", v "  + variation +", lower 1%: " + (mean - 2.33*Math.sqrt(variation)) + ", threshold = " + threshold)
	  // threshold (x - m) / sigma < - 1.645 (lower 0.5%), x < m - 1.645 * sigma을 만족하는 최대 정수
	  
	  def excludeCondition(e: ModuleEdge) = e.edgeCount <= threshold && !(e.isSubtypeEdge)
	  val e2exclude = es.filter(e => excludeCondition(e.asInstanceOf[ModuleEdge])) 
	  println("exclueded edges: "+e2exclude.length +", " + e2exclude  )
   
	  e2exclude
	}
  
  	override def removeEdges(es: List[DepEdge]) = {
 	  	super.removeEdges(es)
		tdg.removeEdges(es.flatMap(_.asInstanceOf[ModuleEdge].getTypeEdges))
	}
 
 
	
	def getNodesWithPrefix(prefixes: List[String]) : List[NodeElem]= {
		def hasModulePrefix(pname: String): Boolean = prefixes.exists(pname.startsWith(_))
		nodes.values.toList.filter(p => hasModulePrefix(p.value))
	}
	
	def getInterfaceClasses: Map[ModuleNode, List[TypeNode]] = {
        var result = Map[ModuleNode, List[TypeNode]]()
        nodes.values.foreach(mn => {
        	val mnode = mn.asInstanceOf[ModuleNode];
        	if (mnode.interfaceTypes.length > 0)
                result += (mnode -> mnode.interfaceTypes)
        })
        result
    }
	
	
	def toTable = {
		val modules = nodes.values.toList.sort((m1, m2) => m1.value.compareTo( m2.value ) < 0 )
		println(" *** MODULE Details ")
		modules.map(m => { 
			val mn = m.asInstanceOf[ModuleNode]
			def shortname(v: String) = {
				if ( v.startsWith(mn.packageName) ) v.substring(mn.packageName.length  + 1)
				else v
			}
			
			val (name, numTypes, typeList) = (mn.value, mn.types.length, mn.types.map(t => shortname(t.value)))
			name + "\t" + numTypes + "\t" + typeList.toList.sort( (t1, t2) => t1.compareTo(t2) < 0).mkString(", ")
		}).foreach(str => println(str))
		println(" *** --- ")
	}
}


trait CompositeModuleNode[T <: ModuleNode] {
    var modules = List[T]()
    def getModules(ms: List[T]): List[T] = {
        ms.foldLeft(List[T]())((result, n) =>
            if (n.isInstanceOf[CompositeModuleNode[T]])
                n.asInstanceOf[CompositeModuleNode[T]].modules ::: result else n :: result);
    }
    def addModule(n: T) = {
        modules = if (n.isInstanceOf[CompositeModuleNode[T]])
            n.asInstanceOf[CompositeModuleNode[T]].modules ::: modules else n :: modules
    }
    def getTypeNodes = modules.flatMap(_.types)
    
    def formattedStr(me: T, suffixes: => List[String]): String = {
        val dotstr = toString
        if (suffixes.length > 0) {
            val size = 50
            var formatted = new String
            
            if ( suffixes.length > 1 ) {
	            dotstr + "\\n"+ suffixes.tail.
	            	foldLeft((new StringBuffer(suffixes.head), 0))((result, str) => { 
	            	val (fmtstr, count) = result
	            	if ( count + str.length + 1 > size ) (fmtstr.append(",\\n" + str), str.length)
	            	else (fmtstr.append(", " + str), count + str.length + 1)
	            }).toString
            }
            else 
            	dotstr + "\\n" + suffixes.head
        } else
            dotstr
    }
}

