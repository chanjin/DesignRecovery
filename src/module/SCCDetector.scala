package module

/**
 * SCCDetector.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

class SCCDetector(nodes: Map[String, NodeElem]) {
	class TarjanNode (n: NodeElem) {
		var  index = -1
		var  lowlink = -1
		val 	node = n
		override def toString  = if ( node != null ) node.value else null
	}

	private def min(a: Int, b: Int) = if ( a > b ) b else a

	var nodesscc: Map[NodeElem, TarjanNode] = Map()
	

	var index = 0
	var stack: scala.collection.mutable.Stack[TarjanNode] = new scala.collection.mutable.Stack()
	var result: List[List[NodeElem]] = Nil
	
	def run : List[List[NodeElem]] = {
		nodes.values.foreach( n =>  nodesscc += (n -> new TarjanNode(n)))
		nodesscc.foreach(n =>  if ( n._2.index == -1 ) tarjan(n._2))
		result
	}
    
	def tarjan( v: TarjanNode ) : Unit = {
		v.index = index
		v.lowlink = index
		index = index + 1
      
		stack.push(v)
		for ( n1 <- v.node.neighbors ) {
			val v1 = nodesscc(n1)
			if ( v1.index == -1 ) {
				tarjan(v1)
				v.lowlink = min(v.lowlink, v1.lowlink ) 
			}
			else if ( stack.contains(v1) )
        		v.lowlink = min(v.lowlink, v1.index ) 
		}
  
		var scc: List[TarjanNode] = Nil
		if ( v.lowlink == v.index ) {
			var v1: TarjanNode = new TarjanNode(null)
			do  {
				v1 = stack.pop
				scc = v1 :: scc
			} while ( !stack.isEmpty && v != v1 )
			if (scc.length > 1)   	{
				//println("SCC:" + scc)
				result = scc.map(_.node) :: result
			}
		}
    } // end of tarjan

}