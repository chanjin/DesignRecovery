/**
 * TypeStructure.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */
package module

class TypeNode(qn: String) extends NodeElem(qn) {
	var rank = -1
	def pkgname = if ( qn.contains(".") ) qn.substring(0, qn.lastIndexOf(".")) else "." 
	override def toDot = value + ( if (rank > -1) "(" + rank + ")" else "")
}

object TypeDepKind extends Enumeration {
	type TypeDepKind = Value
	// StructuralDep: Class Declaration Dependency
	// ByteCodeDep: Dependency including Dependent Types in Method Body
	// DirectInhDep: Direct Parent by inherits and implements Dependency
	// OverridingDep: Transitive Class Inheritance or Interface Implementation Dependency
    val StructuralDep, ByteCodeDep, DirectInhDep, OverridingDep = Value
    
    def kindFromStr(s: String) = s match {
		case "P" => TypeDepKind.DirectInhDep 
		case "S" => TypeDepKind.StructuralDep 
		case "C" => TypeDepKind.ByteCodeDep 
		case "O" => TypeDepKind.OverridingDep 
	}
	
	def getDepCode(kind: TypeDepKind) = kind match {
		case TypeDepKind.DirectInhDep => "P"
		case TypeDepKind.StructuralDep => "S"
		case TypeDepKind.ByteCodeDep  => "C"
		case TypeDepKind.OverridingDep  => "O"
	}
}

import TypeDepKind._

class TypeEdge  (n1: NodeElem, n2: NodeElem, kind: TypeDepKind) extends DepEdge(n1, n2) {
	var kinds = List[TypeDepKind]()
	addKind(kind)
	
	def addKind(k: TypeDepKind) = {
		if (!kinds .contains(k)) // || !(kinds.contains(TypeDepKind.OverridingDep ) && k == TypeDepKind.ParentDep)) 
			kinds = k :: kinds
		//if ( k == TypeDepKind.OverridingDep ) kinds = kinds.remove(_ == ParentDep)
	}
	def addKinds(ks: List[TypeDepKind]) = ks.foreach(k => if(!kinds.contains(k)) addKind(k))
	def getKinds:  List[TypeDepKind] = kinds
	def getDepCode = kinds.map( k => TypeDepKind.getDepCode(k)).mkString(",")
	def isKindOf(k: TypeDepKind) = kinds.contains(k)
	override def isSubtypeEdge = kinds.contains(TypeDepKind.OverridingDep )
	override def toDot = getDepCode
}

class SubtypeEdge (n1: NodeElem, n2: NodeElem, ms: List[String]) extends  TypeEdge(n1, n2, TypeDepKind.OverridingDep ) {
	def getMethods = ms
	//override def toDot = super.toDot //+ ": " + ms.mkString(",")
}



class TypeStructure extends DependencyGraph {
	def addNode(value: String): TypeNode = {
		val tn: TypeNode = new TypeNode(value) 
		nodes = Map(value -> tn) ++ nodes
		tn
	}
	
	def addNode(tn: TypeNode): TypeNode = {
		nodes = Map(tn.value -> tn) ++ nodes
		tn
	}
	
	def addTypeEdge(n1: String, n2: String, kind: TypeDepKind): TypeEdge = {
		//println("ADDSimple: " + n1 + ", " + n2 + ": " + kind)
		if ( n1 == n2 ) return null
		var e = getEdge(n1, n2).asInstanceOf[TypeEdge]
		if ( e == null ) { 
			e = new TypeEdge(nodes(n1).asInstanceOf[TypeNode], nodes(n2).asInstanceOf[TypeNode], kind)
			addEdge(e)
		}
		else 
			e.asInstanceOf[TypeEdge].addKind(kind)
		e
	}
	
	def addSubtypeEdge(n1: String, n2: String, ms: List[String]): TypeEdge = {
		//println("Subtype: " + n1 + ", " + n2 + ": " + ms)
		assert(n1 != n2)
		var e = getEdge(n1, n2).asInstanceOf[TypeEdge]
		if (e == null || !e.isSubtypeEdge) {
			if ( e != null ) removeEdge(e)
			val ne = new SubtypeEdge(nodes(n1).asInstanceOf[TypeNode], nodes(n2).asInstanceOf[TypeNode], ms)
			if ( e != null ) ne.addKinds(e.getKinds)
			addEdge(ne)
			e = ne
		}
		else {
			e.asInstanceOf[TypeEdge].addKind(TypeDepKind.OverridingDep )
		}
	    e
	}

 	
 	def inherits(subclass: String, superclass: String) : Boolean = {
 		def getAncestors(cls: TypeNode) : List[TypeNode] = {
 			val parent = cls.outedges.filter(e => {
 				if ( e.isInstanceOf[TypeEdge] ) {
 					val se = e.asInstanceOf[TypeEdge]
 					se.getKinds.exists(k => k == TypeDepKind.DirectInhDep || k == TypeDepKind.OverridingDep )
 				}
 				else false 
 			}).map(_.getTarget)

 			if ( parent.length == 0 ) {
 				return List[TypeNode]()
 			}
 			else {
 				parent.map(_.asInstanceOf[TypeNode]) ::: parent.flatMap(p => getAncestors(p.asInstanceOf[TypeNode]))
 			}
 		}
 		
 		val ancestors = getAncestors(nodes(subclass).asInstanceOf[TypeNode])
 		//println("anc of " + subclass + " - " + ancestors)
 		ancestors.contains(nodes(superclass))
 	}
 	
 	def getChild(parent: String) : String = {
 		val childedges = nodes(parent).inedges.filter(e => { 
 			val edge = e.asInstanceOf[TypeEdge]
 			edge.getKinds.exists(_ == TypeDepKind.DirectInhDep )
 		})
 		if ( childedges.length == 0 ) return null
 		else {
 			childedges.head.getSource.value 
 		}
 	}
  	

 	
 	
 	
 	private def getParents(tn: NodeElem, totn: NodeElem, prevList: List[NodeElem]) : List[NodeElem]  = {
 		if ( tn == totn ) return totn :: prevList
		
		val parentEdges = tn.outedges.filter(e => e.asInstanceOf[TypeEdge].getKinds.contains(TypeDepKind.DirectInhDep))
		parentEdges.foreach(e => { 
				val result = getParents(e.getTarget, totn, e.getSource :: prevList)
				if ( result != null) return result
			}
		)
		null
	}
 	
 	def getParentList(tn: String, totn: String) : List[NodeElem] = {
 		if ( getNode(tn) == null || getNode(totn) == null ) return null
 		getParents(getNode(tn), getNode(totn), List[NodeElem]()).reverse
 	}
 	
 	

}


object TypeStructure {
	
	private def addCloneEdge(tdg: TypeStructure, se: TypeEdge) = {
		if ( se.isSubtypeEdge  ) {
			val edge = new SubtypeEdge(tdg.nodes(se.getSource.value), tdg.nodes(se.getTarget.value), se.asInstanceOf[SubtypeEdge].getMethods)
			edge.addKinds(se.getKinds)
			tdg.addEdge(edge)
		}
		else {
			val edge = new TypeEdge(tdg.nodes(se.getSource.value), tdg.nodes(se.getTarget.value ), se.getKinds.head)
			edge.addKinds(se.getKinds.tail)
			tdg.addEdge(edge)
		}
 	}
	 	
	def makeStructureWithoutEdges(tdg: TypeStructure, edgeFilter: DepEdge => Boolean) : TypeStructure = {
 		// edgeFilter's result is true => removed
 		var newts = new TypeStructure
 		tdg.nodes.values.foreach(node => newts.addNode(node.value).rank = node.asInstanceOf[TypeNode].rank)
 		tdg.edges.filter(e => !edgeFilter(e)).foreach(edge => addCloneEdge(newts, edge.asInstanceOf[TypeEdge]))
 		newts
 	}
	
	// get cloned type structure in which edges are filtered by edgeKind
 	def getNewTypeStrcuture( tdg: TypeStructure, edgeKind: TypeDepKind) = {
		val newtdg = new TypeStructure
		
		tdg.edges.foreach(e => { 
			val kinds = e.asInstanceOf[TypeEdge].getKinds
			if ( kinds.contains(edgeKind)) {// excludes bytecode dependency but not structural dependency
				if ( !newtdg.nodes.contains(e.getSource.value )) newtdg.addNode(e.getSource.value)
				if ( !newtdg.nodes.contains(e.getTarget.value )) newtdg.addNode(e.getTarget.value)
				
				val newedge: TypeEdge = 
					if ( e.isSubtypeEdge ) {
						 newtdg.addSubtypeEdge(e.getSource.value, e.getTarget.value, e.asInstanceOf[SubtypeEdge].getMethods)
					}
					else { 
						newtdg.addTypeEdge(e.getSource.value , e.getTarget.value, kinds.head)
					}
				newedge.addKinds(kinds)
			}
		})
		newtdg
	}
 	
	def mergeInnerClasses(tdg: TypeStructure) = {
		/*val newtdg = new TypeStructure
		val inners = tdg.nodes.values.foldLeft(Map[NodeElem, NodeElem]()) ((m, t) => {
			if ( t.value.contains("$") ) {
				val name = t.value.substring(0, t.value.indexOf("$"))
				assert(tdg.nodes.contains(name))
				m + (t -> tdg.nodes(name) )
			}
			else m 
		})
		
		tdg.nodes.values.foreach(n => if ( !inners.contains(n) ) newtdg.addNode(n.value))
		tdg.edges.foreach(e => {
			val source = if ( inners.contains(e.getSource) ) inners(e.getSource) else e.getSource
			val target = if ( inners.contains(e.getTarget) ) inners(e.getTarget) else e.getTarget
			val existingEdge = newtdg.getEdge(e.getSource, e.getTarget)

			// inners or not, subtype or not
			if ( inners.contains(e.getSource) || inners.contains(e.getTarget)) {
				if ( e.isSubtypeEdge ) addEdge
			}
			else {
				
				if ( e.isSubtypeEdge ) newtdg.addSubtypeEdge(e.getSource, e.getTarget)
				else newtdg.addTypeEdge(e.getSource, e.getTarget)
			}
			
			
			val edge = tdg.getEdge(source.value, target.value)
			if ( edge != null)
			
		})
		
		
		
		
		newtdg*/
	}
}
