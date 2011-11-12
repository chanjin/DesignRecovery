/**
 * TypeObject.java
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */
package bcel.typeanalysis;
import org.apache.bcel.classfile.JavaClass;



public class TypeObject {
	private int id;
	private String name;
	private JavaClass clazz;

	public TypeObject(int id, String name) {
		this.id = id;
		this.name = name;
	}

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setJavaClass(JavaClass clazz) {
		this.clazz = clazz;
	}

	public JavaClass getJavaClass() {
		return clazz;
	}
	

}
