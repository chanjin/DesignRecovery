
/**
 * SepaBcelRepository.java
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

package bcel.typeanalysis;

import java.io.IOException;
import java.io.InputStream;
import java.util.*;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;

import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.util.ClassPath;
import org.apache.bcel.util.Repository;



public class SepaBcelRepository implements Repository {
	private static final String DEFAULT_PATH = ClassPath.getClassPath();

	private static HashMap _instances = new HashMap(); // CLASSPATH X

	// REPOSITORY

	private ClassPath _path = null;

	protected HashMap _loadedClasses = new HashMap(); // CLASSNAME X JAVACLASS

	private SepaBcelRepository(ClassPath path) {
		_path = path;
	}

	public static SepaBcelRepository getInstance() {
		return getInstance(ClassPath.SYSTEM_CLASS_PATH);
	}

	public static SepaBcelRepository getInstance(ClassPath classPath) {
		SepaBcelRepository rep = (SepaBcelRepository) _instances
				.get(classPath);

		if (rep == null) {
			rep = new SepaBcelRepository(classPath);
			_instances.put(classPath, rep);
		}

		return rep;
	}

	/**
	 * Store a new JavaClass instance into this Repository.
	 */
	public void storeClass(JavaClass clazz) {
		_loadedClasses.put(clazz.getClassName(), clazz);
		clazz.setRepository(this);
	}

	/**
	 * Remove class from repository
	 */
	public void removeClass(JavaClass clazz) {
		_loadedClasses.remove(clazz.getClassName());
	}

	/**
	 * Find an already defined (cached) JavaClass object by name.
	 */
	public JavaClass findClass(String className) {
		return (JavaClass) _loadedClasses.get(className);
	}

	/**
	 * Load a JavaClass object for the given class name using the CLASSPATH
	 * environment variable.
	 */
	public JavaClass loadClass(String className) throws ClassNotFoundException {
		if (className == null || className.equals("")) {
			throw new IllegalArgumentException("Invalid class name "
					+ className);
		}

		className = className.replace('/', '.'); // Just in case, canonical
		// form

		try {
			return loadClass(_path.getInputStream(className), className);
		} catch (IOException e) {
			throw new ClassNotFoundException(
					"Exception while looking for class " + className + ": "
							+ e.toString());
		}
	}

	/**
	 * Try to find class source via getResourceAsStream().
	 *
	 * @see Class
	 * @return JavaClass object for given runtime class
	 */
	public JavaClass loadClass(Class clazz) throws ClassNotFoundException {
		String className = clazz.getName();
		String name = className;
		int i = name.lastIndexOf('.');

		if (i > 0) {
			name = name.substring(i + 1);
		}

		return loadClass(clazz.getResourceAsStream(name + ".class"), className);
	}

	private JavaClass loadClass(InputStream is, String className)
			throws ClassNotFoundException {
		JavaClass clazz = findClass(className);

		if (clazz != null) {
			return clazz;
		}

		try {
			if (is != null) {
				ClassParser parser = new ClassParser(is, className);
				clazz = parser.parse();

				storeClass(clazz);

				return clazz;
			}
		} catch (IOException e) {
			throw new ClassNotFoundException(
					"Exception while looking for class " + className + ": "
							+ e.toString());
		}

		throw new ClassNotFoundException("SepaBcelRepository could not load "
				+ className);
	}

	/**
	 * Clear all entries from cache.
	 */
	public void clear() {
		_loadedClasses.clear();
	}


	public Collection getClassNames() {
		return _loadedClasses.keySet();
	}
	
	public void loadClasses(String file) {
		try {
			JarFile jarfile = new JarFile(file); 			// Open the JAR file
			Enumeration entries = jarfile.entries();	// 포함된 모든 파일들 (.class, .gif, ... )

			while (entries.hasMoreElements()) {
				ZipEntry entry = (ZipEntry) entries.nextElement();
				String name = entry.getName();
				if (name.endsWith(".class")) {
					int index = name.lastIndexOf(".");
					try {
						loadClass(name.substring(0, index));
					}
					catch (ClassNotFoundException e) {
						//System.err.println(e);
						System.err.println("\t" + name.substring(0, index) + " is not found!");
					}
				}
			}

			jarfile.close();
		} catch (IOException e) {
			System.err.println(e);
		}
	}

	@Override
	public ClassPath getClassPath() {
		return _path;
	}
}
