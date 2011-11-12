/**
 * ClassFileLoader.java
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */
package bcel.typeanalysis;

import java.io.File;
import java.io.FilenameFilter;
import java.util.HashMap;


public class ClassFileLoader {

	public void getClassNames(File root, HashMap<Integer, TypeObject> classMap) {
		load(root.getAbsolutePath(), root, classMap);
	}

	public HashMap<Integer, TypeObject> getClassNames(File root) {
		HashMap<Integer, TypeObject> classMap = new HashMap<Integer, TypeObject>();
		load(root.getAbsolutePath(), root, classMap);

		return classMap;
	}

	private void loadDir(String root, File parent,
			HashMap<Integer, TypeObject> classMap) {
		FilenameFilter filter = new FilenameFilter() {
			public boolean accept(File dir, String name) {
				return name.endsWith(".class");
			}
		};
		File[] files = parent.listFiles(filter);
		for (File file : files) {
			String path = file.getAbsolutePath();
			if (path.endsWith(".class")) {
				int index = path.lastIndexOf(".class");
				path = path.substring(0, index);
			}
			path = path.replace(root, "");
			if (File.separatorChar == '/') {
				path = path.replaceAll("/", ".");
			} else {
				path = path.replaceAll("\\\\", ".");
			}
			if (path.charAt(0) == '.') {
				path = path.replaceFirst(".", "");
			}
			int id = classMap.size();
			classMap.put(id, new TypeObject(id, path));
		}
	}

	private void load(String root, File directory,
			HashMap<Integer, TypeObject> classMap) {
		loadDir(root, directory, classMap);

		File[] children = directory.listFiles();
		for (File child : children) {
			if (!child.isDirectory())
				continue;

			load(root, child, classMap);
		}
	}
}
