package util;


/**
 * FileOut.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

object FileOut {
	import java.io.{FileOutputStream, PrintWriter}
	def saveFile(file: String, str: String) = {
		 var fos: FileOutputStream = null
		 var writer: PrintWriter = null
		 try{   
		  	 fos = new FileOutputStream(file)
		  	 writer = new PrintWriter(fos, true)
			 writer.print(str)
			 writer.close()
		  	 fos.close()
		 }
	    finally {
	    	
	    }
	}
  
	import java.io.File
	def removeFiles(dir: String, ext: String) = {
		val directory = new File(dir)
		directory.listFiles.foreach(f => { 
			if (f.getName.endsWith(ext)) {
				//println(f.getName)
				f.delete
			}
		})	   
	}
	
	def makeDirectory(dir: String) = {
		val directory = new File(dir)
		if ( !directory.exists ) {
			directory.mkdir
		}
		else 
			directory.listFiles.foreach( f => f.delete)
	}
}

/*object FileOutTest extends Application {
	(new FileOut).removeFiles("/Users/chanjinpark/Develop/ws_design_recovery/DesignRecovery/Jars/android 2.2", "dot")
}
*/