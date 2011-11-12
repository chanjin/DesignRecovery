package util

/**
 * Util.scala
 * 
 * 	Project: DesignRecovery
 * Copyright (c) 2011 Chanjin Park
 * License - GNU LESSER GENERAL PUBLIC LICENSE v3.0 (LGPL v3.0)
 *
 */

object Util {
	def isFromJar(path: String): Boolean = path.endsWith(".jar")
	
	
	import java.text.NumberFormat
   	val nfpct = NumberFormat.getPercentInstance()
   	nfpct.setMinimumFractionDigits(1);
   	val nfnum = NumberFormat.getNumberInstance()
   	nfnum.setMinimumFractionDigits(2)
   	
   	def percentage(t: Long, total: Long) : String = nfpct.format(t.asInstanceOf[Double] / total.asInstanceOf[Double])

}