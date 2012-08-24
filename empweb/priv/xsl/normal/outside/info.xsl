<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/title-page.xsl"/>

 

<xsl:template name="s-title">
    <xsl:text>Главная</xsl:text>
</xsl:template>


<xsl:template name="s-main-root">
	<center style="padding:100px;">
	   <xsl:value-of select="/data/meta/errormessage" />
	   <xsl:value-of select="/data/meta/message" />
	   <br />
	   <a href="/">Перейти на главную страницу</a>
	</center>
</xsl:template>
</xsl:stylesheet>
