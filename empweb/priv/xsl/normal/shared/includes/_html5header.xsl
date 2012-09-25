<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" >

<xsl:template name="html5header" >
    <xsl:text disable-output-escaping="yes"><![CDATA[﻿<!doctype html>
    <!--[if lt IE 7 ]> <html lang="ru" class="no-js ie6" xmlns="http://www.w3.org/1999/xhtml" > <![endif]-->
    <!--[if IE 7 ]>    <html lang="ru" class="no-js ie7" xmlns="http://www.w3.org/1999/xhtml" > <![endif]-->
    <!--[if IE 8 ]>    <html lang="ru" class="no-js ie8" xmlns="http://www.w3.org/1999/xhtml" > <![endif]-->
    <!--[if IE 9 ]>    <html lang="ru" class="no-js ie9" xmlns="http://www.w3.org/1999/xhtml" > <![endif]-->
    <!--[if (gt IE 9)|!(IE)]><!-->]]></xsl:text>
</xsl:template>


<xsl:template name="html5header-min" >
    <xsl:text disable-output-escaping="yes"><![CDATA[<!doctype html>]]></xsl:text>
</xsl:template>

<xsl:template name="html5header-end" >
    <xsl:text disable-output-escaping="yes"><![CDATA[<!--<![endif]-->]]></xsl:text>
</xsl:template>


</xsl:stylesheet>
