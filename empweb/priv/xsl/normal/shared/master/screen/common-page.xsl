<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [
    <!ENTITY nbsp "&#160;">
    <!ENTITY raquo  "&#187;">
    <!ENTITY laquo  "&#171;">
]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="base.xsl"/>

<xsl:template name="s-title-base">
    <xsl:call-template name="s-title-common"/>
</xsl:template>

<xsl:template name="s-main-base">
    <xsl:call-template name="s-main-common"/>
</xsl:template>
 
<xsl:template name="foot-scripts-base">
    <script src="/j/jm.js"><xsl:text><![CDATA[ ]]></xsl:text></script>
    <script src="/j/sl.js"><xsl:text><![CDATA[ ]]></xsl:text></script> 
    <xsl:call-template name="foot-scripts-common"/>
</xsl:template>

<!--
    ###########################################################################
    ###
    ###########################################################################
-->

<!--
    TODO: Вынести в util
-->
<xsl:template name="u-button">
    <xsl:param name="Href" select="'Href'"/>
    <xsl:param name="Class" select="'but'"/>
    <xsl:param name="Title" select="'title'"/>
    <xsl:param name="Text" select="'text'"/>
    <a class="{$Class} m-button" href="{$Href}" title="{$Title}">
        <xsl:value-of select="$Text" />
    </a>
    <!-- Alt:
        <form>
            <input type="button" />
        </form>
    -->
</xsl:template>


</xsl:stylesheet>

