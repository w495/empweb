<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<!--
        
    ЗАМЕЧАНИЕ: При прочих равных условиях лучше использовать CSS
-->

<xsl:template name="toCaseVariant">
    <xsl:param name="Case" />
    <xsl:param name="Text" />
    
    <xsl:variable name="SMALLCASE"
        select="'abcdefghijklmnopqrstuvwxyzабвгдеёжзиклмнопрстуфхцчшщъыьэюя '"/>
    <xsl:variable name="UPPERCASE"
        select="'ABCDEFGHIJKLMNOPQRSTUVWXYZAБВГДЕЁЖЗИКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ^'"/>

    <xsl:choose>
        <xsl:when test="$Case = 'upper'">
            <xsl:value-of select="translate($Text, $SMALLCASE, $UPPERCASE)"  />
        </xsl:when>
        <xsl:when test="$Case = 'lower'">
            <xsl:value-of select="translate($Text, $UPPERCASE, $SMALLCASE)" />
        </xsl:when>
        <xsl:otherwise>
            <xsl:text>!ERR!</xsl:text>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<xsl:template name="toCapitlize">
    <xsl:param name="Text" />
        <xsl:call-template name="toCaseVariant">
              <xsl:with-param name="Case" select="'upper'"/>
              <xsl:with-param name="Text" select="substring($Text, 1, 1)"/>
        </xsl:call-template>
        <xsl:value-of select="substring($Text, 2)" />
</xsl:template>

</xsl:stylesheet>
