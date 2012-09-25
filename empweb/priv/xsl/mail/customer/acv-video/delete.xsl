<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../../shared/master/acv-video/delete.xsl"/>

<xsl:template name="s-main-delete">
    <xsl:text>Ваша рекламная кампания была удалена модератором! &#xa;</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>Для выяснения причин </xsl:text>
    <xsl:text>просим связаться со службой поддержки &#xa;</xsl:text>
    <xsl:text>http://tvzavr.ru/feedback/form &#xa;</xsl:text>
    <xsl:text>&#xa;</xsl:text>
</xsl:template>
    

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>


