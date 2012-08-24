<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../../shared/master/customer/change.xsl"/>

<xsl:template name="s-main-change">
    <xsl:text>Ваш аккаунт был ободрен модератором! &#xa;</xsl:text>
    <xsl:text>Теперь можно перейти в личный кабинет: &#xa;</xsl:text>
    <xsl:value-of select="/data/meta/sys-dns" />
    <xsl:text>/pers &#xa;</xsl:text>
</xsl:template>
    

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>


