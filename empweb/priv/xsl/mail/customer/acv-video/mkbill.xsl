<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../../shared/master/acv-video/change.xsl"/>

<xsl:template name="s-main-change">
    <xsl:text>Вам выстален счет. &#xa;</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>Для оплаты просим Вас перейти по ссылке: &#xa;</xsl:text>
    <xsl:value-of select="/data/meta/sys-dns" />
    <xsl:text>/pay/</xsl:text>
    <xsl:value-of select="/data/acv-video/id" />
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>Благодарим за своевременную оплату! &#xa;</xsl:text>
</xsl:template>
    

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>


