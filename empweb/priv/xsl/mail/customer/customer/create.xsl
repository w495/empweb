<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../../shared/master/customer/create.xsl"/>

<xsl:template name="s-main-create">
    <xsl:text>Спасибо за создание аккаунта </xsl:text>
    <xsl:text>в рекламной системе Tvzavr! &#xa;</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>Дождитесь одобрения вашего аккаунта модератором. &#xa;</xsl:text>
    <xsl:text>Вам придет оповещение по почте. &#xa;</xsl:text>
    <xsl:text>Пока можно ознакомиться с документацией. &#xa;</xsl:text>
    <xsl:value-of select="/data/meta/sys-dns" />
    <xsl:text>/docs &#xa;</xsl:text>
</xsl:template>
    

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>
</xsl:stylesheet>


