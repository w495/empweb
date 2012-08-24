<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template name="hotline">
    <xsl:param name="Srcs" select="/data/srcs" />
    <xsl:param name="Visible" />
    
    <section>
        <div class="hl-head">
            <xsl:text>Единый «телефон доверия» МЧС России: 	(495) 449-99-99</xsl:text><br/>
            <xsl:text>Экстренные телефоны по регионам:</xsl:text>
        </div>
        <ul>
            <xsl:for-each select="$Srcs/item">
                <xsl:sort select="name" data-type="text" order="ascending" case-order="upper-first"/>
                <li class="hl-item">
                    <div class="hl-item-name">
                        <xsl:value-of select="name" disable-output-escaping="yes" />
                    </div>
                    <div class="hl-item-data">
                        <xsl:attribute name="style">
                            <xsl:if test="not($Visible)">display:none</xsl:if>
                        </xsl:attribute>
                        <xsl:call-template name="hl-item-data">
                            <xsl:with-param name="Value" select="value" />
                        </xsl:call-template>
                    </div>
                </li>
            </xsl:for-each>
        </ul>
    </section>
</xsl:template>


<xsl:template name="hl-item-data">
    <xsl:param name="Value" />
    <table>
        <xsl:value-of select="$Value" disable-output-escaping="yes"/>
    </table>
</xsl:template>


</xsl:stylesheet>
