<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template name="pFormatText" >
    <!--
        Обрамляет текстовые абзацы (text1 \n text2) тегами <p> </p>.
    -->
    <xsl:param name="Input" />
    
    <xsl:variable name="SEPARATOR" select="'&#xa;'" />
    <!-- &#xa; - суть \n -->

    <xsl:choose>
        <xsl:when test="contains($Input, $SEPARATOR)">
            <p >
                <xsl:value-of select="substring-before($Input, $SEPARATOR)" disable-output-escaping="true" />
            </p>
            <xsl:call-template name="pFormatText">
                <xsl:with-param 
                    name="Input" 
                    select="substring-after($Input, $SEPARATOR)"
                />
            </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of  select="$Input" disable-output-escaping="true" />
        </xsl:otherwise>
    </xsl:choose>
    
</xsl:template>

</xsl:stylesheet>
