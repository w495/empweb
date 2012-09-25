<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template name="areagen">
    <xsl:param name="Prefix"  select="'/Contacts/Details'" />
    <xsl:param name="Visible" />
    
    <xsl:variable name="Areas" select="document('areagen/areas.xml')/areas"/>
    
    <xsl:call-template name="areagen-area">
        <xsl:with-param name="Areas" select="$Areas/area" />
        <xsl:with-param name="Prefix" select="$Prefix" />
        <xsl:with-param name="Visible" select="$Visible" />
    </xsl:call-template>
    
</xsl:template>

<xsl:template name="areagen-area">
    <xsl:param name="Areas" />
    <xsl:param name="Prefix" />
    <xsl:param name="Visible" />
    
    <ul>
        <xsl:for-each select="$Areas">
            <li id="areagen-area-{tr}" class="cla">
                <xsl:choose>
                    <xsl:when test="not(item)">
                        <div class="cla-name">
                            <a href="{link}" class="cla-name-link">
                                <xsl:value-of select="name" />
                            </a>
                        </div>
                    </xsl:when>
                    <xsl:otherwise>
                        <div class="cla-name">
                            <xsl:value-of select="name" />
                        </div>
                        <div class="cla-list">
                            <xsl:attribute name="style">
                                <xsl:if test="not($Visible)">display:none</xsl:if>
                            </xsl:attribute>
                            <xsl:call-template name="areagen-item">
                                <xsl:with-param name="Items" select="item" />
                                <xsl:with-param name="Prefix" select="$Prefix" />
                                <xsl:with-param name="AreaTr" select="tr" />
                                <xsl:with-param name="Visible" select="$Visible" />
                            </xsl:call-template>
                        </div>
                    </xsl:otherwise>
                </xsl:choose>
            </li> 
        </xsl:for-each>
    </ul>
</xsl:template>

<xsl:template name="areagen-item">
    <xsl:param name="Items" />
    <xsl:param name="Prefix" />
    <xsl:param name="Visible" />
    
    <ul>
        <xsl:for-each select="$Items">
            <xsl:sort select="name" data-type="text" order="ascending" case-order="upper-first"/>
            <li class="cla-item">
                <div class="cla-item-name">
                    <xsl:value-of select="name" />
                </div>
                <div class="cla-item-data" style="display:none">
                    <xsl:attribute name="style">
                        <xsl:if test="not($Visible)">display:none</xsl:if>
                    </xsl:attribute>
                    <xsl:choose>
                        <xsl:when test="not(data/field)">
                            <div class="row">
                                <xsl:value-of select="data" />
                            </div>
                        </xsl:when >
                        <xsl:otherwise>
                            <table>
                                <xsl:for-each select="data/field">
                                    <tr>
                                        <td class="name">
                                            <xsl:value-of select="name" />
                                        </td>
                                        <td class="value">
                                            <xsl:value-of select="value" />
                                        </td>
                                    </tr>
                                </xsl:for-each>
                            </table>
                        </xsl:otherwise>
                    </xsl:choose>
                </div>
            </li> 
        </xsl:for-each>
    </ul>
</xsl:template>

</xsl:stylesheet>
