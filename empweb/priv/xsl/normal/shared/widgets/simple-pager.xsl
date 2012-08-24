<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<!--
    Описание простого педжера,
    который делает простую педженацию
    (постраничное меню, как в поиске yandex)
-->

<xsl:template name="simple-pager">
    <xsl:param name="Current"    />
        <!-- Номер текущей страницы -->
    <xsl:param name="Total"     />
        <!-- Общее число страниц -->
    <xsl:param name="Width"     />
        <!-- Ширина пейджера -->
    <xsl:param name="LinkPathPreffix"  />
        <!-- Преффикс ссылки -->
    
    <!--
        У пейджера есть минимальный порог применимости.
        Равен он трем
        Если,  например Width=2,
        то на центральных шагах все равно будет
        отображаться 3 элемента.
    -->

    <xsl:choose>
        <xsl:when test="$Width &lt; $Total">
            <xsl:choose>
                <xsl:when test="($Current + floor($Width div 2)) &lt; ($Total -1)">
                    <xsl:choose>
                        <xsl:when test="$Current &lt; $Width">
                            <xsl:call-template name="simple-pager-find-current">
                                <xsl:with-param name="Stop"     select="$Width"/>
                                <xsl:with-param name="Target"   select="$Current"/>
                                <xsl:with-param name="LinkPathPreffix" select="$LinkPathPreffix"/>
                            </xsl:call-template>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:call-template name="simple-pager-item">
                                <xsl:with-param name="Link" select="concat($LinkPathPreffix, ($Current -  floor($Width div 2) - 1))"/>
                                <xsl:with-param name="Name" select="'...'"/>                    
                            </xsl:call-template>
                            <xsl:call-template name="simple-pager-find-current">
                                <xsl:with-param name="Start"    select="$Current - floor($Width div 2)"/>
                                <xsl:with-param name="Stop"     select="$Current + floor($Width div 2) + 1"/>
                                <xsl:with-param name="Target"   select="$Current"/>
                                <xsl:with-param name="LinkPathPreffix" select="$LinkPathPreffix"/>
                            </xsl:call-template>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:call-template name="simple-pager-item">
                        <xsl:with-param name="Link" select="concat($LinkPathPreffix, ($Total  -  $Width - 1))"/>
                        <xsl:with-param name="Name" select="'...'"/>                    
                    </xsl:call-template>
                    <xsl:call-template name="simple-pager-find-current">
                        <xsl:with-param name="Start"    select="$Total - $Width"/>
                        <xsl:with-param name="Stop"     select="$Total"/>
                        <xsl:with-param name="Target"   select="$Current"/>
                        <xsl:with-param name="LinkPathPreffix" select="$LinkPathPreffix"/>
                        <xsl:with-param name="WithoutTail" select="'True'"/>
                    </xsl:call-template>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:when>
        <xsl:otherwise>
            <xsl:call-template name="simple-pager-find-current">
                <xsl:with-param name="Stop"     select="$Total"/>
                <xsl:with-param name="Target"   select="$Current"/>
                <xsl:with-param name="LinkPathPreffix" select="$LinkPathPreffix"/>
                <xsl:with-param name="WithoutTail" select="'True'"/>
            </xsl:call-template>            
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

              
<xsl:template name="simple-pager-find-current">
    <xsl:param name="Counter"   select="0"/>
    <xsl:param name="Start"     select="0" />
    <xsl:param name="Stop" />
    <xsl:param name="Target" />
    <xsl:param name="LinkPathPreffix" />
    <xsl:param name="WithoutTail" select="''" />
    <xsl:choose>
        <xsl:when test="$Counter &lt; ($Stop - $Start)">
            <xsl:text> </xsl:text> 
            <xsl:choose>
                <xsl:when test="($Start + $Counter) = $Target">
                    <xsl:call-template name="simple-pager-selected-item">
                        <xsl:with-param name="Link" select="concat($LinkPathPreffix, $Start + $Counter)"/>
                        <xsl:with-param name="Name" select="$Start + $Counter + 1"/>                    
                    </xsl:call-template>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:call-template name="simple-pager-item">
                        <xsl:with-param name="Link" select="concat($LinkPathPreffix, $Start + $Counter)"/>
                        <xsl:with-param name="Name" select="$Start + $Counter + 1"/>                    
                    </xsl:call-template>
                </xsl:otherwise>
            </xsl:choose>
            <xsl:text> </xsl:text> 
            <xsl:call-template name="simple-pager-find-current">
                <xsl:with-param name="Counter" select="$Counter + 1"/>
                <xsl:with-param name="Start" select="$Start"/>
                <xsl:with-param name="Stop" select="$Stop"/>
                <xsl:with-param name="Target" select="$Target"/>
                <xsl:with-param name="LinkPathPreffix" select="$LinkPathPreffix"/>
                <xsl:with-param name="WithoutTail" select="$WithoutTail"/>
            </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
            <xsl:if test="not($WithoutTail)">
                <xsl:call-template name="simple-pager-item">
                    <xsl:with-param name="Link" select="concat($LinkPathPreffix, $Start + $Counter)"/>
                    <xsl:with-param name="Name" select="'...'"/>                    
                </xsl:call-template>        
            </xsl:if>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>    


<xsl:template name="simple-pager-item">
    <xsl:param name="Link" />
    <xsl:param name="Name" />
    <xsl:text> </xsl:text>
    <a href="{$Link}/">
        <xsl:value-of select="$Name" />
    </a>
    <xsl:text> </xsl:text>    
</xsl:template>


<xsl:template name="simple-pager-selected-item">
    <xsl:param name="Link"   select="0"/>
    <xsl:param name="Name"   select="0"/>
    <xsl:text> [</xsl:text>
    <a href="{$Link}/">
        <xsl:value-of select="$Name" />
    </a>
    <xsl:text>] </xsl:text>    
</xsl:template>



</xsl:stylesheet>
