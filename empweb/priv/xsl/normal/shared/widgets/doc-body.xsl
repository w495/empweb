<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<!--
    Шаблон тела документа.
    Имеет вид:
    ~~~~~~~~~~~~~~~~~~~~~~
     $DateTime
    ~~~~~~~~~~~~~~~~~~~~~~     
     $Title
    ~~~~~~~~~~~~~~~~~~~~~~
     | $Pic |  $Text ...
         ...
    [~~~~~~~~~~~~~~~~~~~~~~
     Вложения:
        $Attaches
    ~~~~~~~~~~~~~~~~~~~~~~]
-->

<xsl:include href="../utils/erlangFormatDate.xsl" />
<xsl:include href="../utils/tipograf.xsl" />
    
<xsl:template name="doc-body">
    <xsl:param name="Pic" />
    <xsl:param name="Title" />
    <xsl:param name="Text" />
    <xsl:param name="DateTime" />
    <xsl:param name="Attaches" />
    <xsl:param name="PrinterFlag" />
    <xsl:param name="GoBack" select="'#'"/>

    <!-- <xsl:if test="$Title">    -->
        <header>
            <h2 id="doc-body-header" class="second-header">
                <xsl:call-template name="tipograf">
                    <xsl:with-param  name="Input" select="$Title"/>
                </xsl:call-template>
            </h2>
            <div class="breakline">&nbsp;</div>
        </header>
    <!-- </xsl:if> -->
    
    <!--  <xsl:if test="$Text"> -->
        <section>
            <div id="doc-body-text">            
                <xsl:if test="$Pic != '' ">
                    <!-- Картинки может и не быть -->
                    <div id="doc-body-picture" >
                        <img src="/{$Pic}" alt="{$Title}" width="240" />
                    </div>
                </xsl:if>
                <xsl:call-template name="tipograf">
                    <xsl:with-param  name="Input" select="$Text"/>
                </xsl:call-template>
            </div>
        </section>
    <!-- </xsl:if> -->
    
    <xsl:if test="not($PrinterFlag)">
        <footer>
            <xsl:if test="$Attaches/item">
                <!-- Если нет вложений то вообще не показываем этот div-->
                <section>
                    <div id="doc-body-attaches">
                        <h3 id="doc-body-attaches-header" class="second-header">Вложения:</h3>
                        <ul id="doc-body-attaches-list">
                            <xsl:for-each select="$Attaches/item">
                                <li class="doc-body-attaches-item">
                                    <!--
                                        <xsl:value-of select="attach_type_id" />
                                        <xsl:text:>&nbsp;</xsl:text:>
                                    -->
                                    <!-- возможно надо добавить картиночку -->
                                    <a href="/{url}"
                                       type="application/file"
                                       title="{alt}"
                                       alt="{name}" >
                                        <xsl:value-of select="name" />
                                    </a>
                                    [<xsl:value-of select="size" />]
                                    <!--
                                        <xsl:text:>&nbsp;</xsl:text:>
                                        <xsl:value-of select="alt" />
                                    -->
                                </li>
                            </xsl:for-each>
                        </ul>
                    </div>
                </section>
            </xsl:if>
            <xsl:if test="$GoBack">
                <div id="doc-body-goback-holder" >
                    <div class="breakline">&nbsp;</div>
                    <span id="doc-body-goback" class="doc-body-border-bar relief">
                        <a href="{/data/meta/self-parent-path}"  onclick="history.back(); return false">
                            <xsl:text>вернуться</xsl:text>
                        </a>
                    </span>
                </div>
            </xsl:if>
        </footer>
    </xsl:if>
    
</xsl:template>

</xsl:stylesheet>
