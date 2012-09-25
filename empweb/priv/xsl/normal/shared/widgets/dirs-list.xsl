<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:include href="../../shared/utils/tipograf.xsl" />

<xsl:template name="dirs-attaches">
    <xsl:param name="Attaches" />
    <xsl:param name="PrinterFlag" />
    <xsl:param name="GoBack" select="'#'"/>
    
    <xsl:if test="not($PrinterFlag)">
        <xsl:if test="$Attaches/item">
            <section>
                <div id="doc-body-attaches">
                    <h3 id="doc-body-attaches-header" class="second-header">Вложения:</h3>
                    <ul id="doc-body-attaches-list">
                        <xsl:for-each select="$Attaches/item">
                            <li class="doc-body-attaches-item">
                                <a href="/{url}"
                                   type="application/file"
                                   title="{alt}"
                                   alt="{name}" >
                                    <xsl:value-of select="name" />
                                </a>
                                [<xsl:value-of select="size" />]
                            </li>
                        </xsl:for-each>
                    </ul>
                </div>
            </section>
        </xsl:if>
        <footer>
            <div id="doc-body-goback-holder" >
                <div class="breakline">&nbsp;</div>
                <span id="doc-body-goback" class="doc-body-border-bar relief">
                    <a href="{/data/meta/self-parent-path}"  onclick="history.back(); return false">
                        <xsl:text>вернуться</xsl:text>
                    </a>
                </span>
            </div>
        </footer>
    </xsl:if>
</xsl:template>


<xsl:template name="dirs-list">
    <xsl:param name="Item"  select="/data/dirs/item" />
    <xsl:param name="DirPath" select="/TEST" />
    <xsl:if test="$Item">
        <section>
            <h3>Разделы:</h3>
            <ul>
                <xsl:for-each select="$Item">        
                    <li>
                        <a href="{$DirPath}/{id}/">
                            <xsl:call-template name="tipograf">
                                <xsl:with-param  name="Input" select="name"/>
                            </xsl:call-template>
                        </a>
                    </li> 
                </xsl:for-each>
            </ul>
        </section>
    </xsl:if>
</xsl:template>


<xsl:template name="docs-list">
    <xsl:param name="Item"  select="/data/docs/item" />
    <xsl:param name="DocPath" select="/TEST" />
    <xsl:if test="$Item">
        <section>
            <h3>Документы:</h3>
            <ul>
                <xsl:for-each select="$Item">
                    <li>
                        <a href="{$DocPath}/{id}/">
                            <xsl:call-template name="tipograf">
                                <xsl:with-param  name="Input" select="name"/>
                            </xsl:call-template>
                        </a>
                    </li> 
                </xsl:for-each>
            </ul>
        </section>
    </xsl:if>
</xsl:template>



</xsl:stylesheet>
