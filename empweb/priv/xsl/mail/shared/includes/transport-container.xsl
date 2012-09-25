<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [
    <!ENTITY nbsp "&#160;">
    <!ENTITY raquo  "&#187;">
    <!ENTITY laquo  "&#171;">
]>


<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" >
    
<xsl:template name="transport-container">
    <ul id="transport-container-list">
        <li>
            <a href="{/data/meta/item-parent-path}Surface/" id="surface-transport" class="transport-type" title="Наземный транспорт">
                <span class="unvisible" >
                    <xsl:text disable-output-escaping="yes">Наземный транспорт</xsl:text>
                </span>
            </a>
        </li>
        <li>
            <a href="{/data/meta/item-parent-path}Air/" id="air-transport" class="transport-type" title="Воздушный транспорт">
                <span class="unvisible" >
                    <xsl:text disable-output-escaping="yes">Воздушный транспорт</xsl:text>
                </span>
            </a>
        </li>
        <li>
            <a href="{/data/meta/item-parent-path}Water/" id="water-transport" class="transport-type" title="Водный транспорт">
                <span class="unvisible" >
                    <xsl:text disable-output-escaping="yes">Водный транспорт</xsl:text>
                </span>
            </a>
        </li>
    </ul>
</xsl:template>



</xsl:stylesheet> 
