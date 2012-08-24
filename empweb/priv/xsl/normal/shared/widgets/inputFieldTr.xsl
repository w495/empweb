<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<!--
    
    Пример использования:
    
    <form action="{$Action}" method="{$Method}" >
        <table>
            <xsl:call-template name="inputFieldTr">
                <xsl:with-param name="LabelName" select="'#'"  />
                <xsl:with-param name="Name" select="'id'"  />
                <xsl:with-param name="Value" select="'null'"  />
                <xsl:with-param name="Type" select="'hidden'"  />
            </xsl:call-template>
        <table>
    <form>
-->

<xsl:template name="inputFieldTr">
    <xsl:param name="LabelName" select="'Поле'" />
    <xsl:param name="Type" select="'text'"/>
    <xsl:param name="Name" select="''"/>
    <xsl:param name="Placeholder" select="'Введите текст'"/>
    <xsl:param name="Required" select="''"/>
    <xsl:param name="Size" select="'25'"/>
    <xsl:param name="Maxlength" select="'25'"/>
    <xsl:param name="Value" select="''"/>
    <xsl:param name="IsError" select="''"/>
    <xsl:param name="ErrorMess" select="''"/>
    <tr class="edit-user-line {$Type}" >
        <td align="right">
            <label class="edit-user-label">
                <xsl:if test="$Type != 'hidden'">
                    <xsl:value-of select="$LabelName" />
                    <xsl:if test="$Required">
                        <span style="color: red">*</span>
                    </xsl:if>
                    <xsl:text>:</xsl:text>
                </xsl:if>
            </label>
            <xsl:text>&nbsp;</xsl:text>
        </td>
        <td>
            <input type="{$Type}" placeholder="{$Placeholder}"
                size="{$Size}" maxlength="{$Maxlength}"
                name="{$Name}" id="{$Type}-{$Name}" class="edit-user-input"
            >
                <xsl:if test="$Required != ''">
                    <xsl:attribute name="required">required</xsl:attribute>
                </xsl:if>
                <xsl:if test="$Value != ''">
                    <xsl:attribute name="value">
                        <xsl:value-of select="$Value" />
                    </xsl:attribute>
                </xsl:if>
            </input>
            <xsl:if test="$IsError != ''">
                <div class="doc-body-form-line-error">
                    <xsl:value-of select="$ErrorMess" />
                </div>
            </xsl:if>
        </td>
    </tr>
</xsl:template>


</xsl:stylesheet>
