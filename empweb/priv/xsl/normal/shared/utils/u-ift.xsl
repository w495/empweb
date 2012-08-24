<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template name="u-ift">

    <!--
        INPUT FIELD TR
        Стили:
            .u-ift-line
            .u-ift-line-label
            .u-ift-line-input
            .u-ift-line-error
            .alert

        Пример использования:
            <form action="{$Action}" method="{$Method}" >
                <table>
                    <xsl:call-template name="inputFieldTr">
                        <xsl:with-param name="Label_name" select="'#'"  />
                        <xsl:with-param name="Name" select="'id'"  />
                        <xsl:with-param name="Value" select="'null'"  />
                        <xsl:with-param name="Type" select="'hidden'"  />
                    </xsl:call-template>
                <table>
            <form>
    -->

    <xsl:param name="Label_name" select="'Поле'" />
    <xsl:param name="Type" select="'text'"/>
    <xsl:param name="Name" select="''"/>
    <xsl:param name="Placeholder" select="'Введите текст'"/>
    <xsl:param name="Required" select="''"/>
    <xsl:param name="Size" select="'25'"/>
    <xsl:param name="Max_length" select="'25'"/>
    <xsl:param name="Value" select="''"/>
    <xsl:param name="Is_error" select="''"/>
    <xsl:param name="Error_message" select="''"/>
    <tr class="u-ift-line {$Type}" >
        <td align="right">
            <label class="u-ift-line-label">
                <xsl:if test="$Type != 'hidden'">
                    <xsl:value-of select="$Label_name" />
                    <xsl:if test="$Required">
                        <xsl:call-template name="u-ift-rstar"/>
                    </xsl:if>
                    <xsl:text>:</xsl:text>
                </xsl:if>
            </label>
            <xsl:text>&nbsp;</xsl:text>
        </td>
        <td>
            <input type="{$Type}" placeholder="{$Placeholder}"
                size="{$Size}" maxlength="{$Max_length}"
                name="{$Name}" class="u-ift-line-input {$Type}-{$Name}"
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
            <xsl:if test="$Is_error != ''">
                <div class="u-ift-line-error">
                    <xsl:value-of select="$Error_message" />
                </div>
            </xsl:if>
        </td>
    </tr>
</xsl:template>

<xsl:template name="u-ift-rstar">
    <span class="u-ift-rstar">*</span>
</xsl:template>

</xsl:stylesheet>
