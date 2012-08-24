<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template name="erlangFormatDateTime">
    <!--
        Формирует дату в формате YYYY.MM.DD HH:mm
            из формата представления в erlang.
        
        DateTime исходная erlang-дата.
        Поясненеие:
            изначально erlang возвращает дату без ведущих нулей:
                2011 1 1 1 1 1.
            Это будет означать:
                2011.01.01 01:01:01.
        В данном случае, конвертировать в стандартное представление даты можно
        и средствами erlang, но можно делать подобные вещи и на уровне предаста-
        вления. Потому мы используем для этого xsl-преобразование.
    -->
    <xsl:param name="DateTime" />
    
    <!-- Year -->
    <xsl:value-of select="substring($DateTime,1,4)" />
    <xsl:text>.</xsl:text>

    <xsl:variable name="AfterYear"> 
        <xsl:value-of select="substring-after($DateTime, ' ')" />
    </xsl:variable>
    
    <!-- Month -->
    <xsl:call-template name="getTwoDigits">
          <xsl:with-param name="String" select="$AfterYear"/>
    </xsl:call-template>
    <xsl:text>.</xsl:text>
        
    <xsl:variable name="AfterMonth">
        <xsl:value-of select="substring-after($AfterYear, ' ')" />
    </xsl:variable>

    <!-- Day -->
    <xsl:call-template name="getTwoDigits">
          <xsl:with-param name="String" select="$AfterMonth"/>
    </xsl:call-template>
    <xsl:text>&nbsp;</xsl:text>
    
    <xsl:variable name="AfterDay">
        <xsl:value-of select="substring-after($AfterMonth, ' ')" />
    </xsl:variable>
    <!-- Hour -->
    <xsl:call-template name="getTwoDigits">
          <xsl:with-param name="String" select="$AfterDay"/>
    </xsl:call-template>
    <xsl:text>:</xsl:text>

    <xsl:variable name="AfterHour">
        <xsl:value-of select="substring-after($AfterDay, ' ')" />
    </xsl:variable>

    <!-- Minute -->    
    <xsl:call-template name="getTwoDigits">
          <xsl:with-param name="String" select="$AfterHour"/>
    </xsl:call-template>

</xsl:template>

<xsl:template name="erlangFormatDate">
    <!--
        Формирует дату в формате YYYY.MM.DD
            из формата представления в erlang.
        
        DateTime исходная erlang-дата.
        Поясненеие:
            изначально erlang возвращает дату без ведущих нулей:
                2011 1 1.
            Это будет означать:
                2011.01.01.
        В данном случае, конвертировать в стандартное представление даты можно
        и средствами erlang, но можно делать подобные вещи и на уровне предаста-
        вления. Потому мы используем для этого xsl-преобразование.
    -->
    <xsl:param name="DateTime" />
    <!-- Year -->
    <xsl:value-of select="substring($DateTime,1,4)" />
    <xsl:text>.</xsl:text>
    <xsl:variable name="AfterYear"> 
        <xsl:value-of select="substring-after($DateTime, ' ')" />
    </xsl:variable>
    <!-- Month -->
    <xsl:call-template name="getTwoDigits">
          <xsl:with-param name="String" select="$AfterYear"/>
    </xsl:call-template>
    <xsl:text>.</xsl:text>
    <xsl:variable name="AfterMonth">
        <xsl:value-of select="substring-after($AfterYear, ' ')" />
    </xsl:variable>
    <!-- Day -->
    <xsl:call-template name="getTwoDigits">
          <xsl:with-param name="String" select="$AfterMonth"/>
    </xsl:call-template>
</xsl:template>

<xsl:template name="erlangFormatPubdate">
        <!--
            Формирует дату в формате YYYY-MM-DDTHH:mmZ
                из формата представления в erlang.
            
            DateTime исходная erlang-дата.
            Поясненеие:
                изначально erlang возвращает дату без ведущих нулей:
                    2011 1 1 1 1 1.
                Это будет означать:
                    2011-01-01T01:01:01Z.
                    
            Z - указание временного пояса.
            
                [-12:00; +14:00]
                UTC - Universal Coordinated Time
                MSD - Moscow Daylight Time [UTC/GMT +4 hours]
        -->
    <xsl:param name="DateTime" />
    <xsl:param name="TimeZone" />
    
    <!-- Year -->
    <xsl:value-of select="substring($DateTime,1,4)" />
    <xsl:text>-</xsl:text>

    <xsl:variable name="AfterYear"> 
        <xsl:value-of select="substring-after($DateTime, ' ')" />
    </xsl:variable>
    
    <!-- Month -->
    <xsl:call-template name="getTwoDigits">
          <xsl:with-param name="String" select="$AfterYear"/>
    </xsl:call-template>
    <xsl:text>-</xsl:text>
        
    <xsl:variable name="AfterMonth">
        <xsl:value-of select="substring-after($AfterYear, ' ')" />
    </xsl:variable>

    <!-- Day -->
    <xsl:call-template name="getTwoDigits">
          <xsl:with-param name="String" select="$AfterMonth"/>
    </xsl:call-template>
    <xsl:text>T</xsl:text>
    
    <xsl:variable name="AfterDay">
        <xsl:value-of select="substring-after($AfterMonth, ' ')" />
    </xsl:variable>
    <!-- Hour -->
    <xsl:call-template name="getTwoDigits">
          <xsl:with-param name="String" select="$AfterDay"/>
    </xsl:call-template>
    <xsl:text>:</xsl:text>

    <xsl:variable name="AfterHour">
        <xsl:value-of select="substring-after($AfterDay, ' ')" />
    </xsl:variable>

    <!-- Minute -->    
    <xsl:call-template name="getTwoDigits">
          <xsl:with-param name="String" select="$AfterHour"/>
    </xsl:call-template>
    
    <xsl:value-of select="$TimeZone" />
</xsl:template>


<xsl:template name="getTwoDigits">
    <!--
        Откусывает от входной строки String подстроку до первого пробела.
        Приводит эту подстроку к виду CC.
    -->
    <xsl:param name="String" />
    <xsl:variable name="Before">
        <xsl:value-of select="substring-before($String, ' ')" />
    </xsl:variable>
    <xsl:call-template name="getNullOrOne">
          <xsl:with-param name="Val" select="$Before"/>
    </xsl:call-template>
</xsl:template>


<xsl:template name="getNullOrOne">
    <xsl:param name="Val" />
    <!--
        Возвращает строку вида СС.
        Если входная строка Val
            уже имеет вид СС, то она сама и возвращается.
        Если входная строка Val
            имеет вид С, то возвращается 0C.
    -->
    <xsl:choose>
        <xsl:when test="string-length($Val) = 2">
            <xsl:value-of select="$Val" />
        </xsl:when>
        <xsl:when test="string-length($Val) = 1">
            <xsl:text>0</xsl:text><xsl:value-of select="$Val" />
        </xsl:when>
        <xsl:otherwise>
            <xsl:text>ERROR</xsl:text>
        </xsl:otherwise>
    </xsl:choose>    
</xsl:template>



</xsl:stylesheet>
