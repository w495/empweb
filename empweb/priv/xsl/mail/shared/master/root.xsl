<?xml version="1.0" encoding="utf-8"?>
<!--
    Это основной шаблон для вывода на экран монитора.

    # НАПОМИНАНИЕ:

        ## ФОРМАТИРОВАНИЕ:
            * все текстовые элементы должны помещаться в тег <xsl:text>

        ## ИМЕНОВАНИЕ ШАБЛОНОВ:
            * если шаблон описан в теле документа, то
                название шаблона должно совпадать
                с классом содержащим этот шаблон;
            * если описанный шаблон необходим в дочернем шаблоне,
                то но при этом, в текущем шаблоне необходимо содержать
                некоторую информацию, то к шаблону в дочернем элементе
                нужно добавить постфикс с числовым выражением
                уговня вложенности. Нулевой уровень не указывается.
                    `s-foo' ~~> `s-foo-1' ~~> `s-foo-2'.

        ## ИМЕНОВАНИЕ КЛАССОВ:
            * классы блочных элементов должны начинаться на с префикса b.

        ## ЗАПРЕЩЕНО:
            * использование атрибутов id.

-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">


<xsl:output
    method="text"
    encoding="utf-8"
/>

<xsl:include href="../widgets/text-table.xsl" />

<xsl:include href="../utils/erlangFormatDate.xsl" />

<xsl:template match="/" name="s-html">
    <!--
        Описание страницы
    -->
    <xsl:call-template name="s-body" />
    <xsl:text>&#xa;</xsl:text>
</xsl:template>

<xsl:template name="s-head">
</xsl:template>

<xsl:template name="s-body">
    <!--
        Тело документа.
    -->
    <xsl:call-template name="s-header" />
    <xsl:text>&#xa;</xsl:text>
    <xsl:call-template name="s-main" />
    <xsl:text>&#xa;</xsl:text>
    <xsl:call-template name="s-footer" />
</xsl:template>

<xsl:template name="s-header">
    <xsl:text>Здравствуйте, </xsl:text>
    <xsl:value-of select="/data/meta/username" />
    <xsl:text>!</xsl:text>
    <xsl:text>&#xa;</xsl:text>
</xsl:template>


<xsl:template name="s-main">
    <!--
        Основное содержимое документа.
    -->
    <xsl:call-template name="s-main-root" />
</xsl:template>

<xsl:template name="s-main-root">
    <!--
        Основное содержимое документа.
        Первый уровенгь вложенности.
    -->
    <xsl:call-template name="s-main-base" />
</xsl:template>


<xsl:template name="s-main-base">
    <!--
        Основное содержимое документа.
        Второй уровенгь вложенности.

        <xsl:call-template name="s-main-concrete" />
    -->
</xsl:template>


<xsl:template name="s-footer">
    <!--
        Подол страницы
    -->
    <xsl:text>С уважением,</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>администрация Tvzavr.</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>© 2011 OOO «ТиВиЗавр»</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>Все права защищены.</xsl:text>
</xsl:template>


</xsl:stylesheet>
