<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [
    <!ENTITY nbsp   "&#160;">
    <!ENTITY raquo  "&#187;">
    <!ENTITY laquo  "&#171;">
    <!ENTITY bull   "&#8226;">
]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="base.xsl"/>


<xsl:template name="s-title-base">
    <xsl:text>Документация: </xsl:text>
    <xsl:call-template name="s-title-docs"/>
</xsl:template>

<xsl:template name="s-title-docs">
</xsl:template>


<xsl:template name="s-signin">
    <xsl:call-template name="signin-form-mini">
        <xsl:with-param name="Action" select="concat('/signin/post', /data/meta/self-retpath)" />
        <xsl:with-param name="Method" select="'POST'"/>
        <xsl:with-param name="Has_errors" select="/data/meta/has-errors"/>
        <xsl:with-param name="Error_message" select="/data/meta/error-mess"/>
    </xsl:call-template>
</xsl:template>


<xsl:template name="s-main-base">
    <section class="s-roller">
        <xsl:call-template name="s-roller" />
    </section>
    <!--
    <section class="s-news">
        <xsl:call-template name="s-news" />
    </section>
    -->
    <section class="s-about">
        <xsl:call-template name="s-about" />
    </section>
</xsl:template>

<xsl:template name="s-roller">
    <div class="b-roller">
        <ul class="b-roller-frames">
            <li class="s-roller-frame">
                <xsl:call-template name="s-roller-frame">
                    <xsl:with-param name="Name" select="'docs'"/>
                    <xsl:with-param name="Head" select="'Кабинет рекламодателя'"/>
                    <xsl:with-param name="Subhead" select="''"/>
                    <xsl:with-param name="Pic_url" select="'/i/tv-1.png'"/>
                    <xsl:with-param name="Content" >
                        <xsl:call-template name="s-roller-frame-pre-roll" />
                    </xsl:with-param>
                </xsl:call-template>
            </li>
        </ul>
    </div>
</xsl:template>

<xsl:template name="s-roller-frame">
    <xsl:param name="Name" select="'name'"/>
    <xsl:param name="Head" select="'Видеореклама'"/>
    <xsl:param name="Subhead" select="'PRE-ROLL'"/>
    <xsl:param name="Content" select="'s'"/>
    <xsl:param name="Pic_url" select="'/s/'"/>
    <xsl:param name="Ct" select="'s-roller-frame-1'"/>
    <div class="b-rf-text" id="{$Name}">
        <hgroup class="b-rf-hg">
            <h1 class="b-rf-head">
                <xsl:value-of select="$Head" />
            </h1>
            <!--
                <h2 class="b-rf-caption">
                    <xsl:value-of select="$Subhead" />
                </h2>
            -->
        </hgroup>
        <div class="b-rf-content" >
            <xsl:copy-of select="$Content" />
        </div>
        <div class="b-rf-act" >
            <xsl:call-template name="u-button">
                <xsl:with-param name="Href"  select="'/'"/>
                <xsl:with-param name="Class" select="'b-rfa-bat'"/>
                <xsl:with-param name="Title" select="'разместить рекламу'"/>
                <xsl:with-param name="Text"  select="'Разместить рекламу'"/>
            </xsl:call-template>
        </div>
    </div>
    <div class="b-rf-picture">
        <img class="b-rfp" src="{$Pic_url}" alt="{$Head}:{$Subhead}" title="{$Head}:{$Subhead}"/>
    </div>
</xsl:template>

<xsl:template name="s-roller-frame-pre-roll">
    <ul class="b-rf-ul">
        <li class="e-rf-ul">
            <a href="/docs/audience" class="m-white-link">
                <xsl:text>Аудитория</xsl:text>
            </a>
        </li>
        <li class="e-rf-ul">
            <a href="/docs" class="m-white-link">
                <xsl:text>Размещение рекламы</xsl:text>
            </a>
        </li>
        <li class="e-rf-ul">
            <a href="/docs/offer" class="m-white-link">
                <xsl:text>Прайс-лист</xsl:text>
            </a>
        </li> 
        <li class="e-rf-ul">
            <a href="/docs/howto" class="m-white-link">
                <xsl:text>Руководство пользователя</xsl:text>
            </a>
        </li>
        <li class="e-rf-ul">
            <a href="/docs/contact" class="m-white-link">
                <xsl:text>Контакты</xsl:text>
            </a>
        </li>
    </ul>
</xsl:template>



<xsl:template name="s-about">
    <header class="b-docs-header">
        <h1 class="e-docs-head">
            <xsl:call-template name="e-docs-head" />
        </h1>
        <a class="b-docs-doc" href="/d/tvzavr_presentation.pptx" >
            <xsl:text>Презентация</xsl:text>
        </a>
        <a onclick="ContactUs()" class="b-news-doc b-news-mail" href="#">
            <xsl:text>Связаться с нами</xsl:text>
        </a>
    </header>
    <xsl:call-template name="s-about-docs" />
</xsl:template>

<xsl:template name="e-docs-head">
    <xsl:text>Размещение рекламы</xsl:text>
</xsl:template>


<xsl:template name="foot-scripts-base">
    <!--
        Скрипты добавляемые внизу страницы.
        Рекомендовано это использовать, если сами скрипты не меняют
        начальный вид страницы или ее DOM (до загрузки)
    -->
    <script src="/j/jm.js"><xsl:text><![CDATA[ ]]></xsl:text></script>
    <script src="/j/sl.js"><xsl:text><![CDATA[ ]]></xsl:text></script>
</xsl:template>

<!--
    ###########################################################################
    ### 
    ###########################################################################
-->

<!--
    TODO: Вынести в util
-->
<xsl:template name="u-button">
    <xsl:param name="Href" select="'Href'"/>
    <xsl:param name="Class" select="'but'"/>
    <xsl:param name="Title" select="'title'"/>
    <xsl:param name="Text" select="'text'"/>

    <a class="{$Class}"><xsl:text><![CDATA[ ]]></xsl:text></a>

    <!--
    <a class="{$Class} m-button" href="{$Href}" title="{$Title}">
        <xsl:value-of select="$Text" />
    </a>
    -->
    <!-- Alt:
        <form>
            <input type="button" />
        </form>
    -->
</xsl:template>


</xsl:stylesheet>

