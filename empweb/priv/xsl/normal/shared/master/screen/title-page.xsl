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
	<xsl:text>Главная</xsl:text>
</xsl:template>



<xsl:template name="b-thehead-logo">
    <img class="b-thehead-logo" src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAANAAAAAvCAMAAAClvrSKAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJ
bWFnZVJlYWR5ccllPAAAAyBpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuMC1jMDYwIDYxLjEzNDc3NywgMjAxMC8wMi8xMi0xNzozMjowMCAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENTNSBXaW5kb3dzIiB4bXBNTTpJbnN0YW5jZUlEPSJ4bXAuaWlkOjE0QjhDN0YzNUJDMzExRTE4N0Y0RUVDOENDQzA1MjZDIiB4bXBNTTpEb2N1bWVu
dElEPSJ4bXAuZGlkOjE0QjhDN0Y0NUJDMzExRTE4N0Y0RUVDOENDQzA1MjZDIj4gPHhtcE1NOkRlcml2ZWRGcm9tIHN0UmVmOmluc3RhbmNlSUQ9InhtcC5paWQ6MTRCOEM3RjE1QkMzMTFFMTg3RjRFRUM4Q0NDMDUyNkMiIHN0UmVmOmRvY3VtZW50SUQ9InhtcC5kaWQ6MTRCOEM3RjI1QkMzMTFFMTg3RjRFRUM4Q0NDMDUyNkMiLz4gPC9yZGY6RGVzY3JpcHRpb24+IDwvcmRmOlJERj4gPC94OnhtcG1ldGE+IDw/eHBhY2tldCBlbmQ9InIiPz75+DnpAAADAFBMVEUXW6oamWQ1gsQAhMoAesPY686v1u1Vp12014ykz35ztG3m8uZAom4DjUOy2bGezHc1qIys04VktIrK5tTJ5LONw2eJxHBcrkVksUAXp+AkoXpwyfGFwj/M2OtYvuyAunbv9/Mqo4FltnYAdL1ssWiW2viQsNe725Req2AAhT/k8dcnreMHndoea7X1+vSIw0pBicdXsXOVx29CwvNOqkFos0x+0PO/5vchdLvb7tt8wJm94McQkkcwn0FkrWSezmlrk8cKot2H1PUrUKN7ullqtUGKxFap1ZtwpNMgZbCPxuap03oqmmg9ZbE7o0BApNoRVKZ/yewZlUkUlVJ0ukAcnXASk0slm0DJ5MZHrHRBoVXb6fUZmFwgQJpIeLxUrEEsm04AjNAAZbOl0m9RhMOGwolsueKWylY6suaPm8sIkEeOyamXy4jQ5/QAbrlGpkAjfsI2nlF0uUuLyJd6vFLx9PmSy6Vis1smY69MpFmazZV8u3MTS6AGl9cjmEtAnNMbX62Ev2AsnUGTyWOg0rRLuOpyt1IuabN9vkni6/RAk8sQldcAru9CpUsYlkB6t3E2oUBfyvU6cLe/388bRZ0ood1isUl7vUBQqnsQdb0Ak9nf8fmAvloBj9UfZLALnd4SlVgMklMgiMhCtuksr+dAuetyu2wRjlXe7uASs/ABkdObzWPB4J+x2sInufEdmEFTrE+ou9yGvntwtk5AsuUUarU8o0ujzOdvtXYqe78gk1YQjUpoernv9+if1O53uk+dznyWynAIZLFPsIQgmNZ2h78Ji00wkMtBvvCPx31ArODf8OdxuX8wm9TA3798u39HqExNfr9ApEAAoeQjPJghl0t/v0AYllAAarZkw++Mxj9ca7HP7/sAm99MqE0AYK9KplSbz6wKsfADkdQwvfKSxmiYyWw4ol5br0wQj9Kn3/gQh8tFpmCQyEsZeb8RouDg8NsYl1kgmVUfr+mlwN63xeDw+PxQodQ3XauYvN40n0gAqOpMYatEb7cuZK/////8Qx/cAAABAHRSTlP///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////8AU/cHJQAACYVJREFUeNrcmntUFNcdx5fFVRQWi6C7RImIFlhjdkEBKzVqwF2MSpG69dUobldFHtao+AQSEbGoWJNIQHkIlIj1QUPAqIkhxCRqqIZoW7dtFKtJ1dbGQkIl0TSZ3Hnd+7tzBzAn5/Sc8vlDZr6/3/3d+537mDmChsNcHX+V+/9HI/7ovDOuoKBgXK8xFHOooKAXGbr694JD42JihvYWQ1cPffzCHZ7f9Q5DnYc+jvqhQC8x9EKB5KeXGCr96v0YiY73e4OhqK+GRkn8rww1+h11Oj9CJDs9/Bq/Z7HRIUKpj+46xVKaAx1DZTo6ojDjuTvSVcx4pogcIS/ixhDn3WEsd50hjYybI0IkmceCrpL9zDh4VGrn9Bit7HOGGEmeoaPsoGLJTsSRZBT0MPOGXv6rjP8fCVHcAXzdEUPX7sSRA514IJbkySz8iEMoOx4oM9fpEyLhMeOIZZjFTw7ryLNwKp5EMo54YM2MXE72kGs5LcOOmDlN9cuPquAfxUW1tuXxl3nLn2wdStW+09pWzUeq21qrBSHEZZnhoc4Mi2syaelncdmYTCdKkSZptMslhH2cNpeFmiSzyzXZh69nc7nkgDnXZaMqWVwenObYY3kq+CeiYeOIfyu17BJbl4t6dUUrv+p0xRafrrEU4wnw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
YII=" alt="tvzavr" title="tvzavr" />
</xsl:template>


<xsl:template name="s-main-base">
    <section class="s-roller">
        <xsl:call-template name="s-roller" />
    </section>
    <section class="s-news">
        <xsl:call-template name="s-news" />
    </section>
    <section class="s-about">
        <xsl:call-template name="s-about" />
    </section>
</xsl:template>

<xsl:template name="s-roller">
    <div class="b-roller">
        <ul class="b-roller-frames">
            <li class="s-roller-frame">
                <xsl:call-template name="s-roller-frame">
                    <xsl:with-param name="Name" select="'pre-roll'"/>
                    <xsl:with-param name="Head" select="'Кабинет рекламодателя'"/>
                    <xsl:with-param name="Subhead" select="''"/>
                    <xsl:with-param name="Pic_url" select="'/i/tv-1.png'"/>
                    <xsl:with-param name="Content" >
                        <xsl:call-template name="s-roller-frame-pre-roll" />
                    </xsl:with-param>
                </xsl:call-template>
            </li> 
        </ul>
        <!--
	        <div class="b-roller-nav">
	            <ul class="b-rn-ul">
	                <li class="e-rn-ul">
	                    <a class="e-rn-but cur" href="#pre-roll">
	                        <xsl:text><![CDATA[ ]]></xsl:text>
	                    </a>
	                </li>
	                <li class="e-rn-ul">
	                    <a class="e-rn-but" href="#mid-roll">
	                        <xsl:text><![CDATA[ ]]></xsl:text>
	                    </a>
	                </li>
	                <li class="e-rn-ul">
	                    <a class="e-rn-but" href="#post-roll">
	                        <xsl:text><![CDATA[ ]]></xsl:text>
	                    </a>
	                </li>
	            </ul>
	        </div>
    	-->
    </div>
</xsl:template>

<xsl:template name="s-roller-frame">
    <xsl:param name="Name" select="'name'"/>
    <xsl:param name="Head" select="'Кабинет рекламодателя'"/>
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



<xsl:template name="s-news">
    <header class="b-news-header">
        <h1 class="e-news-head">
            <xsl:text>О проекте</xsl:text>
        </h1> 
        <a class="b-docs-doc" href="/d/tvzavr_presentation.pptx" >
            <xsl:text>Презентация</xsl:text>
        </a>
        <a class="b-news-doc b-news-mail" href="#">
            <xsl:text>Связаться с нами</xsl:text>
        </a>
    </header>
    <!--
	    <div class="b-news">
	        <ul class="s-news-list">
	            <xsl:call-template name="s-news-list" />
	        </ul>
	        <div class="s-news-all"> 
	            <xsl:call-template name="u-button">
	                <xsl:with-param name="Href"  select="'/'"/>
	                <xsl:with-param name="Class" select="'b-na-bat'"/>
	                <xsl:with-param name="Title" select="'посмотреть все новости'"/>
	                <xsl:with-param name="Text"  select="'Посмотреть все'"/>
	            </xsl:call-template> 
	        </div>
	    </div>
    -->
</xsl:template>

<xsl:template name="s-news-list">
<!-- <xsl:for> -->
        <li class="s-news-list-item">
            <xsl:call-template name="s-news-list-item">
                <xsl:with-param name="Caption" select="''" />
                <xsl:with-param name="Pic_url" select="'/i/acv-1.jpg'" />
            </xsl:call-template>
        </li>
        <li class="s-news-list-item">
            <xsl:call-template name="s-news-list-item">
                <xsl:with-param name="Caption" select="''" />
                <xsl:with-param name="Pic_url" select="'/i/acv-2.jpg'" />
            </xsl:call-template>
        </li>
        <li class="s-news-list-item">
            <xsl:call-template name="s-news-list-item">
                <xsl:with-param name="Caption" select="''" />
                <xsl:with-param name="Pic_url" select="'/i/acv-3.jpg'" />
            </xsl:call-template>
        </li>
<!-- </xsl:for> -->
</xsl:template>

<xsl:template name="s-news-list-item">
    <xsl:param name="Id" select="'id'"/>
    <xsl:param name="Link" select="'/1/'"/>
    <xsl:param name="Pic_url" select="'Pic_url'"/>
    <xsl:param name="Caption"  select="'default'" />
    <xsl:param name="Alt" select="'default'" />
    <xsl:param name="Title"  select="'default'" />
    <div class="b-nlil" href="{$Link}" title="{$Caption}">
        <figure class="b-nli">
            <img
                class="b-nliim"
                src="{$Pic_url}"
                alt="{$Caption}"
                title="{$Caption}"
            />
            <figcaption class="b-nlic">
                <xsl:value-of select="$Caption"/>
            </figcaption>
        </figure>
    </div>
</xsl:template>

<xsl:template name="s-about">
    <article class="b-a"> 
        <div class="b-ac">
            <p class="b-ac-p" > 
                <xsl:text>TVzavr.ru – Интернет-кинотеатр, осуществляющий бесплатную онлайн-трансляцию лицензионных кино и видеофильмов </xsl:text>
                <br /> 
                <xsl:text>Интернет-портал TVzavr.ru создан в апреле 2010 года и принадлежит  ООО «Ти Ви Завр»</xsl:text>
            </p> 
            <br />
            <h2 class="b-ah">
                <xsl:text>Устройства доступа</xsl:text>
            </h2>
            <ul class="b-ac-ul">
                <li class="e-ac-ul">
                    <xsl:text>Персональный компьютер</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text> Мобильные устройства ( приложения для iOS и Android )</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Smart TV</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Set-top box</xsl:text>
                </li>
            </ul> 
            <p class="m-center">
                <img src="/i/ipad.jpg" />
                <img src="/i/smarttv.jpg" />
            </p>
            <ul class="b-ac-ul">
                <li class="e-ac-ul">
                    <xsl:text>Возможность поделиться ссылкой фильма во всех популярных соцсетях:</xsl:text>
                    <span class="m-social">
                        <img src="/i/social/facebook.png" />
                        <xsl:text> </xsl:text>
                        <img src="/i/social/livejournal.png" />
                        <xsl:text> </xsl:text>
                        <img src="/i/social/mail.png" />
                        <xsl:text> </xsl:text>
                        <img src="/i/social/twitter.png" />
                        <xsl:text> </xsl:text>
                        <img src="/i/social/vkontakte.png" />
                    </span>
                </li>
            </ul>  
        </div>
    </article>
    
    <article class="b-a">  
        <div class="b-ac">  
	        <h2 class="b-ah">
	            <xsl:text>Конкурентные преимущества TVzavr.ru</xsl:text>
	        </h2> 
            <ol class="b-ac-ol">
                <li class="e-ac-ul">
                    <xsl:text>Техническая платформа, разработанная специально для видеохостинга, обеспечивающая высокое качество воспроизведения видео без последующих подгрузок</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Возможность демонстрации фильмов в формате HD и full HD.</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Доступность просмотра ряда фильмов в оригинальной версии с субтитрами.</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Широкий региональный охват, доступность во всех населенных пунктах РФ</xsl:text>
                </li>
                <li class="e-ac-ul">
                    <xsl:text>Регулярное взаимодействие с аудиторией портала – проведение конкурсов и викторин с </xsl:text>
                    <xsl:text>розыгрышами призов, акций, offline мероприятий, анонсирование самого интересного и нового рассылкой и т.п.</xsl:text>
                </li>  
            </ol>  
		</div>
    </article> 
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
