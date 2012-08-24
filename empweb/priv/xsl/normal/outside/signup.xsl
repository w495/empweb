<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:import href="../shared/master/screen/common-page.xsl"/>

<xsl:include href="../shared/utils/tipograf.xsl" />


<xsl:template name="s-title-common">
    <xsl:text>регистрация</xsl:text>
</xsl:template>

<xsl:template name="s-main-common">
    <section class="b-signup">
        <h3 class="b-signup-h">
            <xsl:text>Регистрация рекламодателя</xsl:text>
        </h3>
        <div class="s-signup">
            <xsl:call-template name="s-signup" />
        </div>
    </section>
</xsl:template>

<xsl:template name="foot-scripts-common">
    <script src="/j/jm.js"><xsl:text><![CDATA[ ]]></xsl:text></script>
    <script src="/j/sl.js"><xsl:text><![CDATA[ ]]></xsl:text></script>
    <script src="/j/reg.js"><xsl:text><![CDATA[ ]]></xsl:text></script>
</xsl:template>

<xsl:template name="u-csif-rs">
    <span class="u-csif-rstar">*</span>
</xsl:template>


<xsl:template name="u-csif">
    <xsl:param name="Label_name" select="'Поле'" />
    <xsl:param name="Type" select="'text'"/>
    <xsl:param name="Name" select="''"/>
    <xsl:param name="Placeholder" select="'Введите текст'"/>
    <xsl:param name="Required" select="''"/>
    <xsl:param name="Size" select="'25'"/>
    <xsl:param name="Auto_complete" select="'off'"/>
    <xsl:param name="Pattern" select="''"/>
    <xsl:param name="Max_length" select="'41'"/>
    <xsl:param name="Is_error" select="''"/>
    <xsl:param name="Error_type" select="''"/>
    <xsl:param name="Error_message" select="'поле введено не верно'"/>
    <xsl:param name="Error_data" select="''"/>


    <xsl:param name="Error_value" select="$Error_data/*[name()=$Name]"/>


    <div class="b-csif">
        <label class="e-csif-fl m-{$Name}">
            <xsl:if test="$Type != 'hidden'">
                <xsl:value-of select="$Label_name" />
                <xsl:if test="$Required">
                    <xsl:call-template name="u-csif-rs"/>
                </xsl:if>
            </xsl:if>
        </label>
        <input
            type="{$Type}"
            placeholder="{$Placeholder}"
            size="{$Size}"
            maxlength="{$Max_length}"
            autocomplete="{$Auto_complete}" 
            name="{$Name}"
            class="e-csif-fi {$Type}-{$Name}"
        >
            <xsl:if test="$Required != ''">
                <xsl:attribute name="required">required</xsl:attribute>
            </xsl:if>
            <xsl:if test="$Error_value != ''">
                <xsl:attribute name="value">
                    <xsl:value-of select="$Error_value" />
                </xsl:attribute>
            </xsl:if>
            <xsl:if test="$Error_value != ''">
                <xsl:attribute name="value">
                    <xsl:value-of select="$Error_value" />
                </xsl:attribute>
            </xsl:if>
            <xsl:if test="$Pattern != ''">
                <xsl:attribute name="pattern">
                    <xsl:value-of select="$Pattern" disable-output-escaping="yes" />
                </xsl:attribute>
            </xsl:if>
        </input>
        <div class="e-csif-fm">
            <xsl:choose>
                <xsl:when test="$Error_type = $Name">
                    <div class="e-csif-w m-{$Error_type}"
                        title="{$Error_message}">&nbsp;</div>
                </xsl:when>
                <xsl:when test="$Error_type != $Name">
                    <div class="e-csif-r">&nbsp;</div>
                </xsl:when>
                <xsl:otherwise>&nbsp;</xsl:otherwise>
            </xsl:choose>
        </div>
    </div>
</xsl:template>

<xsl:template name="signup-form2">
    <xsl:param name="Action" select="'/signup_post'" />
    <xsl:param name="Method" select="'POST'"/>
    <xsl:param name="Error_type" select="/data/meta/error/type"/>
    <xsl:param name="Error_hint" select="/data/meta/error/hint"/>
    <xsl:param name="Error_spec" select="/data/meta/error/spec"/>
    <xsl:param name="Error_data" select="/data/meta/error/data"/>

    <xsl:if test="$Error_type != ''">
        <div class="signup-form-alert">
            <span>
                <xsl:text> Есть ошибки: </xsl:text>
            </span>
            <xsl:choose>
                <xsl:when test="$Error_type = 'email'">
                    <xsl:text>пользователь с таким E-mail уже существует</xsl:text>
                </xsl:when>
                <xsl:when test="$Error_type = 'login'">
                    <xsl:text>пользователь с таким логином уже существует</xsl:text>
                </xsl:when>
                <xsl:when test="$Error_type = 'captcha'">
                    <xsl:text>капча введена не верно</xsl:text>
                </xsl:when>
                <xsl:when test="$Error_type = 'password'">
                    <xsl:choose>
                        <xsl:when test="$Error_spec = 'type'">
                            <xsl:text>неверная длинна пароля</xsl:text>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:text>пароли не совпадают</xsl:text>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:when>
                <xsl:when test="$Error_type = 'captcha'">
                    <xsl:text>капча введена не верно</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$Error_type" />
                </xsl:otherwise>
            </xsl:choose>
        </div>
    </xsl:if>

    <form action="{$Action}" method="{$Method}" >
        <div>
            <div class="b-csif-c m-1">
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Логин'"  />
                    <xsl:with-param name="Name" select="'login'"  />
                    <xsl:with-param name="Pattern" select="'[A-Za-z]{1,}'"/>
                    <xsl:with-param name="Placeholder" select="'только латинские буквы'"  />
                    <xsl:with-param name="Required" select="'true'"/>
                    <xsl:with-param name="Size" select="'1024'"/>
                    <xsl:with-param name="Max_length" select="'1024'"/>
                    <xsl:with-param name="Error_message" select="'пользователь с таким логином уже существует'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Error_data" select="$Error_data"/>
                </xsl:call-template>
                <!--
                     Класс password-password-c добавится автоматически к полю повторного ввода пароля
                     К полю первичного ввода пароля добавится  класс password-password
                -->
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Пароль'" />
                    <xsl:with-param name="Name" select="'password'"  />
                    <xsl:with-param name="Placeholder" select="'не короче 6 символов'"  />
                    <xsl:with-param name="Type" select="'password'"  />
                    <xsl:with-param name="Required" select="'true'"/>
                    <xsl:with-param name="Size" select="'128'"/>
                    <xsl:with-param name="Pattern" select="'[^а-яА-Я]{6,}'"/>
                    <xsl:with-param name="Max_length" select="'128'"/>
                    <xsl:with-param name="Error_message" select="'пароли не совпадают'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Error_data" select="$Error_data"/>
                </xsl:call-template>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Повторите пароль'" />
                    <xsl:with-param name="Name" select="'password-c'"  />
                    <xsl:with-param name="Placeholder" select="'не короче 6 символов'"  />
                    <xsl:with-param name="Type" select="'password'"  />
                    <xsl:with-param name="Required" select="'true'"/>
                    <xsl:with-param name="Size" select="'128'"/>
                    <xsl:with-param name="Pattern" select="'[^а-яА-Я]{6,}'"/>
                    <xsl:with-param name="Max_length" select="'128'"/>
                    <xsl:with-param name="Error_message" select="'пароли не совпадают'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Error_data" select="$Error_data"/>
                </xsl:call-template>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Электронная почта'" />
                    <xsl:with-param name="Name" select="'email'"  />
                    <xsl:with-param name="Placeholder" select="'mail@example.com'"  />
                    <xsl:with-param name="Required" select="'true'"/>
                    <xsl:with-param name="Size" select="'1024'"/>
                    <xsl:with-param name="Type" select="'text'"/>
                    <xsl:with-param name="Max_length" select="'1024'"/>
                    <xsl:with-param name="Error_message" select="'пользователь с таким E-mail уже существует'"/>
                    <xsl:with-param name="Pattern">
                        <xsl:text disable-output-escaping="yes"><![CDATA[([\w\!\#$\%\'\*\+\-\/\=\?\^\`{\|\}\~]+\.)*[\w\!\#$\%\'\*\+\-\/\=\?\^\`{\|\}\~]+@((((([a-z0-9]{1}[a-z0-9\-]{0,62}[a-z0-9]{1})|[a-z])\.)+[a-z]{2,6})|(\d{1,3}\.){3}\d{1,3}(\:\d{1,5})?)]]></xsl:text>
                    </xsl:with-param>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Error_data" select="$Error_data"/>
                </xsl:call-template>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Город'" />
                    <xsl:with-param name="Name" select="'city'"  />
                    <xsl:with-param name="Placeholder" select="'название города'"  />
                    <xsl:with-param name="Size" select="'1024'"/>
                    <xsl:with-param name="Max_length" select="'1024'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Error_data" select="$Error_data"/>
                </xsl:call-template>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Телефон'" />
                    <xsl:with-param name="Name" select="'telephone_number'" />
                    <xsl:with-param name="Required" select="'true'"/>
                    <xsl:with-param name="Pattern" select="'\+[0-9]{5,}'"/>
                    <xsl:with-param name="Placeholder" select="'+01234567890'" />
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Error_data" select="$Error_data"/>
                </xsl:call-template>
            </div>
            <div class="b-csif-c m-2">
                <xsl:variable name="Checked" select="$Error_data/*[name()='legal_type']"/>
                <div class="b-csif" >
                    <label class="e-csif-fl m-organization">Вы представляете</label>
                    <select name="legal_type" class="e-csif-fi legal-type"> 
                        <xsl:choose>
                            <xsl:when test="$Checked = 'false'">
                                <option value="false" selected="selected">Физическое лицо</option>
                                <option value="true">Юридическое лицо</option>
                            </xsl:when>
                            <xsl:when test="$Checked = 'true'">
                                <option value="false">Физическое лицо</option>
                                <option value="true" selected="selected">Юридическое лицо</option>
                            </xsl:when>
                            <xsl:otherwise>
                                <option value="false">Физическое лицо</option>
                                <option value="true">Юридическое лицо</option>
                            </xsl:otherwise>
                        </xsl:choose>                 
                    </select>
                </div>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Организация'" />
                    <xsl:with-param name="Name" select="'organization'"  />
                    <xsl:with-param name="Size" select="'1024'"/>
                    <xsl:with-param name="Max_length" select="'1024'"/>
                    <xsl:with-param name="Required" select="'true'"/>
                    <xsl:with-param name="Placeholder" select="'Введите организацию'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Error_data" select="$Error_data"/>
                </xsl:call-template>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Должность'" />
                    <xsl:with-param name="Name" select="'position'"  />
                    <xsl:with-param name="Size" select="'1024'"/>
                    <xsl:with-param name="Max_length" select="'1024'"/>
                    <xsl:with-param name="Placeholder" select="'Введите позицию'"  />
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Error_data" select="$Error_data"/>
                </xsl:call-template>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Фамилия'" />
                    <xsl:with-param name="Name" select="'lastname'"  />
                    <xsl:with-param name="Size" select="'1024'"/>
                    <xsl:with-param name="Max_length" select="'1024'"/>
                    <xsl:with-param name="Placeholder" select="'Назовите Фамилию'"  />
                    <xsl:with-param name="Required" select="'true'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Error_data" select="$Error_data"/>
                </xsl:call-template>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Имя'" />
                    <xsl:with-param name="Name" select="'firstname'"  />
                    <xsl:with-param name="Size" select="'1024'"/>
                    <xsl:with-param name="Max_length" select="'1024'"/>
                    <xsl:with-param name="Placeholder" select="'Назовите Имя'"  />
                    <xsl:with-param name="Required" select="'true'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Error_data" select="$Error_data"/>
                </xsl:call-template>
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Отчество'" />
                    <xsl:with-param name="Name" select="'patronimic'"  />
                    <xsl:with-param name="Size" select="'1024'"/>
                    <xsl:with-param name="Max_length" select="'1024'"/>
                    <xsl:with-param name="Placeholder" select="'Отчество'"  />
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Error_data" select="$Error_data"/>
                </xsl:call-template>
            </div>
            
            <div class="b-csif-c m-2 legal-block" style="margin: 0;">  
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'Адрес *'" />
                    <xsl:with-param name="Name" select="'address'"  />
                    <xsl:with-param name="Size" select="'1024'"/> 
                    <xsl:with-param name="Max_length" select="'1024'"/>
                    <xsl:with-param name="Placeholder" select="'Адрес организации'"  /> 
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Error_data" select="$Error_data"/>
                </xsl:call-template> 
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'ИНН *'" />
                    <xsl:with-param name="Name" select="'inn'"  />
                    <xsl:with-param name="Size" select="'1024'"/> 
                    <xsl:with-param name="Max_length" select="'1024'"/>
                    <xsl:with-param name="Placeholder" select="'ИНН'"  /> 
                    <xsl:with-param name="Pattern" select="'[0-9]{10}'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Error_data" select="$Error_data"/>
                </xsl:call-template> 
                <xsl:call-template name="u-csif">
                    <xsl:with-param name="Label_name" select="'КПП *'" />
                    <xsl:with-param name="Name" select="'kpp'"  />
                    <xsl:with-param name="Size" select="'1024'"/> 
                    <xsl:with-param name="Max_length" select="'1024'"/>
                    <xsl:with-param name="Placeholder" select="'КПП'"  /> 
                    <xsl:with-param name="Pattern" select="'[0-9]{9}'"/>
                    <xsl:with-param name="Error_type" select="$Error_type"/>
                    <xsl:with-param name="Error_data" select="$Error_data"/>
                </xsl:call-template> 
            </div>
            
        </div>
        <div>
            Введите текст, изображенный на картинке:<br/>
            <img src="/captcha.png" alt="captcha" class="e-captcha"/>
            <input type="text" name="captcha" /> <br/>
        </div>
        <input type="submit" value="Принять"/>
        <div>
            <xsl:text>Поля, обязательные для заполнения, помечены: </xsl:text>
            <xsl:call-template name="u-csif-rs"/>
            <xsl:text>.</xsl:text>
        </div>
    </form>
</xsl:template>

<xsl:template name="s-signup">
    <xsl:call-template name="signup-form2">
        <xsl:with-param name="Action" select="concat('/signup', /data/meta/self-retpath)" />
        <xsl:with-param name="Method" select="'POST'"/>
        <xsl:with-param name="Error_type" select="/data/error/type"/>
        <xsl:with-param name="Error_spec" select="/data/error/spec"/>
        <xsl:with-param name="Error_hint" select="/data/error/hint"/>
        <xsl:with-param name="Error_data" select="/data/error/data"/>
    </xsl:call-template>
</xsl:template>

<xsl:template match="/">
    <xsl:apply-imports />
</xsl:template>

</xsl:stylesheet>
