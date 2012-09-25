
/* ************************************************************************


#asset(qx/icon/Tango/16/mimetypes/media-video.png)
#asset(qx/icon/Tango/16/mimetypes/office-presentation.png)
#asset(qx/icon/Tango/16/mimetypes/executable.png)


#asset(qx/icon/Tango/32/actions/list-add.png)
#asset(qx/icon/Tango/32/actions/dialog-apply.png)
#asset(qx/icon/Tango/128/actions/dialog-apply.png)

#asset(qx/icon/Tango/32/categories/multimedia.png)




************************************************************************ */


qx.Class.define("zqr.view.Form.AcvVideoShow",
{
    
    extend : zqr.view.Form.BaseForm,
    
    construct : function(controller, Row, formDescr) {
        console.log("formDescr = ", formDescr);
        this.isModerator = formDescr.isModerator;
        this.base(arguments, controller, Row);
        this.addListeners();
    },
    
    members : {
        urc : {  // upload request config
        
            /**
                не удалено, но заменено на /chstate-acv-video
                """
                    url: {
                        accept: "/activate-acv-video",
                        reject: "/disactivate-acv-video"
                    },
                """
            */
            url: "/chstate-acv-video",
            method: "POST",
            mimetype: "application/json"
        },
        drc : {  // download request config
            url: "/get-acv-video",
            method: "GET",
            mimetype: "application/json"
        },

        tabs : null,
        
        /* Поля формы */
        inp : {
            Estate_name         : null,
            Id                  : null,
            Pay_status          : null,
            Active              : null,
            Sum                 : null
        },


        /* Cписок групп справа */
        snetList: null,
        snetListOptions: {
            url:            "/get-snet-regions",
            labelFieldName: "name",
            descrFieldName: "description"
        },

        taSummary   : null, /* текстовое описание рекламной кампании */
        flashBar    : null, /* поле флеш */
        flashPlayer : null, /* объект плеера */
        mailtoLink  : null, /* кнопка c mailto: */
        payButton:null,


        /**
            Строит основную, не изменяемую часть формы,
                первые 2 вкладки
        **/
        buildBaseForm : function(tabs) {
            this.taSummary = new qx.ui.form.TextArea()
                .set({width:800, height:100, readOnly: true});
            this.flashBar = new qx.ui.container.Composite(new qx.ui.layout.HBox())
                .set({width:800, height:400});
            var infoPage = new qx.ui.tabview.Page("Описание", "icon/16/mimetypes/office-presentation.png");
            infoPage.setLayout(new qx.ui.layout.HBox());
            infoPage.add(this.taSummary);
            tabs.add(infoPage);
            var flashPage = new qx.ui.tabview.Page("Просмотр", "icon/16/mimetypes/media-video.png");
            flashPage.setLayout(new qx.ui.layout.HBox());
            flashPage.add(this.flashBar);
            tabs.add(flashPage);
            return true;
        },

        __buildEditForm_lhtable : function(tabs) {
            var lhtl = new qx.ui.layout.Grid(3, 1);
            lhtl.setColumnFlex(1, 1);
            lhtl.setColumnAlign(0, "right", "top");
            var lhtable = new qx.ui.groupbox.GroupBox("Показ");
            
            /**
                Если видео просматривает модератор,
                то появляется дополнительный набор полей,
                которые модератор может отредактировать.
            **/
            this.inp.Id = new qx.ui.form.TextField();
            var estate = this.inp.Estate_name = new qx.ui.form.SelectBox()
                .set({
                    toolTip: new qx.ui.tooltip.ToolTip("Выбор пола пользователя")
                });
            this._fillSelect(this.inp.Estate_name, [
                {name:"Предреклама",    value: "sprev"           },
                {name:"Простая",        value: ""                },
                {name:"Постреклама",    value: "spost"           },
                {name:"Специальная",    value: "special"         },
                {name:"Заставка",       value: "identification"  },
                {name:"Админская",      value: "admin"           },
                {name:"Системная",      value: "system"          },
                {name:"Пустая",         value: "empty"  }
            ], "name", "value");
            this.inp.Estate_name.getValue = function(){
                return estate.getSelection()[0].getModel();
            };
            this.inp.Active = new qx.ui.form.CheckBox("").set({
                triState:true,
                value: null
            });
            this.inp.Sum = new qx.ui.form.Spinner(0, 0, 134217728).set({
                enabled: false
            });
            this.inp.Pay_status = new qx.ui.form.CheckBox("").set({
                triState:true,
                value: null
            });
            lhtable.setLayout(lhtl);
            this.mailtoLink = new qx.ui.embed.Html();
            var vertical_offset = -1;
            /* -------------------------------------------------------- */
            lhtable.add(new qx.ui.basic.Label().set({value: "Разрешена",  rich : true}),
                    {row:++vertical_offset, column:0});
            lhtable.add(this.inp.Active,   {row:vertical_offset, column:1});
            /* -------------------------------------------------------- */
            lhtable.add(new qx.ui.basic.Label().set({value: "Сумма",  rich : true}),
                    {row:++vertical_offset, column:0});
            lhtable.add(this.inp.Sum,   {row:vertical_offset, column:1});
            lhtable.add(new qx.ui.basic.Label().set({value: "[руб]",  rich : true}),
                    {row:vertical_offset, column:2});
            /* -------------------------------------------------------- */
            lhtable.add(new qx.ui.basic.Label().set({value: "Оплачен",  rich : true}),
                    {row:++vertical_offset, column:0});
            lhtable.add(this.inp.Pay_status,   {row:vertical_offset, column:1});
            lhtable.add(new qx.ui.basic.Label().set({value: "Сословие",  rich : true}),
                    {row:++vertical_offset, column:0});
            lhtable.add(this.inp.Estate_name,   {row:vertical_offset, column:1});
            /* -------------------------------------------------------- */
            lhtable.add(this.mailtoLink, {row:++vertical_offset, column:1});
            return lhtable;
        },

        __buildEditForm_eholder : function(tabs) {
            var eholder = new qx.ui.container.Composite(new qx.ui.layout.HBox());
            var lhtable = this.__buildEditForm_lhtable();            
            eholder.add(lhtable);

            var rhtable = new qx.ui.groupbox.GroupBox("Подcети");
            var rhtl = new qx.ui.layout.Grid(4, 2);
            rhtl.setColumnFlex(0, 1);
            rhtl.setRowFlex(3, 1);
            rhtl.setColumnAlign(0, "right", "top");
            rhtable.setLayout(rhtl);
            
            this.snetList = new zqr.view.SelListTree(this,
                this.snetListOptions.url,
                this.snetListOptions.labelFieldName,
                this.snetListOptions.descrFieldName
            );

            this.snetUfwich = new qx.ui.form.TextArea().set({
                readOnly: true
            });

            var addButton = new qx.ui.form.Button(
                null, "icon/32/actions/list-add.png"
            ).set({
                width:  50,
                height: 50
            });

            addButton.addListener("execute",function(event){
                console.log("---");
                this.controller.biz.swapCont(function(root){
                    console.log("X");
                    return new zqr.view.Controller.FormController(
                        root, {id:null},
                        {
                            type : "form",
                            controller : "snetRegionForm",
                            toolbar : [
                                {type : "back"}
                            ]
                        }
                    );
                });
                console.log("---");
            }, this);


            var row = 0;

            rhtable.add(this.snetUfwich,     {row:row, column:0, colSpan:2});

            row += 1;
            rhtable.add(this.snetList, {row:row, column:0, rowSpan: 3});
            rhtable.add(addButton,    {row:row, column:1});

            var goButton = new qx.ui.form.Button(
                null, "icon/32/actions/dialog-apply.png"
            ).set({
                width:  50,
                height: 50
            });

            row += 1;
            rhtable.add(goButton,      {row:row, column:1});

            goButton.addListener("execute",function(event){
                console.log("---");
                this.controller.biz.onMenuChange({name: "подсети", model:"resource/zqr/descr/snet/region-tab.json"})
                console.log("---");
            }, this);
            
            eholder.add(rhtable,{flex:1});
            return eholder;
        },

        /**
            Редактируемую часть формы для админа
        **/
        buildEditForm : function(tabs) {
            if(!this.isModerator)
                return true;

            var editPage = new qx.ui.tabview.Page("Параметры", "icon/16/mimetypes/executable.png");
            editPage.setLayout(new qx.ui.layout.VBox());
            
            var eholder = this.__buildEditForm_eholder();
            editPage.add(eholder, {flex:1});
            
            

            this.addbuttonRow(editPage);
            
            tabs.add(editPage);
            return true;
        },

        /**
            Основная функция построения формы
        **/
        buildForm : function() {
            this.base(arguments);
            var cnt = new qx.ui.container.Composite(new qx.ui.layout.HBox());
            var tabs =
                this.tabs =
                    new qx.ui.tabview.TabView().set({
                            width:800
                    });

            console.log("<------");
            this.buildBaseForm(tabs)
            console.log("------");
            this.buildEditForm(tabs);
            console.log("------>");
            cnt.add(tabs);
            this.controller.placeForm(cnt);
        },

        /**
            Обработчики событий,
                которые не удалось вынести внутрь
                    отдельных виджетов.
        **/
        addListeners: function(){
            var _this = this;
            if(this.isModerator){
                _this.inp.Active.addListener("changeValue",function(event){
                    _this.inp.Sum.setEnabled(_this.inp.Active.getValue());
                });
            }
        },


        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            var flag = true;
            
            return flag;
        },

        /**shop_f1
            Формирует данные для сервера
        **/
        _uploadData : function(e) {
            this._dropInvalid();
            var res = {}
            for(var fieldName in this.inp){
                item = fieldName.toLowerCase()
                res[item] = this.inp[fieldName].getValue();
            }
            if(this.validateForm()) {
                this.uReq = new qx.io.remote.Request
                    (this.urc.url, this.urc.method, this.urc.mimetype);
                this.uReq.setTimeout(80000);
                for(var item in res){
                    this.uReq.setParameter(item, res[item], true);
                }
            }
        },
        
        /**
            Заполняет форму
        **/
        fillForm : function(result) {

            var clip = result.value;
            console.log("1")
            clip.result = result;

            clip.active = zqr.util.utils.parseEboolean(clip.active);
            clip.pay_status = zqr.util.utils.parseEboolean(clip.pay_status);
            if(clip.sum && "" != clip.sum){
                clip.sum = parseInt(clip.sum);
            }
            else{
                clip.sum = 0;
            }

            this.buildFormLoad(clip);

            console.log("n")
            return true;
        },

        /**
            Основная функция построения формы,
                когда данные уже получены.
            По сути заполнение, и перестроение при необходимости.
        **/
        buildFormLoad : function(clip) {
            this.buildTaFormLoad(clip);
            this.buildFlashFormLoad(clip);
            this.buildEditFormLoad(clip);
            this.buildPayFormLoad(clip);
            return true;
        },

        /**
            Перестроение формы с плеером.
        **/
        buildPayFormLoad : function(clip) {
            if(this.isModerator)
                return true;
            if(clip.active && (!clip.pay_status)){

                var payButton = new qx.ui.form.Button(
                    "Оплатить счет",
                    "icon/128/actions/dialog-apply.png"
                ).set({
                    width:  400,
                    height: 400
                });
                
                payButton.setIconPosition("top");

                payButton.addListener("execute",function(event){
                    window.open('/pay/'+clip.id);
                });
                
                var payPage = new qx.ui.tabview.Page("Оплатить", "icon/16/actions/dialog-apply.png");
                var layout = new qx.ui.layout.Dock();

                //var layout = new qx.ui.layout.Grid(1, 1);
                //layout.setColumnFlex(0, 1);
                
                //payPage.setLayout(new qx.ui.layout.VBox());
                payPage.setLayout(layout);
                //payPage.add(payButton, {row:0, column: 0});
                payPage.add(payButton, {edge:"center"});
                this.tabs.add(payPage);
            }

        },

        /**
            Перестроение редактируемой части формы (для админа)
        **/
        buildEditFormLoad : function(clip) {
            if(!this.isModerator)
                return true;
            console.log("2")
            /**
                Если видео просматривает модератор,
                то появляется дополнительный набор полей,
                которые модератор может отредактировать.
            **/
            this.inp.Id.setValue(clip.id);
            // this.inp.Estate_name.setValue(clip.estate_name);
            this.inp.Estate_name.setSelection([this.inp.Estate_name.itemMap[clip.estate_name]]);
            if(null == clip.active){
                console.log("clip.active = ", clip.active);
                //this.inp.Active.setValue('null');
            }else{
                this.inp.Active.setValue(clip.active);
            }
            this.inp.Pay_status.setValue(clip.pay_status);

            this.inp.Sum.setValue(clip.sum);

            this.snetList.addListener("appear", function(){
                this.snetList.resetFun(function(){
                    console.log("clip.snet_list = ", clip.snet_list);
                    this.snetList.setCheckedObj(clip.snet_list);
                }, this);
            }, this);

            this.snetList.setCheckedObj(clip.snet_list);
            this.snetUfwich.setValue(clip.snet_ufwich);
            
            console.log("3")
            this.mailtoLink.setHtml("<button><a target='_blank' href='mailto:"+clip.email+"' style='text-decoration:none;color:black !important;'>Отправить сообщение</a></button>");

            return true;
        },

        /**
            Обертка надо построителем плеера
        **/
        buildFlashFormLoad : function(clip) {
            this.resetFlashPlayer(clip.ref);
        },

        /**
            Перестроение плеера и содержащего его виджета.
        **/
        resetFlashPlayer : function(ref) {
            this.flashPlayer = new qx.ui.embed.Flash("resource/zqr/flash/tvzavrplayer2.swf").set({
                width: 800,
                height: 400,
                variables : {
                    autoplay:0,
                    src:"/" + ref
                }
            });
            this.flashBar.add(this.flashPlayer);//, {flex: 1});
        },

        /**
            Перестраивает текстовое поле описания кампании.
        **/
        buildTaFormLoad : function(clip) {
            console.log("clip = ", clip);

            var catList         = clip.cat_list;
            var geoList         = clip.geo_list;
            var snetList        = clip.snet_list;
            var eventList       = clip.event_list;
            var pageurlList     = clip.pageurl_list;
            var platformList    = clip.platform_list;

            
            var txt = "Рекламная кампания, размещение видео в видео.\n";
            txt += "Название: " + clip.name + "\n";
            txt += "Комментарий: " + clip.comment + "\n";

            txt += "-------------------\n";
            txt += "Состояние счета: ";
            switch(clip.pay_status) {
                case null:        txt += "счет не выставлен"; break;
                case false:
                    txt += "счет выставлен, но не оплачен";
                    txt += "\n";
                    txt += "Сумма: " + clip.sum + " р.";
                    if(this.payButton){
                    //   this.eholder.add(this.payButton);
                    }
                    break;
                case true:
                    txt += "оплачен";
                    txt += "\n";
                    txt += "Сумма: " + clip.sum + " р.";
                    break;
                default:
                    txt += "неопределено"; break;
            }
            txt += "\n";
            txt += "Статус: ";
            switch(clip.active) {
                case null:        txt += "на модерации"; break;
                case false:   txt += "запрещен"; break;
                case true:    txt += "разрешен"; break;
                default:
                    txt += "неопределено"; break;
            }
            txt += "\n";
            txt += "-------------------\n";

            txt += "Дата начала: " + zqr.util.utils.formatJsDateTime(zqr.util.utils.getDate(clip.datestart, 0)) + "\n";
            txt += "Дата конца: " + zqr.util.utils.formatJsDateTime(zqr.util.utils.getDate(clip.datestop, 0)) + "\n";
            txt += "Внешняя ссылка: " + clip.url + "\n";
            txt += "URL ролика: " + clip.ref + "\n";

            /*
                Убрал, ибо пользователь задать это не может,
                В базу забивается некая очень большая константа,
                которая не соответсвует правде.
                txt += "Продолжительность ролика: " + clip.duration + " секунд\n";
            */

            txt += "Желаемое количество показов: " + clip.wish + "\n";
            txt += "Показано: " + clip.shown + "\n";
            var tpl = "";
            var apl = ["preroll", "midroll", "postroll"];
            for(var i=0; i<apl.length; i++) {
                if(clip[apl[i]] == "true") {
                    if(tpl != "")
                        tpl += ", ";
                    tpl += apl[i];
                }
            }
            txt += "Размещение ролика: " + tpl + "\n";
            txt += "Повторный показ ролика учтенному пользователю: ";
            if(clip.rerun_hours == "")
                txt += "не ограничен";
            else
                txt += clip.rerun_hours + ":" + clip.rerun_minutes;
            txt += "\n";
            txt += "Таргетирование пользователей:\n";
            txt += "Пол: ";
            switch(clip.user_male) {
                case "true": txt += "для мужчин"; break;
                case "false": txt += "для женщин"; break;
                default: txt += "любой";
            }
            txt += "\n";
            txt += "Возраст:";
            if(clip.age_from == "")
                txt += "любой";
            else
                txt += "от " + clip.age_from + " до " + clip.age_to + " лет";
            txt += "\n";
            txt += "Время показа: ";
            if(clip.time_from == "")
                txt += "любое";
            else
                txt += "с " + clip.time_from + " до " + clip.time_to + " часов";
            txt += "\n";
            // -------------------------------------------------------------\\
            txt += "Таргетирование по регионам: ";
            if(geoList.length == 0)
                txt += "весь мир\n";
            else
                txt += "\n";
            for(var i=0; i<geoList.length; i++)
                txt += geoList[i].name_ru + " (" + geoList[i].code + ")\n";
            // -------------------------------------------------------------\\
            txt += "Таргетирование по жанрам: ";
            if(!catList)
                catList = [];
            if(catList.length == 0)
                txt += "все жанры\n";
            else{
                txt += "\n";
                for(var i=0; i<catList.length; i++){
                    txt += catList[i].name + "\n";
                }
            }
            // -------------------------------------------------------------\\
            txt += "Таргетирование по подсетям: ";
            txt += "\n          пожелание: "
            if(clip.snet_ufwich && "" != clip.snet_ufwich && "null" != clip.snet_ufwich){
                txt += "\n              " + clip.snet_ufwich ;
            }
            else{
                txt += "\n              все доступные";
            }
            if(snetList && (snetList.length > 0)){
                txt += "\n          подсети: ";
                for(var i = 0; i != snetList.length; ++i){
                    txt += "\n              "  + snetList[i].name;
                }
            }
            else{
                txt += "\n          подсети пока не указаны";
            }
            txt += "\n";
            
            // -------------------------------------------------------------\\
            txt += "Таргетирование платформам: ";
            if(platformList && (platformList.length > 0)){
                for(var i = 0; i != platformList.length; ++i){
                    txt += "\n      "  + (i+1) + ") ";
                    txt +=  platformList[i].name ;
                    txt += " "
                    txt +=  platformList[i].description ;
                }
            }
            else{
                txt += "\n          все доступные";
            }
            txt += "\n";
            // -------------------------------------------------------------\\
            txt += "Таргетирование площадкам: ";
            if(pageurlList && (pageurlList.length > 0)){
                for(var i = 0; i != pageurlList.length; ++i){
                    txt += "\n              "
                    txt +=  pageurlList[i].url ;
                    txt += "\n                "
                    txt +=  pageurlList[i].description ;
                }
            }
            else{
                txt += "\n          все доступные";
            }
            txt += "\n";
            // -------------------------------------------------------------\\
            
            txt += "Рекламные события: ";
            if(eventList && (eventList.length > 0)){
                for(var i = 0; i != eventList.length; ++i){
                    txt += "\n      "  + (i+1) + ") ";
                    switch(eventList[i].type) {
                        case "show": txt += "начало показа рекламы"; break;
                        case "fullshow": txt += "конец показа рекламы"; break;
                        case "click": txt += "клик по рекламе"; break;
                        default: txt += eventList[i].type;
                    }
                    txt += "\n      "   + eventList[i].url;
                }
            }
            else{
                txt += "\n          не указаны";
            }

            
            if(this.isModerator) {
                txt += "\nEmail пользователя: " + clip.email + "\n";
            }
            txt += "\n";
            
            this.taSummary.setValue(txt);

        },

        refresh: function() {
            console.log("zqr.view.Form.SnetRegionForm");
        },

        hide_global_pb : function() {
            this.controller.biz.hide_global_pb ()
        },

        show_global_pb : function() {
            this.controller.biz.show_global_pb()
        },

        onAction: function(x, y, z){
            return this.controller.biz.onAction(x, y, z);
        },

        _uploadData : function(e) {
            this.base(arguments, e);
            if(this.validateForm()) {
                this.uReq.setParameter("snet_list", this.snetList.getSelectedId(), true);
            }
        }
        

    }
});

