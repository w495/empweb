/* ************************************************************************
    https://gist.github.com/1639960
************************************************************************ */

qx.Class.define("zqr.view.Form.AcvVideoCreateMaster.BasePage",
{
    type : "abstract",
    
    extend: qx.core.Object,

    construct : function(uReq, Row, Options) {
        this.uReq = uReq;
        this.inp = {};
        this.buildForm();
        if(!Row)
            console.log("##", Row, "$$", this.drc.url );
            
        if(Row && Row["id"]){
            // console.log("this.drc.url ----> loadFormData ----> ", this.drc.url);
            this.loadFormData(Row["id"], "id");
        }
        this.addListeners();
        
        if(Options){
            if(Options.disabled)
            {
                this.disableAll();
            }
        }
        
    },

    members : {
        
        /* Upload request берется из конструктора */
        uReq : null,
        
        /* Download request делаем сами*/
        dReq : null,
        
        /**
         * Download  request config
         *
         * Предполагается, что загружать данные каждая страница
         * мастера будет самостоятельно, а вот выгружаться на сервер они будут
         * одним запросом.
         * 
        **/
        drc : {
            url:        null,
            method:     null,
            mimetype:   null
        },
        
        getComposite : function(){
            return this.composite;
        },

        /**
         * Поля формы.
         * Вообще, учитывая, богатсво форм они могут не понадобиться.
        **/
        inp : {
        },

        buildForm : function(){
        },

        refresh: function(){
        },

        /**
            Обработчики событий,
                которые не удалось вынести внутрь
                    отдельных виджетов.
        **/
        addListeners: function() {            
            var _this = this;
        },
        
        /**
            Получает данные с сервера.
        **/
        loadFormData : function(id, paramName) {
            console.log("this.drc.url ---->", this.drc.url);
            this.dReq = new qx.io.remote.Request
                (this.drc.url, this.drc.method, this.drc.mimetype);
            this.dReq.setTimeout(60000);
            this.dReq.setParameter(paramName, id);
            this.dReq.addListener("completed", this._onLoadFormDataCompl, this);
            this.dReq.send();
        },
        
        _onLoadFormDataCompl : function(response) {
            var result = response.getContent();
            if (false == zqr.util.errors.process(this, result))
                return false;
            this.fillForm(result);
            return true;
        },
        
        disableAll: function() { this.changeEnabled(false);},
        
        /**
            Функция блокировки\разблокировки элементов ввода.
        **/
        changeEnabled: function(enabled) {
            if(this.inp){
                for(var name in this.inp){
                    var field = this.inp[name]
                    if(field.setReadOnly)
                    {
                        console.log("field.setReadOnly");
                        this.inp[name].setReadOnly(!enabled);
                    }
                    else if(field.setEnabled)
                    {
                        console.log("field.setEnabled");
                        this.inp[name].setEnabled(enabled);
                    }else
                    {
                        console.log("not false");
                    }
                    if(this.onChangeEnabled){
                        this.onChangeEnabled(enabled);
                    }
                    //field.addListener('changeEnabled',function(enabled){
                    //    console.log("change enabled");
                    //});
                }
            }
        },
        
        /**
            Функция блокировки\разблокировки элементов ввода, которые не относятся
                к this.inp, и там их нельзя обработать.
        **/
        onChangeEnabled: function(enabled) {

        },
        
        /**
            Заполняет форму полученными данными.
        **/
        fillForm : function(data) {
        },
        
        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            var flag = true;
            return flag;
        },
        
        /**
            Сбрасывает невалидность
        **/
        _dropInvalid : function() {
            for(var item in this.inp) {
                this.inp[item].setValid(true);
            }
        },
        _fillSelect : function(sel, vals, alias, value) {
            sel.itemMap = [];
            for(var j=0; j<vals.length; j++) {
                var SI = vals[j];
                var selItem = new qx.ui.form.ListItem(SI[alias], null, SI[value]);
                sel.itemMap[SI[value]] = selItem;
                sel.add(selItem);
            }
        },
                
        /**
            Применив некоторые преобразования <<загружает>> данные на сервер
        **/
        saveData : function(e) {
            this._dropInvalid();
            
            return true;
        }
    }
});

