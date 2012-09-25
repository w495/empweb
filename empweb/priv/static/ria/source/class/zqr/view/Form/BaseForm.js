/* ************************************************************************

    Абстрактный класс описания формы
    для созданию / редактированию чего либо.

#asset(qx/icon/Tango/32/actions/edit-undo.png)
#asset(qx/icon/Tango/32/actions/edit-redo.png)

#asset(qx/icon/Tango/32/actions/media-seek-backward.png)
#asset(qx/icon/Tango/32/actions/media-seek-forward.png)

#asset(qx/icon/Tango/32/actions/mail-mark-junk.png)

#asset(qx/icon/Tango/32/actions/dialog-apply.png)
#asset(qx/icon/Tango/32/actions/dialog-ok.png)

#asset(qx/icon/Tango/32/actions/process-stop.png)




************************************************************************ */


qx.Class.define("zqr.view.Form.BaseForm",
{
    type : "abstract",

    extend : zqr.view.Form.AbstractForm,

    construct : function(controller, Row) {
        this.base(arguments, controller);
        // Привязываем обработчики только один раз.
        // Использовать addListenerOnce тут нельзя,
        //      ибо в случае отказа функция вызываться не будет.
        // Использовать hasListener тут нельзя,
        //      ибо биндит к члену класса
        this.submitButton =  new qx.ui.form.Button("Отправить", "icon/32/actions/dialog-ok.png");
        this.submitButton.addListener("execute", this._onSubmitClick, this);
        this.cancelButton = new qx.ui.form.Button("Отмена", "icon/32/actions/mail-mark-junk.png");
        this.cancelButton.addListener("execute", this._onCancelClick, this);
        this.inp = {};
        this.fake_inp = {};
        this.buildForm();
        if(Row != undefined && Row["id"] != undefined)
            this.loadFormData(Row["id"], "id");
    },  
    members : {
        /* Поля формы */
        inp : {},
        fake_inp: {},
        /* Upload request */
        uReq : undefined,
        /* Download  request */
        dReq : undefined,   
        /* Upload request config */
        urc : {             
            url: "",        // 
            method: "",     // POST \ GET
            mimetype: ""    // application/json
        },
        /* Download  request config */
        drc : {             
            url: "",        // 
            method: "",     // POST \ GET
            mimetype: ""    // application/json
        },

        setLegend:function(legend) {
           this.controller.setLegend(legend);
        },

        getLegend:function() {
           return this.controller.getLegend();
        },
                
        showEMsg : function(fieldName, msg) {
            for(var item in this.inp) {
                if(fieldName == item.toLowerCase()){
                    this.inp[item].setInvalidMessage(msg);
                    this.inp[item].setValid(false);
                }
            }
        },
        resetValues: function() {
            for(var item in this.inp) {
                // очищаем поля формы
                if(!this.inp[item]
                    || !this.inp[item].resetValue
                    || !this.inp[item].resetValid)
                    continue;
                this.inp[item].resetValue();
                this.inp[item].resetValid();
            }
        },
        /**
            Строит визуальное представление формы
        **/
        buildForm : function() {
            this.resetValues();
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
            Добавляет кнопки [Отправить] [Отмена]
        **/
        __addbuttonRow: function() {
            var buttonRow = new qx.ui.container.Composite();
            buttonRow.setMarginTop(5);
            var hbox = new qx.ui.layout.HBox(5);
            hbox.setAlignX("right");
            buttonRow.setLayout(hbox);
            if(this.submitButton){
                buttonRow.add(this.submitButton);
            }
            if(this.cancelButton){
                buttonRow.add(this.cancelButton);
            }
            return buttonRow;
        },
                
        /**
            Добавляет кнопки [Отправить] [Отмена]
        **/
        addbuttonRow: function(cnt, vertical_offset) {
            var buttonRow  = this.__addbuttonRow();

            if(cnt.getLayout() instanceof qx.ui.layout.Grid){
                cnt.add(buttonRow, {row:vertical_offset , column:0, colSpan:3});
            }
            else{
                cnt.add(buttonRow);
            }
        },



        _dropInvalid : function() {
            console.log("_dropInvalid : function() { this.inp = ", this.inp);
            for(var item in this.inp) {
                this.inp[item].setValid(true);
            }
        },
        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            var flag = true;
            return flag;
        },

        _uploadData : function(e) {
            this._dropInvalid();
            var res = {}
            for(var fieldName in this.inp){
                item = fieldName.toLowerCase()
                res[item] = this.inp[fieldName].getValue();
            }  
            if(this.validateForm()) {
                this.uReq = undefined;
                this.uReq = new qx.io.remote.Request
                    (this.urc.url, this.urc.method, this.urc.mimetype);
                this.uReq.setTimeout(60000);
                for(var item in res){
                    this.uReq.setParameter(item, res[item], true);
                }
            }
        },

        /**
            Посылает данные на сервер.
        **/
       _onSubmitClick : function(e) {
            this._uploadData(e);
            if(this.uReq){
                this.submit(this.uReq);
            }
        },
        _onCancelClick : function(e) {
            this.back();
        },
        /**
            Получает данные с сервера.
        **/
        loadFormData : function(id, paramName) {
            this.dReq = undefined;
            this.dReq = new qx.io.remote.Request
                (this.drc.url, this.drc.method, this.drc.mimetype);
            console.log("(this.drc.url, this.drc.method, this.drc.mimetype) = ", (this.drc.url, this.drc.method, this.drc.mimetype));
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
        /**
            Заполняет форму полученными данными.
        **/
        fillForm : function(data) {
            for(var fieldName in this.inp){
                var item = fieldName.toLowerCase();
                this.inp[fieldName].setValue(data.value[item])
            }
        }
    }
});

