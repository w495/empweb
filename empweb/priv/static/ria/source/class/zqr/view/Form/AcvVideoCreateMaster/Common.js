/* ************************************************************************
    https://gist.github.com/1639960
************************************************************************ */

qx.Class.define("zqr.view.Form.AcvVideoCreateMaster.Common",
{
    extend : zqr.view.Form.AcvVideoCreateMaster.BasePage,
    
    construct : function(uReq, Row, Options) {
        this.base(arguments, uReq, Row, Options);
        this.IsModerator = false;
    },

    statics : {
        DATE_PAST_OFFSET : 0,
        DATE_FUTURE_OFFSET : 1,
        DATE_DELTA : 1
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
            url: "/get-acv-video/common",
            method: "GET",                  // POST \ GET
            mimetype: "application/json"    // application/json
        },
        
        getComposite : function(){
            return this.composite;
        },

        /**
         * Поля формы.
         * Вообще, учитывая, богатсво форм они могут не понадобиться.
        **/
        inp : {
            Id:null,
            Name:null,
            Comment:null,
            DateStart:null,
            DateStop:null
        },
        
        // -------------------
        textfield1 : null,
        textfield2 : null,
        
        buildForm : function(){
            var RFM = zqr.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            
            var layout = new qx.ui.layout.Grid(2, 5);
            layout.setColumnFlex(1, 1);
            layout.setColumnAlign(0, "right", "top");
            
            this.composite  = new qx.ui.container.Composite (layout);
            this.composite.setWidth(zqr.Config.MASTER_FORM_WIDTH);
            

            this.inp.Id = new qx.ui.form.TextField();
            this.inp.Name = new qx.ui.form.TextField() 
                .set({
                    placeholder: "Название рекламной кaмпании",
                    required:true,
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Название рекламной кaмпании")
                });
                
            this.inp.Comment = new qx.ui.form.TextArea() 
                .set({
                    placeholder: "Его кроме Вас никто не увидит",
                    required:true,
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Ваш комментарий к вашей рекламной кaмпании")

                });
                
            var dateStart = new Date();
            dateStart.setDate(dateStart.getDate()
                - zqr.view.Form.AcvVideoCreateMaster.Common.DATE_PAST_OFFSET);
            this.inp.DateStart = new qx.ui.form.DateField()
                .set({
                    value: dateStart,
                    required:true,
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Камапания начнется ровно в 00:00 этой даты")
                });
            var dateStop = new Date();
            dateStop.setDate(dateStop.getDate()
                + zqr.view.Form.AcvVideoCreateMaster.Common.DATE_FUTURE_OFFSET);
            this.inp.DateStop = new qx.ui.form.DateField()
                .set({
                    value: dateStop,
                    required:true,
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Камапания закончится ровно в 00:00 этой даты")
                });
                
            var pageName = new qx.ui.basic.Label()
                .set({
                    value: "Общая информация",  font: "bold",
                    alignX: "left", rich : true
                });

            var vertical_offset = -1;
            this.composite.add(pageName, {row:++vertical_offset, column:0, colSpan:2})
            
            this.composite.add(new qx.ui.basic.Label().set({value: "Название"  + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.Name,   {row:vertical_offset, column:1});
            
            this.composite.add(new qx.ui.basic.Label().set({value: "Комментарий"  + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.Comment,   {row:vertical_offset, column:1});
            
            this.composite.add(new qx.ui.basic.Label().set({value: "Дата начала"  + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.DateStart,   {row:vertical_offset, column:1});
            
            this.composite.add(new qx.ui.basic.Label().set({value: "Дата конца"  + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.DateStop,   {row:vertical_offset, column:1});
            
            this.inp.Name.focus();
            return this.composite;
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
                this.inp[fieldName].setValue(data.value[item]);
            }
        },
        
        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            var flag = true;
            
            var id =  this.inp.Id.getValue();
            if(null == this.inp.Comment.getValue()) {
                this.inp.Comment.setValue("");
            }
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(1, 100, this.inp.Name);
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(1, 200, this.inp.Comment);
            flag &= zqr.view.Form.AbstractForm.customFormcheckDateNow(this.inp.DateStart,
                zqr.view.Form.AcvVideoCreateMaster.Common.DATE_PAST_OFFSET
                + zqr.view.Form.AcvVideoCreateMaster.Common.DATE_DELTA);
            flag &= zqr.view.Form.AbstractForm.customFormcheckDateNow(this.inp.DateStop,
                0);
                        
            if(this.inp.DateStop.getValue() < this.inp.DateStart.getValue()){
                this.inp.DateStop.setValid(false);
                this.inp.DateStop.setInvalidMessage("Должна быть позднее даты начала");
                flag &= false;
            }

            return flag;
        },
        
        /**
            Применив некоторые преобразования <<загружает>> данные на сервер
        **/
        saveData : function(e) {
            this.base(arguments, e);
            
            var formIsValid = this.validateForm();
            if(formIsValid){
                var res = {}
                for(var fieldName in this.inp){
                    item = fieldName.toLowerCase()
                    if(("datestart" == item) || ("datestop" == item)){
                        // приведение даты к виду воспринимаем
                        res[item] = zqr.util.utils.
                            normalize_date(this.inp[fieldName].getValue());
                    }
                    else{
                        res[item] = this.inp[fieldName].getValue();
                    }
                }  
                for(var item in res){
                    console.log(item, res[item]);
                    this.uReq.setParameter(item, res[item], true);
                }
            }
            return formIsValid;
        }
    }
});


