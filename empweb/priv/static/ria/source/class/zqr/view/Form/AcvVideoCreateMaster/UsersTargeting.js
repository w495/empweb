/* ************************************************************************
    https://gist.github.com/1639960
************************************************************************ */

qx.Class.define("zqr.view.Form.AcvVideoCreateMaster.UsersTargeting",
{
    extend : zqr.view.Form.AcvVideoCreateMaster.BasePage,
    
    construct : function(uReq, Row, Options) {
        console.log("zqr.view.Form.AcvVideoCreateMaster.UsersTargeting")
        this.base(arguments, uReq, Row, Options);
    },

    members : {
        helpUrl : "/docs/offer",

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
            url: "/get-acv-video/users-targeting",
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
            Age_from:       null,
            Age_to:         null,
            Time_from:      null,
            Time_to:        null,
            Gender:         null
        },
        
        boxGender:  null,
        boxAge :    null,
        boxTime :   null,
        boxRerun:   null,
        
        buildForm : function(){
            console.log("AcvVideoCreateMaster");
            var RFM = zqr.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            var pageName = new qx.ui.basic.Label()
                .set({
                    value: "Таргетирование по зрителям",  font: "bold",
                    alignX: "left", rich : true
                });
                
            var layout = new qx.ui.layout.Grid(1, 5);
            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");
            
            this.composite  = new qx.ui.container.Composite (layout);
            this.composite.setWidth(zqr.Config.MASTER_FORM_WIDTH_M);
            
            this.boxGender = this.makeBoxGender();
            this.boxAge = this.makeBoxAge();
            this.boxTime = this.makeBoxTime();
                    console.log("AcvVideoCreateMaster");
            var vertical_offset = -1;
            
            this.composite.add(pageName,
                {row:++vertical_offset, column:0});
            
            this.composite.add(this.boxGender,
                {row:++vertical_offset, column:0});
            
            this.composite.add(this.boxAge,
                {row:++vertical_offset, column:0});
            
            this.composite.add(this.boxTime,
                {row:++vertical_offset, column:0});
            
            return this.composite;
        },
        
        makeBoxGender : function() {
            this.inp.Gender = new qx.ui.form.SelectBox()
                .set({
                    toolTip: new qx.ui.tooltip.ToolTip("Выбор пола пользователя")
                    //,blockToolTip: true
                });
            this.__fillSelect(this.inp.Gender, [
                    // {name:"Все",     value: "null"},
                    {name:"Мужской", value: "true"},
                    {name:"Женский", value: "false"}
                ], "name", "value");
            var vertical_offset = 0;
            var boxGender = new qx.ui.groupbox.CheckGroupBox("Пол")
                .set({
                    toolTip: new qx.ui.tooltip.ToolTip("Активируйте, если хотите показывать ролик только определенным по полу зрителям")
                });
            var layout2 = new qx.ui.layout.Grid(1, 1)
            boxGender.setLayout(layout2);
            boxGender.setValue(false);
            layout2.setColumnFlex(0, 1);
            boxGender.add(this.inp.Gender, {row:0, column:0});
            return boxGender;
        },
        
        makeBoxAge : function() {
            this.inp.Age_from = new qx.ui.form.Spinner(1, 1, 100)
                .set({
                    toolTip: new qx.ui.tooltip.ToolTip("Показывать зрителям старше этого возраста")
                });
            this.inp.Age_to = new qx.ui.form.Spinner(2, 100, 200)
                .set({
                    toolTip: new qx.ui.tooltip.ToolTip("Показывать зрителям младше этого возраста")
                });
            var vertical_offset = 0;
            var boxAge = new qx.ui.groupbox.CheckGroupBox("Возраст")
                .set({
                    toolTip: new qx.ui.tooltip.ToolTip("Активируйте, если хотите показывать ролик только зрителям указанного возраста")
                });
            var layout = new qx.ui.layout.Grid(1, 5)
            layout.setColumnFlex(1, 1);
            layout.setColumnFlex(2, 1);
            layout.setColumnFlex(4, 1);
            boxAge.setLayout(layout);
            boxAge.setValue(false);
            boxAge.add(new qx.ui.basic.Label().set({value: "От:",  rich : true}), {row:vertical_offset, column:0});
            boxAge.add(this.inp.Age_from, {row:vertical_offset, column:1});
            boxAge.add(new qx.ui.basic.Label().set({value: "До:",  rich : true}), {row:vertical_offset, column:3});
            boxAge.add(this.inp.Age_to, {row:vertical_offset, column:4});
            return boxAge;
        },
        
        makeBoxTime : function() {
            this.inp.Time_from = new qx.ui.form.Spinner(0, 0, 24)
                .set({
                    toolTip: new qx.ui.tooltip.ToolTip("Показывать ролик начиная с этого часа")
                });
            this.inp.Time_to = new qx.ui.form.Spinner(0, 24, 24)
                .set({
                    toolTip: new qx.ui.tooltip.ToolTip("Показывать ролик до этого часа")
                });
            var vertical_offset = 0;
            var boxTime = new qx.ui.groupbox.CheckGroupBox("Время показа")
                .set({
                    toolTip: new qx.ui.tooltip.ToolTip("Активируйте, если хотите показывать ролик только в определнное время суток")
                });
            var layout = new qx.ui.layout.Grid(1, 5);
            layout.setColumnFlex(1, 1);
            layout.setColumnFlex(2, 1);
            layout.setColumnFlex(4, 1);
            boxTime.setLayout(layout);
            boxTime.setValue(false);
            boxTime.add(new qx.ui.basic.Label().set({value: "От:",  rich : true}), {row:vertical_offset, column:0});
            boxTime.add(this.inp.Time_from, {row:vertical_offset, column:1});
            boxTime.add(new qx.ui.basic.Label().set({value: "До:",  rich : true}), {row:vertical_offset, column:3});
            boxTime.add(this.inp.Time_to, {row:vertical_offset, column:4});
            return boxTime;
        },
        
        __fillSelect : function(sel, vals, alias, value) {
            sel.itemMap = [];
            for(var j=0; j<vals.length; j++) {
                var SI = vals[j];
                var selItem = new qx.ui.form.ListItem(SI[alias], null, SI[value]);
                sel.itemMap[SI[value]] = selItem;
                sel.add(selItem);
            }
        },
        
        onChangeEnabled: function(enabled) {
            this.boxGender.setEnabled(enabled);
            this.boxAge.setEnabled(enabled);
            this.boxTime.setEnabled(enabled);
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

            this.boxGender.setValue(("null" != data.value.user_male));
            //this.inp.Gender.setValue(data.value.user_male);
            this.inp.Gender.setSelection([this.inp.Gender.itemMap[data.value.user_male]]);

            this.boxTime.setValue((("null" != data.value.time_from) && ("null" != data.value.time_to)));
            this.inp.Time_from.setValue(parseInt(data.value.time_from));
            this.inp.Time_to.setValue(parseInt(data.value.time_to));
            this.boxAge.setValue((("null" != data.value.age_from) && ("null" != data.value.age_to)));
            this.inp.Age_from.setValue(parseInt(data.value.age_from));
            this.inp.Age_to.setValue(parseInt(data.value.age_to));
        },

        validateAge: function() {
            var age_from = this.inp.Age_from.getValue();
            var age_to = this.inp.Age_to.getValue();
            console.log("age_from  = ", age_from);
            console.log("age_to    = ", age_to);
            if (age_from > age_to) {
                this.inp.Age_from.setValid(false);
                this.inp.Age_to.setValid(false);
                this.inp.Age_from.setInvalidMessage("Должно быть меньше");
                this.inp.Age_to.setInvalidMessage("Должно быть больше");
                return false;
            }
            return true;
        },

        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            console.log("validateForm : function()")
            var flag = true;

            flag &= this.validateAge();
            
            console.log("flag = ", flag);
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
                if(this.boxGender.getValue()){
                    res.user_male = this.inp.Gender.getSelection()[0].getModel();
                }else{
                    res.user_male = "null";
                }
                if(this.boxAge.getValue()){
                    res.age_from    = this.inp.Age_from.getValue();
                    res.age_to      = this.inp.Age_to.getValue();
                }else{
                    res.age_from    = "null";
                    res.age_to      = "null";
                }
                if(this.boxTime.getValue()){
                    res.time_from   = this.inp.Time_from.getValue();
                    res.time_to     = this.inp.Time_to.getValue();
                }else{
                    res.time_from   = "null";
                    res.time_to     = "null";
                }
                for(var item in res){
                    this.uReq.setParameter(item, res[item], true);
                }
            }
            return formIsValid;
        }
    }
});

