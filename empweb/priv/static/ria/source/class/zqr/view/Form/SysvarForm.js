/* ************************************************************************



************************************************************************ */


qx.Class.define("zqr.view.Form.SysvarForm",
{
    extend : zqr.view.Form.BaseForm,

    construct : function(controller, Row) {
        this.base(arguments, controller, Row);
//         this.submitButton.addListener("execute", function(){
//             var win = zqr.util.utils.infoWindow("Конфигурация изменена");
//             this.controller.biz.getRoot().add(win, {
//                 left : win.l*1,
//                 top  : win.t*1
//             });
//         }, this);
    },

    members : { 
        urc : {  // upload request config
            url: "/update-sysvar",
            method: "POST",
            mimetype: "application/json"
        },

        drc : {  // download request config
            url: "/get-sysvar-info",
            method: "GET",
            mimetype: "application/json"
        },
        
        /* Поля формы */
        inp : {
            Id: null,
            Name : null,
            Value : null,
            Description : null,
            Type : null
        },

        buildFormLoadFactory : function(type, value) {
            switch(type)
            {
                case "integer":
                    var _value = parseInt(value);
                    return new qx.ui.form.Spinner(0, _value, 1152921504606846976);
                case "boolean":
                    var _value = (/^true$/i).test(value);
                    var element = new qx.ui.form.CheckBox("").set({
                        value: _value
                    });
                    return element;   
                default:
                    return qx.ui.form.TextField(value)
            }
        },

        buildFormLoad : function(data_value) {

            this.inp.Id = new qx.ui.form.TextField(data_value.id).set({
                enabled: false
            });
            this.inp.Name = new qx.ui.form.TextField(data_value.name).set({
                readOnly: true
            });
            this.inp.Name.setWidth(zqr.Config.MASTER_FORM_WIDTH);
            this.inp.Description = new qx.ui.form.TextField(data_value.description).set({
                readOnly: true
            });

            this.inp.Type = new qx.ui.form.TextField(data_value.type).set({
                readOnly: true
            });
            
            this.inp.Value = this.buildFormLoadFactory(data_value.type, data_value.value)
                // = new qx.ui.form.Spinner(0, 100, 1152921504606846976);

            var layout = new qx.ui.layout.Grid(12, 6);
            var cnt = new qx.ui.container.Composite(layout);
            

            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");

            var l1 = new qx.ui.basic.Label("Параметры системы");
            l1.setFont("bold");
            l1.setAlignX("left");
            cnt.add(l1, {row:0, column:0, colSpan:2});

            var vertical_offset = 0;
            var RFM = zqr.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;

            cnt.add(new qx.ui.basic.Label()
                    .set({value: "Имя" ,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Name,
                    {row:vertical_offset , column:1});

            cnt.add(new qx.ui.basic.Label()
                    .set({value: "Тип",  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Type,
                    {row:vertical_offset ,
                    column:1});
                    
            cnt.add(new qx.ui.basic.Label()
                    .set({value: "Описание",  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Description,
                    {row:vertical_offset , column:1});
                    
            cnt.add(new qx.ui.basic.Label()
                    .set({value: "Значение" + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Value,
                    {row:vertical_offset , column:1});
                    
            this.addbuttonRow(cnt, ++vertical_offset);
            this.controller.placeForm(cnt);
        },

        fillForm : function(data) {
            console.log("-> data = ", data);
            this.buildFormLoad(data.value);
        } 
    }
});

