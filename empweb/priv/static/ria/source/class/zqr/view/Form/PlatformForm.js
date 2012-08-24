/* ************************************************************************

    Класс описания формы по созданию группы
    ИСПОЛЬЗОВАНИЕ:
        Администрировнаие > группы пользователей | [Создать]

************************************************************************ */


qx.Class.define("zqr.view.Form.PlatformForm",
{
    extend : zqr.view.Form.BaseForm,

    construct : function(controller, Row) {
        this.base(arguments, controller, Row);
    },

    members : {

        /* Upload request config */
        urc : {             
            url: "/update-platform",
            method: "POST",
            mimetype: "application/json"
        },

        /* Download  request config */
        drc : {             
            url: "/get-platform-info",
            method: "GET",
            mimetype: "application/json"
        },

        /* Поля формы */
        inp : {
            Id           : null,
            Name         : null,
            Code         : null,
            Description  : null
        },

        buildForm : function() {
            this.base(arguments);
            
            this.inp.Id = new qx.ui.form.TextField().set({
                enabled: false
            });
            this.inp.Name = new qx.ui.form.TextField();
            this.inp.Code = new qx.ui.form.TextField();
            this.inp.Description  = new qx.ui.form.TextArea().set({
                height: 200,
                width: 150
            });
            
            var layout = new qx.ui.layout.Grid(12, 6);
            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");
            
            var cnt = new qx.ui.container.Composite(layout);

            var l1 = new qx.ui.basic.Label("Общая информация").set({
                font:"bold",
                alignX:"left"
            });

            cnt.add(l1, {row:0, column:0, colSpan:2});

            var vertical_offset = 0;
            var RFM = zqr.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            
            cnt.add(new qx.ui.basic.Label("#"),
                {row:++vertical_offset, column:0});
            cnt.add(this.inp.Id, {row:vertical_offset, column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Имя" + RFM,  rich : true}),
                {row:++vertical_offset, column:0});
            cnt.add(this.inp.Name,{row:vertical_offset, column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Код" + RFM,  rich : true}),
                {row:++vertical_offset, column:0});
            cnt.add(this.inp.Code,{row:vertical_offset, column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Описание"+ RFM,  rich : true}),
                {row:++vertical_offset, column:0});
            cnt.add(this.inp.Description,{row:vertical_offset, column:1});

            this.addbuttonRow(cnt, ++vertical_offset);
            this.controller.placeForm(cnt);
            this.inp.Name.focus();
        },

        validateForm : function() {
            var flag = true;
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(1, 50, this.inp.Name);
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(3, 3, this.inp.Code);
            return flag
        }

    }
});

