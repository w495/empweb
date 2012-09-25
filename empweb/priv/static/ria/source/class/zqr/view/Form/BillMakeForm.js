/**
    Форма пополнения кошелька.
    Пользователю выставляется счет на сумму,
    которую он сам укажет.
**/


/* ************************************************************************


************************************************************************ */

qx.Class.define("zqr.view.Form.BillMakeForm",
{
    extend : zqr.view.Form.BaseForm,

    construct : function(controller, Row) {
        this.base(arguments, controller, Row);
    },

    members : { 
        urc : {  // upload request config
            url: "/update-bill",
            method: "POST",
            mimetype: "application/json"
        },

        drc : {  // download request config
            url: "/get-bill-info",
            method: "GET",
            mimetype: "application/json"
        },

        /* Поля формы */
        inp : {
            Id   : null,
            Sum   : null
        },

        buildForm : function() {
            this.base(arguments);

            this.inp.Id = new qx.ui.form.TextField();
            this.inp.Sum = new qx.ui.form.Spinner(0, 100, 1152921504606846976);

            var layout = new qx.ui.layout.Grid(12, 6);
            var cnt = new qx.ui.container.Composite(layout);


            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");

            var l1 = new qx.ui.basic.Label("Пополнение счета");
            l1.setFont("bold");
            l1.setAlignX("left");
            cnt.add(l1, {row:0, column:0, colSpan:2});

            var vertical_offset = 0;
            var RFM = zqr.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;

            cnt.add(new qx.ui.basic.Label()
                    .set({value: "Сумма" + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Sum,
                    {row:vertical_offset , column:1});

            this.cancelButton = null;
            this.addbuttonRow(cnt, ++vertical_offset);
            this.controller.placeForm(cnt);

        }
    }
});

