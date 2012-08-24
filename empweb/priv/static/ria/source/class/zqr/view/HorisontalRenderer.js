qx.Class.define("zqr.view.HorisontalRenderer", {
    extend : qx.ui.form.renderer.Single,
    members : {
        __column : 0,
        __maxRow : 0,

        addItems : function(items, names, title) {
//            var l = this.getLayout();
//            l.setAlignY("middle");
            //var row = 0;
            var col = 0;
            var row = 0;
            // add the header
            if (title != null) {
                this._add(
                    this._createHeader(title), {
                        row: row, column: col, colSpan: 2
                    }
                );
                col++;
            }

            // add the items
            for (var i = 0; i < items.length; i++) {
                var label = this._createLabel(names[i], items[i]);
                label.setAlignY('middle');
                this._add(label, {row: row, column: col});
                var item = items[i];
                label.setBuddy(item);
                if (item instanceof qx.ui.form.RadioGroup) {
                    item = this._createWidgetForRadioGroup(item);
                }
                item.setAlignY('middle');
                this._add(item, {row: row, column: col+1});
                col += 2;
            }
            this.__column = col;
            // save the max row height for the buttons
            this.__maxRow = 0; //< row ? this.__maxRow = row : null;
        },

        addButton : function(button) {
            if (this._buttonRow == null) {
                // create button row
                this._buttonRow = new qx.ui.container.Composite();
                this._buttonRow.setMarginTop(5);
                var hbox = new qx.ui.layout.HBox();
                hbox.setAlignX("right");
                hbox.setSpacing(5);
                this._buttonRow.setLayout(hbox);
                // add the button row
                this._add(this._buttonRow, {row: this.__maxRow, column: this.__column + 1});
            }

            // add the button
            this._buttonRow.add(button);
        }
    }
});

