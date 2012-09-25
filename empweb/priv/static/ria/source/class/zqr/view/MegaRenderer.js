qx.Class.define("zqr.view.MegaRenderer",
{
  extend : qx.ui.form.renderer.AbstractRenderer,


  construct : function(form) {
    //var layout = new qx.ui.layout.VBox(6);
    this.layout = new qx.ui.layout.Grid();
    this.layout.setSpacing(6);
//    layout.setColumnFlex(0, 1);
//    layout.setColumnAlign(0, "right", "top");
    this._setLayout(this.layout);


    this.base(arguments, form);
  },


    members : {
        _row : 0,
        _buttonRow : null,


    addItems : function(items, names, title) {
        if(this.columnCount == undefined) { // первичная задача количества колонок в главной таблице
            if(title.columns==undefined)
                throw("ошибка мегарендерера 1");
            this.columnCount = title.columns;
            for(var i=0; i < this.columnCount; i++) {
                this.layout.setColumnFlex(i*2, 1);
                this.layout.setColumnAlign(i*2, "right", "top");
            }
            return;
        }

        var span = title.span || false;

        var column = 0;

        var i;
        for (i = 0; i < items.length; i++) {
            var label = this._createLabel(names[i], items[i]);
            this._add(label, {row: this._row, column: column*2});
            var item = items[i];
            label.setBuddy(item);
            if(span) {
                this._add(item, {row: this._row, column: column * 2 + 1, colSpan: this.columnCount*2-1});
                this._row++;
                column = 0;
            }
            else {
                this._add(item, {row: this._row, column: column * 2 + 1});
                column++;
                if(column == this.columnCount) {
                    this._row++;
                    column = 0;
                }
            }
        }
        if(span == false && (i)%this.columnCount !=0)
            this._row++;
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
        this._add(this._buttonRow, {row: this._row, column: 0, colSpan: this.columnCount*2});
        // increase the row
        this._row++;
      }

      // add the button
      this._buttonRow.add(button);
    },


    getLayout : function() {
      return this._getLayout();
    },


    _createLabel : function(name, item) {
      var required = "";
      if (item.getRequired()) {
       required = " <span style='color:red'>*</span> ";
      }

      // Create the label. Append a colon only if there's text to display.
      var colon = name.length > 0 || item.getRequired() ? " :" : "";
      var label = new qx.ui.basic.Label(name + required + colon);
      label.setRich(true);
      return label;
    },


    _createHeader : function(title) {
      var header = new qx.ui.basic.Label(title);
      header.setFont("bold");
      if (this._row != 0) {
        header.setMarginTop(10);
      }
      header.setAlignX("left");
      return header;
    }
  }
});
