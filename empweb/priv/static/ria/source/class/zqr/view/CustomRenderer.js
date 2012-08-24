qx.Class.define("zqr.view.CustomRenderer", {
  extend : qx.ui.form.renderer.Single,
  members : {
    __column : 0,
    __maxRow : 0,

    addItems : function(items, names, title) {
      var row = 0;
      // add the header
      if (title != null) {
        this._add(
          this._createHeader(title), {
            row: row, column: this.__column, colSpan: 2
          }
        );
        row++;
      }

      // add the items
      for (var i = 0; i < items.length; i++) {
        var label = this._createLabel(names[i], items[i]);
        this._add(label, {row: row, column: this.__column});
        var item = items[i];
        label.setBuddy(item);
        if (item instanceof qx.ui.form.RadioGroup) {
          item = this._createWidgetForRadioGroup(item);
        }
        this._add(item, {row: row, column: this.__column + 1});
        row++;
      }
      this.__column += 2;
      // save the max row height for the buttons
      this.__maxRow < row ? this.__maxRow = row : null;
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
        this._add(this._buttonRow, {
          row: this.__maxRow, column: 0, colSpan: this.__column
        });
      }

      // add the button
      this._buttonRow.add(button);
    }
  }
});

