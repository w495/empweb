/* ************************************************************************
************************************************************************ */

qx.Class.define("zqr.view.Form.Upload.AttachList",
{
    extend : qx.ui.tree.Tree,
 
    /**
     *
     *  @param root --- виджет из которого вызвали.
     *  @param url --- URL запроса.
     *  @param labelFieldName
     *  @param descrFieldName
     *  @param paramdict --- список параметоров url (может быть undefined)
     */
    construct : function(root, url, labelFieldName, descrFieldName, paramdict) {
        this.biz = root;
        this.url = url;
        this.labelFieldName = labelFieldName;
        this.descrFieldName = descrFieldName;
        this.paramdict = paramdict;
        
        this.base(arguments);
        this.setHideRoot(true);
        this.setOpenMode("click");
        this.addListener("changeSelection", this._onMenuSelect, this);

        this.root = new qx.ui.tree.TreeFolder();
        this.setRoot(this.root);
        this.root.setOpen(true);
        this.data = {};
        
    },

    members : {

        _onMenuSelect : function(e) {
        },

        requestItems : function() {
            this._requestItems(this.url);
        },
        
        setParamdict: function(paramdict) {
            this.paramdict = paramdict;
        },
        
        _onIncomeItems : function(response) {
            var result = response.getContent();
            if (false == zqr.util.errors.process(this, result) )
                return false;
            this.data = {};
            this.addItems(result.values);
            if(this.biz["on_selDataLoaded"])
                this.biz.on_selDataLoaded(this);
            return true;
        },

        addItems : function(values) {
            for(var i = 0; i < values.length; i++) {
                var E = values[i];
                if(this.data[E.id] != undefined)
                    continue;
                var Item = new qx.ui.tree.TreeFile();

                var checkbox = new qx.ui.form.CheckBox();
                checkbox.setFocusable(false);
                checkbox.zqr_element = E;
                checkbox.Item = Item;
                Item.addWidget(checkbox);
                
                
                Item.setIcon(null);
                
                Item.addLabel(""+E[this.labelFieldName]);
                
                Item.addWidget(new qx.ui.core.Spacer(), {flex: 1});
                var text = new qx.ui.basic.Label(E[this.descrFieldName]);//alias);
                text.setWidth(150);
                Item.addWidget(text);
               
                this.root.add(Item);
                this.data[E.id] = checkbox;
            }
        },
        
        addItem: function(value) {
            if(this.data[value.id] != undefined)
                return false;
            var Item = new qx.ui.tree.TreeFile();
            
            var checkbox = new qx.ui.form.CheckBox();
            checkbox.setFocusable(false);
            checkbox.zqr_element = value;
            checkbox.Item = Item;
            Item.addWidget(checkbox);
            Item.setIcon(null);
            Item.addLabel("" + value[this.labelFieldName]);
            
            Item.addWidget(new qx.ui.core.Spacer(), {flex: 1});
            var text = new qx.ui.basic.Label(value [this.descrFieldName]);//alias);
            text.setWidth(150);
            Item.addWidget(text);
            
            this.root.add(Item);
            this.data[value.id] = checkbox;
            return true;
        },

        remItems : function(values) {
            var newData = {};
            for(var i=0; i<values.length; i++) {
                var E = values[i];
                if(this.data[E.id] == undefined)
                    continue;
                var checkbox = this.data[E.id];
                
                var req = new qx.io.remote.Request("/update-doc/delete-attach", "POST", "application/json");
                req.setParameter("id", E.id);
                req.send();
                
                this.root.remove(checkbox.Item);
                this.data[E.id] = undefined;
            }

            for(var key in this.data) {
                var E = this.data[key];
                if(E != undefined) 
                    newData[key] = E;
            }
            this.data = newData;
        },

        getSelectedId : function() {
            var ret = [];
            
            for(var key in this.data) {
                var cb = this.data[key];
                if(cb.getValue() == true) {
                    ret.push(cb.zqr_element.id);
                }
            }
            return ret;
        },

        getAllId : function() {
            var ret = [];
            for(var key in this.data) {
                var cb = this.data[key];
                ret.push(cb.zqr_element.id);
            }
            return ret;
        },

        getSelected : function() {
            var ret = [];
            console.log("this.data = ", this.data);
            for(var key in this.data) {
                var cb = this.data[key];
                if(cb.getValue() == true) {
                    ret.push(cb.zqr_element);
                }
            }
            return ret;
        },

        setChecked : function(idList) {
            for(var i in this.data)
                this.data[i].setValue(false);
                
            for(var i=0; i < idList.length; i++) {
                var id=idList[i];
                if(this.data[id] != undefined)
                    this.data[id].setValue(true);
            }
        }
    }
});


