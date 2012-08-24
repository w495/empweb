/* ************************************************************************
************************************************************************ */

qx.Class.define("zqr.view.SortedSelListTree",
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

        this.pattern = "";
        this.string = "";
        
        this.root = new qx.ui.tree.TreeFolder();
        
        this.root.setHeight(100);
        this.setHeight(100);
        
        this.setRoot(this.root);
        this.root.setOpen(true);
        this.data = {};
        this.shadow_data = {};
        
        if(this.url)
            this._requestItems(this.url);
    },

    members : {

        _onMenuSelect : function(e) {
            
        },
        
        setPattern: function(pattern) {
            this.pattern = pattern;
        },
        
        getPattern: function() {
            return this.pattern;
        },
        
        testItems : function(string) {
            var PATTERN_LENGTH_DIFF = 4;
            var REGEXP_ADDITIONS = "^";
            
            /*
            if(Math.abs(this.string.length
                        - string.length
                        - REGEXP_ADDITIONS.length) < PATTERN_LENGTH_DIFF){
                console.log("this.pattern = ", this.string);
                console.log("     pattern = ",      string);
                return false;
            }
            */
            
            
            console.log("this.pattern = ", this.string, "     pattern = ", string);
            
            this.string = string;
            this.pattern = RegExp(REGEXP_ADDITIONS + string );
            //this.data = zqr.util.utils.clone(this.shadow_data);
            
            for(var key in this.shadow_data){
                var checkbox = this.shadow_data[key];
                var value = checkbox.zqr_element;
                var name = value.name;
                var description = value.description;
                // checkbox.show();
                // checkbox.Item.show();
                if(!this.pattern.test(name)&&!this.pattern.test(description)){
                    this.remItem(value);
                    // checkbox.hide();
                    // checkbox.Item.hide();
                }else{
                    if(!this.data[key]){
                        var Item = new qx.ui.tree.TreeFile();
                        var checkbox = new qx.ui.form.CheckBox();
                        checkbox.setFocusable(false);
                        checkbox.zqr_element = value;
                        checkbox.Item = Item;
                        Item.setIcon(null);
                        Item.addWidget(checkbox);
                        Item.addLabel("" + value[this.labelFieldName]);
                        Item.addWidget(new qx.ui.core.Spacer(), {flex: 1});
                        var text =
                            new qx.ui.basic.Label(value[this.descrFieldName]);
                        text.setWidth(150);
                        Item.addWidget(text);
                        this.root.add(Item);
                        this.data[value.id] = checkbox;
                    }
                }
            }
            return true;
        },
        
        reset : function() {
            this.setHideRoot(true);
            this.setOpenMode("click");

            this.root = new qx.ui.tree.TreeFolder();
            this.setRoot(this.root);
            this.root.setOpen(true);
            this.data = {};
            if(this.url)
                this._requestItems(this.url);
        },

        requestItems : function() {
            this._requestItems(this.url);
        },
        
        _requestItems : function(Url) {
            var req = new qx.io.remote.Request(Url, "GET", "application/json");
            for(var key in this.paramdict){
                req.setParameter(key, this.paramdict[key]);
            }
            req.addListener("completed", this._onIncomeItems, this);
            req.send();
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
            for(var i=0; i<values.length; i++) {
                var E = values[i];
                if(this.data[E.id] != undefined)
                    continue;
                var Item = new qx.ui.tree.TreeFile();
                var checkbox = new qx.ui.form.CheckBox();
                checkbox.setFocusable(false);
                checkbox.zqr_element = E;
                checkbox.Item = Item;
                Item.setIcon(null);
                Item.addWidget(checkbox);
                Item.addLabel(""+E[this.labelFieldName]);
                // Item.addWidget(new qx.ui.core.Spacer(), {flex: 1});
                // var text = new qx.ui.basic.Label(E[this.descrFieldName]);
                // text.setWidth(150);
                // Item.addWidget(text);
                
                this.root.add(Item);
                this.data[E.id] = checkbox;
                this.shadow_data[E.id] = checkbox;
            }
        },
        
        hideItem : function(value) {
            
        },
        
        showAllItems : function() {
            return true;
        },
        
        remItem : function(value) {
            var newData = {};

            var E = value;
            if(this.data[value.id] == undefined)
                return false;
            
            var checkbox = this.data[value.id];
            this.root.remove(checkbox.Item);
            this.data[value.id] = undefined;
            
            for(var key in this.data) {
                var E = this.data[key];
                if(E != undefined) 
                    newData[key] = E;
            }
            this.data = newData;
            return true;
        },
        
        remItems : function(values) {
            var newData = {};
            for(var i=0; i<values.length; i++) {
                var E = values[i];
                if(this.data[E.id] == undefined)
                    continue;
                var checkbox = this.data[E.id];
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


