/* ************************************************************************
************************************************************************ */

qx.Class.define("zqr.view.SelListTree",
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
        if(this.paramdict){
            if(this.paramdict.descrlen)
                this.descrlen = this.paramdict.descrlen;
        }

        this.base(arguments);
        this.setHideRoot(true);
        this.setOpenMode("click");
        this.addListener("changeSelection", this._onMenuSelect, this);

        this.root = new qx.ui.tree.TreeFolder();
        this.setRoot(this.root);
        this.root.setOpen(true);
        this.data = {};
        
        if(this.url)
            this._requestItems(this.url);
        
    },

    members : {

        descrlen: 200,
        
        setDescrlen: function(len) {
            this.descrlen = len;
        },

        getDescrlen: function(len) {
            return this.descrlen;
        },

        _onMenuSelect : function(e) {

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

        resetFun : function(callback, owner) {
            this.setHideRoot(true);
            this.setOpenMode("click");

            this.root = new qx.ui.tree.TreeFolder();
            this.setRoot(this.root);
            this.root.setOpen(true);
            this.data = {};
            if(this.url)
                this._requestItemsFun(this.url, callback, owner||this);
        },

        _requestItems : function(Url) {
            var req = new qx.io.remote.Request(Url, "GET", "application/json");
            for(var key in this.paramdict){
                req.setParameter(key, this.paramdict[key]);
            }
            req.addListener("completed", this._onIncomeItems, this);
            req.send();
        },

        _requestItemsFun : function(Url, callback, owner) {
            var req = new qx.io.remote.Request(Url, "GET", "application/json");
            for(var key in this.paramdict){
                req.setParameter(key, this.paramdict[key]);
            }
            if(!owner)
                owner = this
            req.addListener("completed", this._onIncomeItemsFun(callback, owner || this), this);
            req.send();
        },
                
        setParamdict: function(paramdict) {
            this.paramdict = paramdict;
        },

        _onIncomeItemsFun : function(callback, owner ) {
            return function(response) {
                var result = response.getContent();
                if (false == zqr.util.errors.process(this, result) )
                    return false;
                this.data = {};
                this.addItems(result.values);
                if(this.biz["on_selDataLoaded"])
                    this.biz.on_selDataLoaded(this);
                callback.apply(owner || this, [response])
                return true;
            }
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
                var element = values[i];
                if(this.data[element.id] != undefined)
                    continue;
                var Item = new qx.ui.tree.TreeFile();
                var checkbox = new qx.ui.form.CheckBox();
                checkbox.setFocusable(false);
                checkbox.zqr_element = element;
                checkbox.Item = Item;
                Item.setIcon(null);
                Item.addWidget(checkbox);
                Item.addLabel("" + element[this.labelFieldName]);
                
                Item.addWidget(new qx.ui.core.Spacer(), {flex: 1});
                
                var descr = new qx.ui.basic.Label(element[this.descrFieldName]);//alias);
                descr.setWidth(this.getDescrlen());
                
                Item.addWidget(descr);
                
                this.root.add(Item);
                this.data[element.id] = checkbox;
            }
        },
        
        remItem : function(value) {
            var newData = {};

            var element = value;
            if(this.data[value.id] == undefined)
                return false;
            
            var checkbox = this.data[value.id];
            this.root.remove(checkbox.Item);
            this.data[value.id] = undefined;


            for(var key in this.data) {
                var element = this.data[key];
                if(element != undefined) 
                    newData[key] = element;
            }
            this.data = newData;
            return true;
        },
        
        remItems : function(values) {
            var newData = {};
            for(var i=0; i<values.length; i++) {
                var element = values[i];
                if(this.data[element.id] == undefined)
                    continue;
                var checkbox = this.data[element.id];
                this.root.remove(checkbox.Item);
                this.data[element.id] = undefined;
            }

            for(var key in this.data) {
                var element = this.data[key];
                if(element != undefined) 
                    newData[key] = element;
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

        __setFalseAll: function() {
            for(var i in this.data)
                this.data[i].setValue(false);
        },

        setCheckedObj : function(objList) {
            this.__setFalseAll();
            for(var i=0; i < objList.length; i++) {
                console.log("objList[", i, "].id = ", objList[i].id);
                var id=objList[i].id;
                if(this.data[id] != undefined)
                    this.data[id].setValue(true);
            }
        },

        setChecked : function(idList) {
            this.__setFalseAll();
            for(var i=0; i < idList.length; i++) {
                var id=idList[i];
                if(this.data[id] != undefined)
                    this.data[id].setValue(true);
            }
        }
    }
});


