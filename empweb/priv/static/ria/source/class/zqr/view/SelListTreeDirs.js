/* ************************************************************************
************************************************************************ */

qx.Class.define("zqr.view.SelListTreeDirs",
{
    extend : qx.ui.tree.Tree,
 
    /**
     *
     *  @param root --- виджет из которого вызвали.
     *  @param url --- url запроса.
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
        this.setOpenMode("dblclick");
        this.addListener("changeSelection", this._onMenuSelect, this);

        this.root = new qx.ui.tree.TreeFolder();
        this.setRoot(this.root);
        this.root.setOpen(true);
        this.data = {};
        
        console.log("this.url", this.url);
        
        if(this.url)
            this._requestItems(this.url);
        
    },

    members : {
        
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
        
        _requestItems : function(url) {
            var req = new qx.io.remote.Request(url, "GET", "application/json");
            for(var key in this.paramdict){
                req.setParameter(key, this.paramdict[key]);
            }
            req.addListener("completed", this._onIncomeItems, this);
            req.send();
        },
        
        _requestItemsDir : function(url, id) {
            var req = new qx.io.remote.Request(url, "GET", "application/json");
            for(var key in this.paramdict){
                req.setParameter(key, this.paramdict[key]);
            }
            req.setParameter("id", id);
            req.addListener("completed", this._onIncomeItemsGen(id), this);
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

        _onIncomeItemsGen : function(id) {
            var this_ = this;
            return function(response) {
                var result = response.getContent();
                this_.addItemsDir(this_.data[id].item, result.values);
                return true;
            }
        },
        

        addItems : function(values) {
            return this.addItemsDir(this.root, values, true);
        },
        
        addItemsDir : function(a_root, values, useadditemslst) {
            console.log("addItemsDir : function(a_root, values)");
            for(var i in values) {
                if(this.data[values[i].id] != undefined)
                    continue;
                var item = this.mkItem(values[i])
                
                //if(useadditemslst)
                this.addItemsLst(item);
                    
                a_root.add(item);
                /*
                    var radd  = a_root.add
                    radd.apply(radd,[item]);
                    radd.apply(this,[item]);
                */
            }
        },
        
        addItemsLst: function(item) {
            item._holder = this;
            item.addListenerOnce("click", function(){
                console.log("click(item = ", item, " this.element = ", this.element, ")");
                this._holder._requestItemsDir("/get-cities", this.element.id);
            }, item);
            item.checkbox.addListener("execute", function(){
                if(this.item.getParent()){
                    console.log("this.item.getParent() = ", this.item.getParent())
                    if(this.item.getParent().checkbox){
                        console.log("this.item.getParent().checkbox = ", this.item.getParent().checkbox)
                        this.item.getParent().checkbox.setValue(false);
                    }
                }
                var children = this.item.getChildren();
                for(var kc in children){
                    var child = children[kc]
                    console.log("child = ", child);
                    child.checkbox.setValue(false);
                }
            }, item.checkbox);
        },
        
        mkItem : function(element) {
            var item = new qx.ui.tree.TreeFolder();
            var checkbox = new qx.ui.form.CheckBox();
            checkbox.setFocusable(false);
            checkbox.zqr_element = element;
            checkbox.item = item;
            item.setIcon(null);
            item.addWidget(checkbox);
            item.addLabel("" + element[this.labelFieldName]);
            //item.addWidget(new qx.ui.core.Spacer(), {flex: 1});
            //var text = new qx.ui.basic.Label(element[this.descrFieldName]);//alias);
            //text.setWidth(150);
            //item.addWidget(text);
            this.data[element.id] = checkbox;
            item.element = element;
            item.checkbox = checkbox;
            return item;
        },
        
        remItem : function(value) {
            var newData = {};
            var element = value;
            if(this.data[value.id] == undefined)
                return false;
            var checkbox = this.data[value.id];
            this.root.remove(checkbox.item);
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
                this.root.remove(checkbox.item);
                this.data[element.id] = undefined;
            }

            for(var key in this.data) {
                var element = this.data[key];
                if(element != undefined) 
                    newData[key] = element;
            }
            this.data = newData;
        },

        getCheckedId : function() {
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
        },
        
        configureTreeItem : function(treeItem, vLabel, vIcon)
        {
          // A left-justified icon
          if (Math.floor(Math.random() * 4) == 0) {
            var img = new qx.ui.basic.Image("icon/16/status/dialog-information.png");
            treeItem.addWidget(img);
          } else {
            treeItem.addWidget(new qx.ui.core.Spacer(16, 16));
          }
    
          // Here's our indentation and tree-lines
          treeItem.addSpacer();
    
          if (treeItem instanceof qx.ui.tree.TreeFolder) {
            treeItem.addOpenButton();
          }
    
          // The standard tree icon follows
          treeItem.addIcon();
          treeItem.setIcon(arguments.length >= 3 ? vIcon : "icon/16/places/user-desktop.png");
          
          // A checkbox comes right after the tree icon
          var checkbox = new qx.ui.form.CheckBox();
          checkbox.setFocusable(false);
          treeItem.addWidget(checkbox);
    
          // The label
          treeItem.addLabel(vLabel);
    
          // All else should be right justified
          treeItem.addWidget(new qx.ui.core.Spacer(), {flex: 1});
    
          // Add a file size, date and mode
          var text = new qx.ui.basic.Label(Math.round(Math.random() * 100) + "kb");
          text.setWidth(50);
          treeItem.addWidget(text);
    
          text = new qx.ui.basic.Label("May " + Math.round(Math.random() * 30 + 1) + " 2005");
          text.setWidth(150);
          treeItem.addWidget(text);
    
          text = new qx.ui.basic.Label("-rw-r--r--");
          text.setWidth(80);
          treeItem.addWidget(text);
    
          return treeItem;
        }

    }
});


