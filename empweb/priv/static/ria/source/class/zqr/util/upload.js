/**
 * File Upload Widget
 * 
 * @author Frederic Fournaise (http://frederic.fournaise.free.fr/qooxdoo-file-upload-widget.php)
 * @version 20070528
 * @copyright   Frederic Fournaise
 * @license  GNU Lesser General Public License, see http://www.opensource.org/licenses/lgpl-license.php 
 *
 **/
 
qx.Class.define("zqr.util.upload",{
    extend : qx.ui.container.Composite,

    construct : function(vUploadCGI, params){
        this.base(arguments, new qx.ui.layout.HBox());
        this.upload = new qx.ui.groupbox.GroupBox(this.tr("Загрузка фильма"));
        this.upload.setLayout(new qx.ui.layout.VBox());
//        this.upload.set({width  : 300 });
        this.add(this.upload, {flex:1});

        var vUniqueId = (new Date).valueOf();
        var vFrameName = "frame_" + vUniqueId;
        this.uploadFrm = "upload_" + vUniqueId;
        this.inputfile=null;
        
        //form
        this.input=new qx.ui.embed.Html();
        //this.input.set({ height : 40, width  : 500 });
        var html = '<form enctype="multipart/form-data" target="'+vFrameName+'" action="'+vUploadCGI+'" method="post" id="'+this.uploadFrm+'" name="'+this.uploadFrm+'">';
/*        for(var key in params) {
            html += '<input type="hidden" value="' + params[key] + '" id="' + key +'"/>';
        }*/
        html += '</form>';
        this.input.setHtml(html);
        this.input.setHeight(25*(params.length + 1));
//        this.input.setHtml('<form enctype="multipart/form-data" target="'+vFrameName+'" action="'+vUploadCGI+'" method="post" id="'+this.uploadFrm+'" name="'+this.uploadFrm+'">'; 
//        this.input.setHtml('<form enctype="multipart/form-data" target="'+vFrameName+'" action="'+vUploadCGI+'" method="post" id="'+this.uploadFrm+'" name="'+this.uploadFrm+'"></form>');
        this.upload.add(this.input);
        
        this.addListener("appear", this._createInput,this);
        this.params = params;
            
        // iframe
        if (qx.bom.client.Engine.MSHTML) {
          this._iframeNode = document.createElement('<iframe name="' + vFrameName + '"></iframe>');
        } else {
          this._iframeNode = document.createElement("iframe");
        }
        this._iframeNode.src = "javascript:void(0)";
        this._iframeNode.id = this._iframeNode.name = vFrameName;
        this._iframeNode.style.display = "none";
        document.body.appendChild(this._iframeNode);
        var o = this;
        this._iframeNode.onload = function(e) {
          return o._onload(e);
        };
        this._iframeNode.onreadystatechange = function(e) {
          return o._onreadystatechange(e);
        };
  },
  
    events: {
        "sending" : "qx.event.type.Data",
        "completed" : "qx.event.type.Data"
    },
  
    properties :{
        isSent : {init:false, check: "Boolean"}
    },
  
    members :{

        _createInput : function(e){
            if (this.inputfile==null){
                this.inputfile = document.createElement("input");
                this.inputfile.type = "file";
                this.inputfile.name = "upload";
                this.inputfile.multiple=true;
                var _this = this;
                this.inputfile.onchange = function(e) {
                    var obj = this;
                    return _this._onchange(e);
                };
                
                for(var i=0; i<this.params.length; i++) {
                    var P = this.params[i];
                    var toAdd;
                    if(P.type == "select") {
                        var sel = document.createElement("select");

                        sel.style.width = "150px";
                        sel.name = P.name;
                        sel.name = P.name;
                        if(P.str) {
                            var pl = document.createElement("div");
                            pl.appendChild(sel);
                            var z =  document.createElement("span");
                            z.innerHTML = P.str;
                            pl.appendChild(z);
                            toAdd = pl;
                        }
                        else
                            toAdd = sel;

                        for(var j=0; j<P.options.length; j++) {
                            var O = P.options[j];
                            var option = document.createElement("option");
                            option.value = O.value;
                            option.innerHTML = O.alias;
                            sel.appendChild(option);
                        }
                    }
                    else {
                        var inp = document.createElement("input");
                        inp.type = P.type;
                        inp.value = P.value;
                        inp.name = P.name;
                        if(P.str) {
                            var pl = document.createElement("div");
                            pl.appendChild(inp);
                            var z =  document.createElement("span");
                            z.innerHTML = P.str;
                            pl.appendChild(z);
                            toAdd = pl;
                        }
                        else
                            toAdd = inp;
                    }
                    document.getElementById(this.uploadFrm).appendChild(toAdd);
                }
                document.getElementById(this.uploadFrm).appendChild(this.inputfile);

            }
        },
    
        _onchange : function(e){
        },

        submit : function() {
            var d=document.forms[this.uploadFrm];
            this.elements = d.elements;
            d.submit();
            this._isSent = true;
            this.fireDataEvent("sending", ["any data here"]);
//            d.reset();
        },
        
        _onreadystatechange : function(){
            if (this._iframeNode.readyState == "complete" && this._isSent){
                this.fireDataEvent("completed", ["any data here"]);
            }
        },

        _onload : function(){
            if (this._isSent){
                this.fireDataEvent("completed", ["any data here"]);
            }
        }

    }
});
