/* ************************************************************************

#asset(qx/icon/Tango/16/actions/document-save.png)
#asset(qx/icon/Tango/16/actions/list-add.png)

************************************************************************ */


// Добавить тип
qx.Class.define("zqr.view.Form.Upload.AttachWindow",
{
    extend : qx.ui.window.Window,
  
    events:
    {
        "sending"    :  "qx.event.type.Data",
        "completed"  :  "qx.event.type.Data"
    },
    
    construct: function(list)
    {
        this.base(arguments, "Добавление вложения",
                  "icon/16/actions/list-add.png");
        this.submitButton =  new qx.ui.form.Button("Отправить");
        this.submitButton.addListener("execute", this._onSubmitClick, this);
        this.cancelButton = new qx.ui.form.Button("Отмена");
        this.cancelButton.addListener("execute", this._onCancelClick, this);
        this.buildWindow();
        this.addListeners();
        this.fillWindow();
    },
  
    // ------------------------------------------------------------------------
    // [Destructor]
    // ------------------------------------------------------------------------
  
    destruct: function()
    {
  
    },
  
    members:
    {
        submitButton: null,
        cancelButton: null,
        
        fileForm: null,
        
        doc_id: 0,
        
        inp: {
            Id : null,
            Name : null,
            Alt : null,
            Doc_id : null,
            Attach_type_id : null,
            Url : null,
            Updater : null
        },
        
        fileText    : null,
        fileButton  : null,
        fileForm    : null,

        fake_inp:{
            Type: null
        },  
        // TODO это точно надо брать с сервера.
        attach_types: {
            "1": 'file',
            "2": 'video',
            "3": 'image',
            "4": 'link'
        },

        urc : {  // upload request config
            url: "/update-doc/upload-attach",
            form_id: "uploadattach",
            button_id: "uploadfile"
        },
        
        setId: function(doc_id){
           this.doc_id = doc_id;
        },
        
        buildWindow: function()
        {  
            this.inp.Name  = new qx.ui.form.TextField();
            this.inp.Alt   = new qx.ui.form.TextField();
            this.inp.Attach_type_id = new qx.ui.form.TextField("1");
            this.fake_inp.Type = new qx.ui.form.SelectBox();
            this.fileText = new qx.ui.form.TextField();
            this.fileButton = new zqr.view.Form.Upload.UploadButton(
                this.urc.button_id,
                null,
                "icon/16/actions/document-save.png"
            );
            this.fileForm = new zqr.view.Form.Upload.UploadForm(
                this.urc.form_id,
                this.urc.url
            );
            
            this.setLayout(new qx.ui.layout.VBox(10));
            this.setShowMinimize(false);
            this.setShowMaximize(false);
            var layout = new qx.ui.layout.Grid(3, 1);
            var cnt = new qx.ui.container.Composite(layout);
            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");
            var vertical_offset = 0;
            cnt.add(new qx.ui.basic.Label()
                    .set({value: "Имя:",  rich : true}),
                        {row:++vertical_offset, column:0});
            cnt.add(new qx.ui.basic.Label()
                    .set({value: "Описание:",  rich : true}),
                        {row:++vertical_offset, column:0});
            cnt.add(new qx.ui.basic.Label()
                    .set({value: "Файл:",  rich : true}),
                        {row:++vertical_offset, column:0});
            cnt.add(new qx.ui.basic.Label()
                    .set({value: "Тип:",  rich : true}),
                        {row:++vertical_offset, column:0});
            vertical_offset = 0;
            cnt.add(this.inp.Name, {row:++vertical_offset , column:1});
            cnt.add(this.inp.Alt, {row:++vertical_offset , column:1});
            var file_layout = new qx.ui.layout.Grid(12, 6);
            var fileFormCnt = new qx.ui.container.Composite(file_layout);
            file_layout.setColumnFlex(0, 1);
            file_layout.setColumnAlign(0, "left", "middle");
            
            this.fileForm.setParameter('rm','upload');
            this.fileForm.setLayout(new qx.ui.layout.Basic);
            this.fileForm.add(this.fileButton);
            
            fileFormCnt.add(this.fileText ,  {row:0, column:0});
            fileFormCnt.add(this.fileForm, {row:0, column:1});
        
            cnt.add(fileFormCnt, {row:++vertical_offset , column:1});
            cnt.add(this.fake_inp.Type, {row:++vertical_offset , column:1});
            
            var buttonRow = new qx.ui.container.Composite();
            buttonRow.setMarginTop(5);
            var hbox = new qx.ui.layout.HBox(5);
            hbox.setAlignX("right");
            buttonRow.setLayout(hbox);
            buttonRow.add(this.submitButton);
            buttonRow.add(this.cancelButton);
            cnt.add(buttonRow, {row:++vertical_offset , column:0, colSpan:3});
            
            this.add(cnt);    
            this.inp.Name.focus();
        },
      
        _onSubmitClick   : function(e){
            var data = {
                id:             this.id,
                doc_id:         this.doc_id,
                name:           this.inp.Name.getValue() ,
                alt:            this.inp.Alt.getValue(),
                attach_type_id: this.inp.Attach_type_id.getValue()
            };
    
            if(!data.name) data.name            = "Вложение";
            if(!data.alt) data.alt              = "Вложение";
            if(!data.attach_type_id) data.attach_type_id   = 1;
            
            
            if(!this.fileText.getValue() || "" == this.fileText.getValue()){
                return this.close();
                /* alert("Вы обязаны заполнить все поля"); */
            }
            
            for(var item in data){                
                this.fileForm.setParameter(item, data[item]);
            }
            return this.fileForm.send();
        },
        
        _onCancelClick   : function(e){
            this.close();
        },
        
        addListeners: function(){
            var _this = this;
            this.addListener("appear",function(){ this.center(); }, this);
            this.addListener('completed',function(e){            
                for(var item in this.inp){
                    if(_this.inp[item])
                    {
                        _this.inp[item].resetValue();
                    }
                }
            });
            this.fileButton.addListener('changeFileName',function(e){
                if(e.getData()!='') {
                    _this.fileText.setValue(_this.fileButton.getFileName())
                }
            });
            this.fileForm.addListener('completed',function(e){
                var response = _this.fileForm.getIframeTextContent();
                _this.id = response;
                var data = {
                    id:             _this.id,
                    doc_id:         _this.doc_id,
                    name:           _this.inp.Name.getValue(),
                    alt:            _this.inp.Alt.getValue(),
                    attach_type_id: _this.inp.Attach_type_id.getValue()
                };
                _this.fireDataEvent("completed", data);
                _this.close();
            });
            this.fake_inp.Type.addListener("changeSelection", function(e) {
                var type_id = parseInt(e.getData()[0].getModel());
                _this.inp.Attach_type_id.setValue("" + type_id);
            });
        },
        
        fillWindow: function(){
            var defaultItem = null;
            for (var type in this.attach_types)
            {
                var tempItem = new qx.ui.form.ListItem(
                    this.attach_types[type], null, type);
                if(!defaultItem)
                    defaultItem = tempItem;
                this.fake_inp.Type.add(tempItem);
            }
            this.fake_inp.Type.setSelection([defaultItem]);            
        }
    }
});
