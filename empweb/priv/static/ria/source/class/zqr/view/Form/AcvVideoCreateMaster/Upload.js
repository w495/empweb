/* ************************************************************************
    https://gist.github.com/1639960
    
#asset(qx/icon/Tango/16/actions/document-save.png)
************************************************************************ */

qx.Class.define("zqr.view.Form.AcvVideoCreateMaster.Upload",
{
    extend : zqr.view.Form.AcvVideoCreateMaster.BasePage,
    
    construct : function(uReq, Row, Options) {
        this.Options = Options;
        console.log("this.Options = = ", this.Options);
        this.base(arguments, uReq, Row, Options);
    },

    members : {
        
        /* Upload request берется из конструктора */
        uReq : null,
        
        /* Download request делаем сами*/
        dReq : null,
        
        /**
         * Download  request config
         *
         * Предполагается, что загружать данные каждая страница
         * мастера будет самостоятельно, а вот выгружаться на сервер они будут
         * одним запросом.
         * 
        **/
        drc : {             
            url: "/get-acv-video/upload",
            method: "GET",                  // POST \ GET
            mimetype: "application/json"    // application/json
        },
        
        urc : {  // upload request config
            imgurl: "/update-acv-video/uload-video"
        },
        
        getComposite : function(){
            return this.composite;
        },

        inp : {
            Ref:        null,
            Url:        null,

            Duration:          null
        },
        
        buildForm : function(){
            var RFM = zqr.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
            
            /* Сопровождающая картинка */
            this.refButton = new zqr.view.Form.Upload.UploadButton("uploadfile", null, "icon/16/actions/document-save.png"),
            this.refForm = new zqr.view.Form.Upload.UploadForm('uploadFrm', this.urc.imgurl);
            this.refButton.setForm(this.refForm);
                        
            var layout = new qx.ui.layout.Grid(2, 5);
            layout.setColumnFlex(1, 1);
            layout.setColumnAlign(0, "right", "top");
            
            this.composite  = new qx.ui.container.Composite (layout);
            this.composite.setWidth(zqr.Config.MASTER_FORM_WIDTH);
            
            this.inp.Duration =    new qx.ui.form.Spinner(1, 134217728, 134217728*2);
            this.inp.Link_title =   new qx.ui.form.TextField()
                .set({
                    placeholder: "Текст ссылки",
                    required:true,
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Комментарий к вашей рекламной кaмпании")
                });
            this.inp.Alt_title =    new qx.ui.form.TextField()
                .set({
                    placeholder: "Текст подсказки",
                    required:true,
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Комментарий к вашей рекламной кaмпании")
                });
            
            this.inp.Url = new qx.ui.form.TextField()
                .set({
                    placeholder: "http://example.com/",
                    required:true,
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Должен быть адреc в виде http://example.com")
                    
                });
                
            this.inp.Ref = new qx.ui.form.TextField()
                .set({
                    placeholder: "Выберите файл",
                    readOnly:true,
                    required:true,
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Файл в формате mp4, размер не должен привышать 50Мб")
                     
                });
            
            var pageName = new qx.ui.basic.Label()
                .set({
                    value: "Загрузка видео",  font: "bold",
                    alignX: "left", rich : true
                });
                
            var vertical_offset = -1;
            this.composite.add(pageName, {row:++vertical_offset, column:0, colSpan:2})

            /*
            this.composite.add(new qx.ui.basic.Label().set({value: "Alt_title",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.Alt_title,   {row:vertical_offset, column:1});
            */
            
            this.composite.add(new qx.ui.basic.Label().set({value: "Ссылка по клику",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.Url,   {row:vertical_offset, column:1});
            
            this.composite.add(new qx.ui.basic.Label().set({value: "Текст ссылки",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.Link_title,   {row:vertical_offset, column:1});
            
            if((this.Options) && (this.Options.disabled)){
                this.composite.add(new qx.ui.basic.Label().set({value: "Ссылка",  rich : true}),
                        {row:++vertical_offset, column:0});
                this.composite.add(this.inp.Ref,   {row:vertical_offset, column:1});
            }else{
                this.composite.add(new qx.ui.basic.Label().set({value: "Файл",  rich : true}),
                        {row:++vertical_offset, column:0});
                this.composite.add(this._buildPicFormCnt(),   {row:vertical_offset, column:1});
            }
            
            this.inp.Link_title.focus();
            return this.composite;
        },
        
        /**
            Обработчики событий,
                которые не удалось вынести внутрь
                    отдельных виджетов.
        **/
        addListeners: function() {
            var _this = this;
            /* События виджетов для сопровождающей картикни  */
            this.refButton.addListener('changeFileName',function(e){
                if('' != e.getData()) {
                    _this.inp.Ref.setValue(_this.refButton.getFileName());
                    _this.inp.Ref.setValid(true);
                    if(zqr.view.Form.AbstractForm.customFormChkVideoFileName(_this.inp.Ref)){
                        zqr.view.Form.Upload.UploadFakeStatusBar.on();
                        _this.refForm.setParameter("prev", _this.inp.Ref.getValue());
                        _this.refForm.send();
                    }else{
                        // _this.inp.Ref.setValid(false);
                        //_this.inp.Ref.setInvalidMessage("Должна быть позднее даты начала");
                        return false;
                    }
                }
                return true;
            });
            this.refForm.addListener('completed',function(e) {
                zqr.view.Form.Upload.UploadFakeStatusBar.off();
                var response = _this.refForm.getIframeTextContent();
                console.log("this.__form -> completed 2");
                //_this.setEnabled(true);
                //_this.refButton.setEnabled(true);
                _this.inp.Ref.setValue(response);
            });
        },
        
        /**
            @overload
            Функция блокировки\разблокировки элементов ввода,
            которые не относятся
                к this.inp, и там их нельзя обработать.
        **/
        onChangeEnabled: function(enabled) {
            this.refForm.setEnabled(enabled)
            this.refButton.setEnabled(enabled)
        },
        
        /**
            Создает область загрузки картинки.
        **/
        _buildPicFormCnt: function() {
            var pic_layout = new qx.ui.layout.Grid(12, 6);
            var picFormCnt = new qx.ui.container.Composite(pic_layout).set({
                allowGrowX: true
              });
            if(!this.inp.Ref)
                return picFormCnt;
            
            pic_layout.setColumnFlex(0, 1);
            pic_layout.setColumnAlign(0, "right", "middle");
            picFormCnt.add(this.inp.Ref,  {row:0, column:0});
            this.refForm.setParameter('rm','upload');
            this.refForm.setLayout(new qx.ui.layout.Basic);
            picFormCnt.add(this.refForm, {row:0, column:1});
            this.refForm.add(this.refButton , {left:0,top:0});
            return picFormCnt;
        },
        
        
        /**
            Получает данные с сервера.
        **/
        loadFormData : function(id, paramName) {
            this.dReq = new qx.io.remote.Request
                (this.drc.url, this.drc.method, this.drc.mimetype);
            this.dReq.setTimeout(60000);
            this.dReq.setParameter(paramName, id);
            this.dReq.addListener("completed", this._onLoadFormDataCompl, this);
            this.dReq.send();
        },
        
        _onLoadFormDataCompl : function(response) {
            var result = response.getContent();
            if (false == zqr.util.errors.process(this, result))
                return false;
            this.fillForm(result);
            return true;
        },
        
        /**
            Заполняет форму полученными данными.
        **/
        fillForm : function(data) {
            for(var fieldName in this.inp){
                var item = fieldName.toLowerCase();
                if("duration" == item)
                    this.inp[fieldName].setValue(parseInt(data.value[item]));
                this.inp[fieldName].setValue(data.value[item]);
            }
        },
        
        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            var flag = true;

            var url = this.inp.Url.getValue() || "";
            if (!RegExp("^[a-zA-Z]{3,7}\:\/\/").test(url)){
                this.inp.Url.setValue("http://"+url);
            } 
            
            flag &= zqr.view.Form.AbstractForm.customFormChkUrl(this.inp.Url);
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(10, 256, this.inp.Url);
            
           /* flag &= zqr.view.Form.AbstractForm.customFormChkSymb(this.inp.Link_title);*/
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(3, 256, this.inp.Link_title);
            
            flag &= zqr.view.Form.AbstractForm.customFormChkVideoFileName(this.inp.Ref);
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(1, 1024, this.inp.Ref); 

            return flag;
        },
        
        /**
            Применив некоторые преобразования <<загружает>> данные на сервер
        **/
        saveData : function(e) {
            this.base(arguments, e);

            var formIsValid = this.validateForm();
            if(formIsValid){
                var res = {}
                for(var fieldName in this.inp){
                    item = fieldName.toLowerCase()
                    res[item] = this.inp[fieldName].getValue();
                }  
                for(var item in res){
                    this.uReq.setParameter(item, res[item], true);
                }
            }
            return formIsValid;
        }
    }
});

