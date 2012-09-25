/* ************************************************************************

    Класс описания формы по созданию пользователя
    ИСПОЛЬЗОВАНИЕ:
        Администрировнаие > Пользователи >  [Создать] | [Редактировать]

************************************************************************ */

// Функция определяет метод startswith для строк
//if (typeof String.prototype.startsWith != 'function') {
//	  String.prototype.startsWith = function (str){
//		  return this.indexOf(str) == 0;
//	  };
//}


qx.Class.define("zqr.view.Form.CustomerProfileForm",
{
    extend : zqr.view.Form.BaseForm,

    construct : function(controller, Row) {
    	/* 
    		Изящное реиспользование кода)))
    	*/
        Row={id:0};
        this.base(arguments, controller, Row);

        //this.submitButton =  new qx.ui.form.Button("Сохранить");
        //this.submitButton.addListener("execute", this._onSubmitClickChild, this); 
            
        this.addListeners(); 
    },

    members : {    
        urc : {   
            url: "/update-customer-profile",
            method: "POST",
            imgurl: "/update-customer/upload-image",
            mimetype: "application/json"
        },

        drc : {  
            url: "/get-customer-profile",
            method: "GET",
            mimetype: "application/json"
        },
        
        /* Поля формы */
        inp : { 
            Id              : null,
            Login           : null,
            Password1       : null,
            Password2       : null,
            Email           : null,
            City            : null,
            Organization    : null,
            Position        : null,
            Firstname       : null,
            Lastname        : null,
            Patronimic      : null,
            Description     : null,
            Pic_url         : null,
            Telephone_number: null, 
            Address         : null,
            Inn             : null,
            Kpp             : null,
            Legal_type      : null
        },

        /**
            Строит визуальное представление формы
            TODO: отрефакторить, так чтобы было мало букаф
        **/
        
        /* Виджеты для сопровождающей картикни */
        picForm: null,
        picButton: null,
        
//      Инфо о юридисеском пользователе
        legalInfo: null, 
        
        buildForm : function() {
            this.base(arguments); 
            this.inp.Id           = new qx.ui.form.TextField();
            this.inp.Login        = new qx.ui.form.TextField();
            this.inp.Legal_type       = new qx.ui.form.TextField();
            this.inp.Password1    = new qx.ui.form.PasswordField()
                .set({
                        placeholder: "pass"
                });
            this.inp.Password2    = new qx.ui.form.PasswordField()
                .set({
                        placeholder: "pass"
                });
            this.inp.Email        = new qx.ui.form.TextField()
                .set({
                        placeholder: "abc@def.gh",
                        required:true
                });
            this.inp.City         = new qx.ui.form.TextField()
                .set({
                        placeholder: "Город"
                });
            this.inp.Organization = new qx.ui.form.TextField()
                .set({
                        placeholder: "Организация"
                });
            this.inp.Position     = new qx.ui.form.TextField()
                .set({
                        placeholder: "Должность"
                });
            this.inp.Firstname    = new qx.ui.form.TextField()
                .set({
                        placeholder: "Ваше имя"
                });
            this.inp.Lastname     = new qx.ui.form.TextField()
                .set({
                        placeholder: "Фамилия"
                });
            this.inp.Patronimic   = new qx.ui.form.TextField()
                .set({
                        placeholder: "Отчество"
                });
            this.inp.Pic_url      = new qx.ui.form.TextField()
                .set({
                        placeholder: "файл",
                        readOnly:true
                });
                
            this.inp.Telephone_number      = new qx.ui.form.TextField()
                .set({placeholder: "Номер телефона", readOnly:false}); 
            
            this.inp.Legal_type = new qx.ui.form.CheckBox("");
        
            
            /* Сопровождающая картинка */
            this.picButton = new zqr.view.Form.Upload.UploadButton("uploadfile", null, "icon/16/actions/document-save.png"),
            this.picForm = new zqr.view.Form.Upload.UploadForm('uploadFrm', this.urc.imgurl);

            var layout = new qx.ui.layout.Grid(16, 6);
            var cnt = new qx.ui.container.Composite(layout); 

            layout.setColumnFlex(0, 1);
            layout.setColumnWidth(1, 130) ;
            layout.setColumnAlign(0, "right", "top");

            var l1 = new qx.ui.basic.Label("Общая информация");
            l1.setFont("bold");
            l1.setAlignX("left");
            cnt.add(l1, {row:0, column:0, colSpan:2});

            var vertical_offset = 0;
            var RFM = zqr.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;
 
            
            cnt.add(new qx.ui.basic.Label().set({value: "Пароль" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.Password1,{row:vertical_offset , column:1}); 
            cnt.add(new qx.ui.basic.Label().set({value: "Повторите пароль" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.Password2,{row:vertical_offset , column:1}); 
            cnt.add(new qx.ui.basic.Label().set({value: "Фотография:" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this._buildPicFormCnt(), {row:vertical_offset, column:1}); 
            cnt.add(new qx.ui.basic.Label().set({value: "E-mail" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.Email, {row:vertical_offset , column:1}); 
            cnt.add(new qx.ui.basic.Label().set({value: "Имя"+ RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.Firstname, {row:vertical_offset , column:1}); 
            cnt.add(new qx.ui.basic.Label().set({value: "Отчество" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.Patronimic, {row:vertical_offset , column:1}); 
            cnt.add(new qx.ui.basic.Label().set({value: "Фамилия" + RFM,  rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.Lastname, {row:vertical_offset , column:1});  
            cnt.add(new qx.ui.basic.Label().set({value: "Город", rich : true}),  {row:++vertical_offset, column:0});
            cnt.add(this.inp.City, {row:vertical_offset , column:1}); 
            cnt.add(new qx.ui.basic.Label().set({value: "Организация",rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.Organization, {row:vertical_offset , column:1}); 
            cnt.add(new qx.ui.basic.Label().set({value: "Должность", rich : true}), {row:++vertical_offset, column:0});
            cnt.add(this.inp.Position, {row:vertical_offset , column:1});
            cnt.add(new qx.ui.basic.Label().set({value: "Номер телефона"+ RFM,                rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Telephone_number,       {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Юридический пользователь",      textAlign: "right", rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Legal_type,       {row:vertical_offset , column:1});
            
            var legalLayout = new qx.ui.layout.Grid(16, 6); 
            legalLayout.setColumnWidth(1, 130) ;
            legalLayout.setColumnAlign(0, "right", "top");
            this.legalInfo = new qx.ui.container.Composite(legalLayout);
//				.set({width:448, height:336});
//      
//			cnt.add(new qx.ui.basic.Label().set({value: "",                rich : true}),
//			{row:++vertical_offset, column:0, colSpan:2});
			cnt.add(this.legalInfo,       {row:++vertical_offset , column:0, colSpan:2}); 

            this.buildFormLegal();
            this.submitButton.setLabel("Сохранить");
            this.cancelButton = null;
            this.addbuttonRow(cnt, ++vertical_offset); 
            this.controller.placeForm(cnt); 
        },
        ////
        //// Строим форму для отображения юриждической информации на основе полученных данных
        ////
        buildFormLegal:function(){ 
            var vertical_offset = 0;
            var l1 = new qx.ui.basic.Label("Юридическая информация");
            l1.setFont("bold");
            l1.setAlignX("left"); 
            this.legalInfo.add(l1, {row:0, column:0, colSpan:2});
        
            /*Поля для информации о юридическом пользвоателе*/ 
    
            this.inp.Address      = new qx.ui.form.TextField()
                .set({placeholder: "Адрес организации", readOnly:false});
    
            this.inp.Inn      = new qx.ui.form.TextField()
                .set({placeholder: "ИНН", readOnly:false});
    
            this.inp.Kpp      = new qx.ui.form.TextField()
                  .set({placeholder: "КПП", readOnly:false});  
    
            this.legalInfo.add(new qx.ui.basic.Label().set({value: "Адрес организации", rich: true}),
                      {row:++vertical_offset, column:0});
            this.legalInfo.add(this.inp.Address, {row:vertical_offset , column:1});
    
            this.legalInfo.add(new qx.ui.basic.Label().set({value: "ИНН", rich: true}),
                      {row:++vertical_offset, column:0});
            this.legalInfo.add(this.inp.Inn, {row:vertical_offset , column:1});
    
            this.legalInfo.add(new qx.ui.basic.Label().set({value: "КПП", rich: true}),
                      {row:++vertical_offset, column:0});
            this.legalInfo.add(this.inp.Kpp, {row:vertical_offset , column:1}); 

        }, 
        
        legalFormHide:function(){   
	        this.inp.Address.setEnabled(false);  
	        this.inp.Inn.setEnabled(false);  
	        this.inp.Kpp.setEnabled(false);
        },
        
        legalFormShow:function(){ 
	        this.inp.Address.setEnabled(true);  
	        this.inp.Inn.setEnabled(true);  
	        this.inp.Kpp.setEnabled(true);
        },
        
        
        /**
            Обработчики событий,
                которые не удалось вынести внутрь
                    отдельных виджетов.
        **/
        addListeners: function() {            
            var _this = this;
            
            /* События виджетов для сопровождающей картикни  */
            this.picButton.addListener('changeFileName',function(e){
                if('' != e.getData()) {
                    _this.inp.Pic_url.setValue(_this.picButton.getFileName());
                    
                    if(zqr.view.Form.AbstractForm.customFormChkImgFileName(_this.inp.Ref)){
                        zqr.view.Form.Upload.UploadFakeStatusBar.on();
                        _this.picForm.setParameter("prev", _this.inp.Pic_url.getValue());
                        _this.picForm.send();
                    }else{
                        return false;
                    }
                }
                return true;
            }); 

            /* События виджетов для юр лица  */
            this.inp.Legal_type.addListener('changeValue',function(e){  
            	if(e.getData() == true){  
                    _this.legalFormShow();
            	}else{  
                    _this.legalFormHide();
            	}   
                return true;
            }); 
            this.picForm.addListener('completed',function(e) {
                var response = _this.picForm.getIframeTextContent();
                zqr.view.Form.Upload.UploadFakeStatusBar.off();
                _this.inp.Pic_url.setValue(response);
            });   
        },
        
        /**
            Создает область загрузки картинки.
        **/
        _buildPicFormCnt: function() {
            var pic_layout = new qx.ui.layout.Grid(12, 6);
            var picFormCnt = new qx.ui.container.Composite(pic_layout).set({
                allowGrowX: true
              });
            if(!this.inp.Pic_url)
                return picFormCnt;
            
            pic_layout.setColumnFlex(0, 1);
            pic_layout.setColumnAlign(0, "right", "middle");
            picFormCnt.add(this.inp.Pic_url,  {row:0, column:0});
            this.picForm.setParameter('rm','upload');
            this.picForm.setLayout(new qx.ui.layout.Basic);
            picFormCnt.add(this.picForm, {row:0, column:1});
            this.picForm.add(this.picButton , {left:0,top:0});
            return picFormCnt;
        },
        
        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            var pass1 = this.inp.Password1.getValue();
            var pass2 = this.inp.Password2.getValue();
            var flag = true; 

            flag &= zqr.view.Form.AbstractForm.customFormPassCheck(this.inp.Password1, this.inp.Password2); 
 
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(1, 50, this.inp.Firstname);
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(1, 50, this.inp.Lastname);
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(1, 50, this.inp.Patronimic);

            flag &= zqr.view.Form.AbstractForm.checkTelephone(this.inp.Telephone_number);  

            if(this.inp.Legal_type.getValue()){
	            flag &= zqr.view.Form.AbstractForm.checkNumeric(this.inp.Inn); 
	            flag &= zqr.view.Form.AbstractForm.checkNumeric(this.inp.Kpp);
	            flag &= zqr.view.Form.AbstractForm.customFormChkLength(11, 11, this.inp.Inn); 
	            flag &= zqr.view.Form.AbstractForm.customFormChkLength(10, 10, this.inp.Kpp);
            }
            
            flag &= zqr.view.Form.AbstractForm.customFormChkImgFileName(this.inp.Pic_url);
            flag &= zqr.view.Form.AbstractForm.customFormChkEmail(this.inp.Email);
            
            return flag;
        },
		 

        /**
            Посылает данные на сервер.
        **/
       _onSubmitClick : function(e) {
           this.confirm("Сохранить изменения?", function() {
                this._uploadData(e);
                if(this.uReq){
                    this.submit(this.uReq);
                    this.controller.onCancelClick();
                }
            }, this);


//             if (confirm("Сохранить изменения?")) { 
//                 this._uploadData(e);
//                 if(this.uReq){
//                     this.submit(this.uReq);
//                     this.controller.onCancelClick();
//                 }
//             }
        }, 
        
        /**
            Формирует данные для сервера
        **/
        _uploadData : function(e) {
            this.base(arguments, e); 
            var password = this.inp.Password1.getValue(); 
            if(this.validateForm()) {
                this.uReq.setParameter("password", password, true); 
            }
        },
        
        /**
            Заполняет форму
        **/ 
        fillForm : function(data) {     
            for(var fieldName in this.inp){
                if(("Password1" == fieldName)
                    || ("Password2" == fieldName)
                    || ("Legal_type" == fieldName)
                 )continue; 
                 var item = fieldName.toLowerCase(); 
                 
                 if(("null" == data.value[item]) || (!data.value[item])){
                     this.inp[fieldName].setValue("")
                 }else{
                     this.inp[fieldName].setValue(data.value[item]);                	
                 }
                 console.log("item = ", item);
             }  
             console.log("data = ", data);  
             this.inp.Legal_type.setValue(RegExp("^true$").test(data.value['legal_type']));
  
             this.inp.Password1.setValue("");
             this.inp.Password2.setValue(""); 
             
             var _id = data.value["id"];
             this.picForm.setParameter("id", _id);
        }
    }
});

