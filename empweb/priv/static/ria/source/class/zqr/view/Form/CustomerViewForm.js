/* ************************************************************************

    Класс описания формы по созданию пользователя
    ИСПОЛЬЗОВАНИЕ:
        Администрировнаие > Пользователи >  [Создать] | [Редактировать]

************************************************************************ */


qx.Class.define("zqr.view.Form.CustomerViewForm",
{
    extend : zqr.view.Form.BaseForm,

    construct : function(controller, Row) {
        this.base(arguments, controller, Row); 
        this.base(arguments, controller, Row);
        this.addListeners();
    },

    members : {

        urc : {  // upload request config
            url: "/update-customer",
            method: "POST",
            imgurl: "/update-customer/upload-image",
            mimetype: "application/json"
        },

        drc : {  // download request config
            url: "/get-customer-info",
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
            Telephone_number: null
        },
        
        /* Cписок групп справа */
        groupList: null,
        groupListOptions: {
            url:            "/get-customer-groups",
            labelFieldName: "name",
            descrFieldName: "description"
        },

        /**
            Строит визуальное представление формы
            TODO: отрефакторить, так чтобы было мало букаф
        **/
        
        /* Виджеты для сопровождающей картикни */
        picForm: null,
        picButton: null,
        

        buildForm : function() {
            this.base(arguments);

            this.inp.Id           = new qx.ui.form.TextField();
            this.inp.Login        = new qx.ui.form.TextField()
                .set({placeholder: "login", required:true});
            this.inp.Password1    = new qx.ui.form.PasswordField()
                .set({placeholder: "pass", required:true});
            this.inp.Password2    = new qx.ui.form.PasswordField()
                .set({placeholder: "pass", required:true});
            this.inp.Email        = new qx.ui.form.TextField()
                .set({placeholder: "email@example.com", required:true});
            this.inp.City         = new qx.ui.form.TextField()
                .set({placeholder: "Город"});
            this.inp.Organization = new qx.ui.form.TextField()
                .set({placeholder: "Организация"});
            this.inp.Position     = new qx.ui.form.TextField()
                .set({placeholder: "Должность"});
            this.inp.Firstname    = new qx.ui.form.TextField()
                .set({placeholder: "Ваше имя"});
            this.inp.Lastname     = new qx.ui.form.TextField()
                .set({placeholder: "Фамилия"});
            this.inp.Patronimic   = new qx.ui.form.TextField()
                .set({placeholder: "Отчество"});
                
            this.inp.Pic_url      = new qx.ui.form.TextField()
                .set({placeholder: "Выберите файл", readOnly:true});
                
            this.inp.Telephone_number      = new qx.ui.form.TextField()
                .set({placeholder: "Номер телефона", readOnly:false});
            
            this.groupList = new zqr.view.SelListTree(this,
                this.groupListOptions.url,
                this.groupListOptions.labelFieldName,
                this.groupListOptions.descrFieldName
            );
        
            /* Сопровождающая картинка */
            this.picButton = new zqr.view.Form.Upload.UploadButton("uploadfile", null, "icon/16/actions/document-save.png"),
            this.picForm = new zqr.view.Form.Upload.UploadForm('uploadFrm', this.urc.imgurl);

            var layout = new qx.ui.layout.Grid(13, 6);
            var cnt = new qx.ui.container.Composite(layout);

            this.inp.Id.setEnabled(false);
            this.groupList.setWidth(300);

            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "right", "top");

            var l1 = new qx.ui.basic.Label("Общая информация");
            l1.setFont("bold");
            l1.setAlignX("left");
            cnt.add(l1, {row:0, column:0, colSpan:2});

            var vertical_offset = 0;
            var RFM = zqr.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;

            cnt.add(new qx.ui.basic.Label("#"),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Id,         {row:vertical_offset , column:1});
            cnt.add(new qx.ui.basic.Label().set({value: "Логин"             + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Login,      {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Пароль"            + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Password1,  {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Повторите пароль"  + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Password2,  {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "E-mail"            + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Email,          {row:vertical_offset , column:1});

            cnt.add(new qx.ui.basic.Label().set({value: "Имя"               + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Firstname,  {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Отчество"          + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Patronimic, {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Фамилия"           + RFM,  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Lastname,   {row:vertical_offset , column:1});

            cnt.add(new qx.ui.basic.Label().set({value: "Фотография:",  rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this._buildPicFormCnt(), {row:vertical_offset , column:1});

            
            cnt.add(new qx.ui.basic.Label().set({value: "Город",                    rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.City,           {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Организация",              rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Organization,   {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Должность",                rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Position,       {row:vertical_offset , column:1});
            
            cnt.add(new qx.ui.basic.Label().set({value: "Номер телефона"+ RFM,                rich : true}),
                    {row:++vertical_offset, column:0});
            cnt.add(this.inp.Telephone_number,       {row:vertical_offset , column:1});

            this.addbuttonRow(cnt, ++vertical_offset);

            this.controller.placeForm(cnt);
            this.inp.Login.focus();
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
            var id =  this.inp.Id.getValue();
            if(id == null) {
                flag &= zqr.view.Form.AbstractForm.customFormCheckRequired(this.inp.Password1);
                flag &= zqr.view.Form.AbstractForm.customFormCheckRequired(this.inp.Password2);
                flag &= zqr.view.Form.AbstractForm.customFormPassCheck(this.inp.Password1, this.inp.Password2);
            }
            else {
                flag &= zqr.view.Form.AbstractForm.customFormPassCheck(this.inp.Password1, this.inp.Password2);
            }

            flag &= zqr.view.Form.AbstractForm.customFormCheckRequired(this.inp.Email);
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(1, 1024, this.inp.Login);
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(1, 1024, this.inp.Firstname);
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(1, 1024, this.inp.Lastname);
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(1, 1024, this.inp.Patronimic);
            ////////////////////////////
            //проверка номера телефона//
            ////////////////////////////
            flag &= zqr.view.Form.AbstractForm.customFormChkLength(1, 1024, this.inp.Telephone_number);
			////////////////////////////
			////////////////////////////
/*
            for(var fieldName in this.inp){
                if(  ("Password1" == fieldName)
                    || ("Password2" == fieldName)
                    || ("Email" == fieldName)
                    || ("Pic_url" == fieldName)
                ){continue;}
                console.log("fieldName = ", fieldName);
                console.log("this.inp[fieldName] = ", this.inp[fieldName]);
                
                flag &= zqr.view.Form.AbstractForm.customFormChkSymb(this.inp[fieldName]);
            }
*/            
            flag &= zqr.view.Form.AbstractForm.customFormChkImgFileName(this.inp.Pic_url);
            flag &= zqr.view.Form.AbstractForm.customFormChkEmail(this.inp.Email);
            
            return flag;
        },

        /**
            Формирует данные для сервера
        **/
        _uploadData : function(e) {
            this.base(arguments, e);
            var groupIdList = this.groupList.getSelectedId();
            var password = this.inp.Password1.getValue();
            if(this.validateForm()) {
                this.uReq.setParameter("password", password, true);
                this.uReq.setParameter("group_list", groupIdList, true);
            }
        },

        /**
            Заполняет форму
        **/

        fillForm : function(data) {
            for(var fieldName in this.inp){
               if(("Password1" == fieldName) || ("Password2" == fieldName))
                    continue;
                var item = fieldName.toLowerCase();
                this.inp[fieldName].setValue(data.value[item])
                if("null" == data.value[item]){
                    this.inp[fieldName].setValue("")
                }
                console.log("item = ", item);
            }

            this.inp.Password1.setValue("");
            this.inp.Password2.setValue("");
            this.groupList.setChecked(data.groups);
            
            var _id = data.value["id"];
            this.picForm.setParameter("id", _id);
        }
    }
});

