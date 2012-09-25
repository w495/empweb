
qx.Class.define("zqr.view.Form.GenericForm",
{
    extend : zqr.view.Form.AbstractForm,

    construct : function(controller, Row, formDescription, defaultValue) {
        this.base(arguments, controller);
        this.defaultValue = defaultValue;
        this.formDescription = formDescription;
        this.formRow = Row;
        this.form = new qx.ui.form.Form();
        this.reqMap = {};
        this.buildForm(formDescription);
    },

    statics : {

    },

    members : {

        formModel           : undefined,
        defaultValue        : undefined,
        formDescription     : undefined,
        formRow             : undefined,
        form                : undefined,
        reqMap              : undefined,

        buildForm : function(formDescription) {
            this.formFieldDescr = {};
            if(formDescription.fields == undefined)
                return;
            for(var i=0; i<formDescription.fields.length; i++) {
                var E = formDescription.fields[i];
                this.formFieldDescr[E.name] = E;

                var FField;
                switch(E.input.type) {
                    case "title":
                        this.form.addGroupHeader(E.input.value);
                        continue;
                    case "hidden":
                        FField = new qx.ui.form.TextField();
                        FField.setEnabled(false);
                        //FField.setVisibility("hidden");
                        break;
                    case "text-field":
                        FField = new qx.ui.form.TextField();
                        if(E.input.regexp != undefined)
                            FField.setFilter(E.input.regexp);
                        break;
                    case "text-area":
                        FField = new qx.ui.form.TextArea();
                        if(E.input.height != undefined)
                            FField.setHeight(E.input.height);
                        break;
                    case "check-box":
                        FField = new qx.ui.form.CheckBox();
                        break;

                    case "date-field-pd":
                        FField = new qx.ui.form.DateField();
                        var date = new Date();
                        date.setDate(date.getDate() + 1);
                        FField.setValue(date);
                        break;

                    case "date-field-md":
                        FField = new qx.ui.form.DateField();
                        var date = new Date();
                        date.setDate(date.getDate() - 1);
                        FField.setValue(date);
                        break;
                    case "date-field":
                        FField = new qx.ui.form.DateField();
                        FField.setValue(new Date());
                        break;
                    case "password":
                        FField = new qx.ui.form.PasswordField();
                        break;
                    case "text-area":
                        FField = new qx.ui.form.TextArea();
                        break;
                    case "spinner":
                        FField = new qx.ui.form.Spinner();
                        if(E.input.min != undefined)
                            FField.set({minimum: E.input.min});
                        if(E.input.max != undefined)
                            FField.set({maximum: E.input.max});
                        break;
                    case "select-box":
                        FField = new qx.ui.form.SelectBox();
                        FField.itemMap = {};
                        if(E.input.values != undefined) {
                            var idName = E.input.select_id
                            var aliasName = E.input.select_display_name;
                            for(var j=0; j<E.input.values.length; j++) { //sKey in E.input.values) {
                                var SI = E.input.values[j];
                                var selItem = new qx.ui.form.ListItem(SI[aliasName], null, SI[idName]);
                                FField.itemMap[SI[idName]] = selItem;
                                FField.add(selItem);
                            }
                        }

                        if(E.input.values_url != undefined){
                            var req = new qx.io.remote.Request(E.input.values_url, "GET", "application/json");
                            //req.setParameter("name", E.name);
                            this.reqMap[req] = E.name;
                            req.addListener("completed", this._onFetchSelectFields, this);
                            req.send();
                        }
                        break;
                    default:
                        alert(E.input.type + " - неизвестный тип поля формы");
                        continue;
                }

                FField.zqr_special = E;
                if(E.input.value!=undefined){
                    FField.setValue(E.input.value);
                }
                var FValidation = null;
                if(E.validation != undefined) {
                    FField.setRequired(E.validation.required == true);
                    switch(E.validation.type) {
                        case "int":
                            FValidation = zqr.view.Form.AbstractForm.num();
                            break;
                        case "range":
                            FValidation = qx.util.Validate.range(E.validation.min, E.validation.max);
                            break;
                        case "string-range":
                            FValidation = zqr.view.Form.AbstractForm.checkStringLength(E.validation.min, E.validation.max, FField);
                            //FValidation = qx.util.Validate.range(E.validation.type.min, E.validation.type.max);
                            break;
                        case "string-max":
                            FValidation = zqr.view.Form.AbstractForm.checkStringMax(E.validation.max, FField);
                            break;
                        case "equal":
                            FValidation = zqr.view.Form.AbstractForm.password(E.validation.field, this.form, FField);
                            break;
                    }
                }

                if(E.width != undefined)
                    FField.setWidth(E.width);
                this.form.add(FField, E.alias, FValidation, E.name);
            }

            var submitButton = new qx.ui.form.Button(formDescription.submit_btn_label || "Отправить");
            this.form.addButton(submitButton);

            if(formDescription.withCancel == undefined || formDescription.withCancel == true) {
                var cancelButton = new qx.ui.form.Button(formDescription.cancel_btn_label || "Отмена");
                this.form.addButton(cancelButton);
                cancelButton.addListener("execute", this._onCancelClick, this);
                console.log("formDescription = ", formDescription);

                if(formDescription.disableCancel == true){
                    cancelButton.setEnabled(false);
                }
            }

            var auxButtons = Array();
            if (formDescription.buttons){
                for(var k=0;k<formDescription.buttons.length;k++){
                    var btn = formDescription.buttons[k];
                    var auxButton = new qx.ui.form.Button(btn.alias || "Кнопка");
                    auxButton.url = btn.url;
                    this.form.addButton(auxButton);
                    auxButton.addListener("execute", this._onAuxButtonClick, this);
                }
            }

            if(formDescription.render == "single")
                this.controller.placeForm(new qx.ui.form.renderer.Single(this.form));
            else if(formDescription.render == "horisontal")
                this.controller.placeForm(new zqr.view.HorisontalRenderer(this.form));
            else if(formDescription.render == "complex")
                this.controller.placeForm(new zqr.view.MegaRenderer(this.form));
            else
                this.controller.placeForm(new qx.ui.form.renderer.Double(this.form));

            this.fController = new qx.data.controller.Form(null, this.form);
            this.formModel = this.fController.createModel();

            submitButton.addListener("execute", this._onSubmitClick, this);
            if(this.formRow != undefined
                    && this.formRow.isNew != true
                    && formDescription.index_name != undefined
                    && formDescription.formdata_url != undefined
                    && this.formRow[formDescription.index_name] != undefined)
                this.loadFormData(formDescription.formdata_url,
                    this.formRow[formDescription.index_name],
                    formDescription.index_name);
            else if(this.defaultValue != undefined)
                this.fillForm(this.defaultValue);
        },

        _onFetchSelectFields : function(response) {
            var result = response.getContent();
            if (zqr.util.errors.process(this, result)==false) return false;

            if(this.reqMap[response._target] == undefined)
                return false;
            var ffieldName = this.reqMap[response._target];
            this.reqMap[response._target] = undefined;
            var FField = this.form.getItems()[ffieldName];
            var fieldDescr = this.formFieldDescr[ffieldName];
            var idName = fieldDescr.input.select_id;
            var aliasName = fieldDescr.input.select_display_name;
            if(FField == undefined)
                return false;
            for(var j=0; j<result.values.length; j++) {
                var SI = result.values[j];
                var selItem = new qx.ui.form.ListItem(SI[aliasName], null, SI[idName]);
                FField.itemMap[SI[idName]] = selItem;
                FField.add(selItem);
            }
            return true;
        },

        getValues : function() {
            return qx.util.Serializer.toNativeObject(this.formModel);
        },

        _onSubmitClick : function(e) {
            this.controller.disableForm();
            if (this.form.validate()) {
                var req = new qx.io.remote.Request(this.formDescription.submit_url, "GET", "application/json");
                req.setTimeout(60000);
                var methodFlag = false;
                if(this.formDescription.req_type == "POST") {
                    req.setMethod("POST");
                    methodFlag = true;
                }

                var params = null;
                if(this.formModel)
                    params = qx.util.Serializer.toNativeObject(this.formModel);
                else if(this.formFieldDescr)
                    params = qx.util.Serializer.toNativeObject(this.formFieldDescr);
                else if(this.formDescription)
                    params = qx.util.Serializer.toNativeObject(this.formDescription);
                else{
                    this.controller.enableForm();
                    this._showValidationResult();
                    return false;
                }

                console.log("this.formModel = ", this.formModel)
                console.log("this.formFieldDescr = ", this.formFieldDescr)


                params = this.controller.getExtraParams(params); // доп параметры запроса - пейджер и сортировка
                var fFields = this.form.getItems();
                // доп параметры запроса - пейджер и сортировка
                for(var k in params) {
                    var fField = fFields[k];
                    var val = params[k];
                    if(fField instanceof qx.ui.form.DateField)
                        val = val.getTime();
                    req.setParameter(k, val, methodFlag);
                }
            }
            this.submit(req);
            return true;
        },

        _showValidationResult  : function() {
            var fFields = this.form.getItems();
            for(var key in fFields) {
                if(fFields[key].getValid() == false) {
                    switch(fFields[key].getInvalidMessage()) {
                        case "This field is required" :
                            fFields[key].setInvalidMessage("Это поле должно быть заполнено");
                            break;
                    }
                }
            }
        },

        _onCancelClick : function(e) {
            this.controller.onCancelClick();
        },

        _onAuxButtonClick : function(e) {
                var btnCaller = e.getCurrentTarget();
                var param_string = '';
                if(this.formFieldDescr != undefined && this.formFieldDescr != {}) {
                    var params = qx.util.Serializer.toNativeObject(this.formModel);
                    params = this.controller.getExtraParams(params); // доп параметры запроса - пейджер и сортировка
                    for(var k in params) {
                        param_string=param_string+"&"+k+"="+params[k];
                    }
                }
                window.open('/csv/stats/?'+param_string);
                //var req = new qx.io.remote.Request("/csv/stats", "GET", "text/plain");
                //req.send();
        },

        loadFormData : function(url, id, paramName) {
            this.controller.disableForm();
            var req = new qx.io.remote.Request(url, "GET", "application/json");
            req.setParameter(paramName, id);
            req.addListener("completed", this._onLoadFormDataCompl, this);
            req.send();
        },

        _onLoadFormDataCompl : function(response) {
            var result = response.getContent();
            if (zqr.util.errors.process(this, result)==false) return;
            this.fillForm(result);
        },

        fillForm : function(data) {
            var fFields = this.form.getItems();
            if(data.value)
                data = data.value
            else if(data.values)
                data = data.values
            for(var key in data) {
                if(fFields[key] != undefined) {
                    var fField = fFields[key];
                    if(fField instanceof qx.ui.form.SelectBox) {
                        var selField = fField.itemMap[data[key]];
                        if(selField != undefined) {
                            fField.setSelection([selField]);
                        }
                        else {
                            var E = fField.zqr_special.input.extra_value
                            if(E != undefined && data[E.id] != undefined && data[E.id] != "" && data[E.alias] != undefined) { // TODO переделать на проверку is_integer
                                var selItem = new qx.ui.form.ListItem(data[E.alias], null, data[E.id]);
                                fField.itemMap[data[E.id]] = selItem;
                                fField.add(selItem);
                                fField.setSelection([selItem]);
                            }
                        }
                    }
                    else {
                        var v = data[key];
                        var E = fField.zqr_special;
                        if(fField instanceof qx.ui.form.TextField) {
                            if(E.input.datatype == "float")
                                v = "" + v * 1;
                            else
                                v = "" + v;
                        }
                        fFields[key].setValue(v);
                    }
                }
            }
            this.controller.enableForm();
        },

        show_error : function(etype, emsg) {
            alert("Ошибка (" + etype + "): " + emsg);
        }
    }
});

