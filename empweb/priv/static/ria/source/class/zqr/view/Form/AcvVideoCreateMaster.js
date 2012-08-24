/* ************************************************************************
    Мастер создания видео рекламы.


#asset(qx/icon/Tango/32/actions/edit-undo.png)
#asset(qx/icon/Tango/32/actions/edit-redo.png)

#asset(qx/icon/Tango/32/actions/media-seek-backward.png)
#asset(qx/icon/Tango/32/actions/media-seek-forward.png)

#asset(qx/icon/Tango/32/actions/mail-mark-junk.png)

#asset(qx/icon/Tango/32/actions/dialog-apply.png)
#asset(qx/icon/Tango/32/actions/dialog-ok.png)

#asset(qx/icon/Tango/32/actions/process-stop.png)
#asset(qx/icon/Tango/32/status/dialog-information.png)

    
************************************************************************ */


qx.Class.define("zqr.view.Form.AcvVideoCreateMaster",
{

    extend : zqr.view.Form.AbstractForm,

    construct : function(controller, Row) {
        this.__Row = Row;
        if(this.__Row)
            this.createNew = (this.__Row.isNew == true);
        this.base(arguments, controller, this.__Row, "рекламной кампании");
        
        this.counterLabel = new qx.ui.basic.Label();
        // кнопки
        // ---------------------------------------------------------------
        this.cancelButton = new qx.ui.form.Button("Отмена", "icon/32/actions/mail-mark-junk.png");
        this.cancelButton.addListener("execute", this._onCancelClick, this);

        // Дублирование логики с верхней  кнопкой назад [<-].
        // this.__hidebutton(this.cancelButton);
        this.helpButton = new qx.ui.form.Button("Информация",  "icon/32/status/dialog-information.png");
        this.helpButton.addListener("execute", this._onHelpClick, this);
        
        this.prevButton =  new qx.ui.form.Button("Назад",  "icon/32/actions/edit-undo.png");
        this.prevButton.addListener("execute", this._onPrevClick, this);
        this.nextButton =  new qx.ui.form.Button("Далее",  "icon/32/actions/edit-redo.png");
        this.nextButton.addListener("execute", this._onNextClick, this);
        this.sendButton = new qx.ui.form.Button("Завершить", "icon/32/actions/dialog-ok.png");
        this.sendButton.addListener("execute", this._onSendClick, this);

        this.__hidebutton(this.sendButton);
        this.__hidebutton(this.prevButton);

        // ---------------------------------------------------------------
        
        // запрос
        // ---------------------------------------------------------------
        this.uReq = new qx.io.remote.Request
            (this.urc.url, this.urc.method, this.urc.mimetype);
        // ---------------------------------------------------------------
        
        // список окон
        // ---------------------------------------------------------------
        this.__list = []; 
        
        console.log("!");


        this.__list.push(new zqr.view.Form.AcvVideoCreateMaster.Common(this.uReq, this.__Row, true)); 
        console.log("Common");
        this.__list.push(new zqr.view.Form.AcvVideoCreateMaster.Upload(this.uReq, this.__Row)); 
        console.log("Upload");
        this.__list.push(new zqr.view.Form.AcvVideoCreateMaster.Show(this.uReq, this.__Row)); 
        console.log("Show");

        this.__list.push(new zqr.view.Form.AcvVideoCreateMaster.Events(this.uReq, this.__Row));
        console.log("Events");

        this.__list.push(new zqr.view.Form.AcvVideoCreateMaster.UsersTargeting(this.uReq, this.__Row)); 
        console.log("UsersTargeting");
        this.__list.push(new zqr.view.Form.AcvVideoCreateMaster.RegionTargeting(this.uReq, this.__Row)); 
        console.log("RegionTargeting");
        this.__list.push(new zqr.view.Form.AcvVideoCreateMaster.SubnetTargeting(this.uReq, this.__Row));
        console.log("SubnetTargeting")
        this.__list.push(new zqr.view.Form.AcvVideoCreateMaster.PlatfromTargeting(this.uReq, this.__Row));
        console.log("PlatfromTargeting");
        this.__list.push(new zqr.view.Form.AcvVideoCreateMaster.PageurlTargeting(this.uReq, this.__Row));
        console.log("PageurlTargeting");        
		this.__list.push(new zqr.view.Form.AcvVideoCreateMaster.CategoryTargeting(this.uReq, this.__Row)); 
        console.log("CategoryTargeting");
        
        this.__step = 0;
        this.__length = this.__list.length;
        // ---------------------------------------------------------------

        this.buildForm();
        this.addListeners();
        this.showCurrentPage();
        this.__updateHelpBtn();
    },
    
    members : {
        
        urc : {
            url:        "/update-acv-video",
            imgurl:     "/update-acv-video/upload-video",
            method:     "POST",
            mimetype:   "application/json"
        },
        
        __step:     null,     // шаг мастера
        __list :   null,      // формы мастера 
        __length :  null,     // длинна мастера
        placeholder : null,

        __showbutton: function(but){
            but.setVisibility("visible");
        },

        __hidebutton: function(but){
            but.setVisibility("excluded");
        },

        __disablebutton: function(but){
            but.setEnabled(false);
        },

        __enablebutton: function(but){
            but.setEnabled(true);
        },
        
        __gotoNext : function(){
            if(this.__step < this.__getLength() - 1){
                this.__step += 1;
                if(this.__step == this.__getLength() - 1){
                    this.__hidebutton(this.nextButton);
                    this.__showbutton(this.sendButton);
                }
            }
            else{
                this.__hidebutton(this.nextButton);
                this.__showbutton(this.sendButton);
            }
            //this.__enablebutton(this.prevButton);
            this.__showbutton(this.prevButton);
            this.__hidebutton(this.cancelButton);

            this.__updateHelpBtn();
        },
        
        __gotoPrev : function(){
            if(0 < this.__step){
                this.__step -= 1;
                if(0 == this.__step) {
                    this.__showbutton(this.cancelButton);
                    this.__hidebutton(this.prevButton);
                }
            }
            else{
                this.__showbutton(this.cancelButton);
                this.__hidebutton(this.prevButton);
            }
            this.__hidebutton(this.sendButton);
            this.__showbutton(this.nextButton);

            this.__updateHelpBtn();
        },

        __updateHelpBtn : function() {
            if(!this.__getCur().helpUrl)
                this.__disablebutton(this.helpButton);
            else
                this.__enablebutton(this.helpButton);
        },
        
        __getCur : function(){ 
        	if(this.__list[this.__step].classname == 'zqr.view.Form.AcvVideoCreateMaster.Show'){
        		this.__list[this.__step].refresh();// = new zqr.view.Form.AcvVideoCreateMaster.Show(this.uReq, this.__Row); 
        	}
            return this.__list[this.__step];
        },

        __delCur : function(){
            this.placeholder.removeAll();
        },
        
        validateCurrent : function(){
            var formIsValid = false;
            var cur = this.__getCur();
            if(cur["saveData"]){
                formIsValid = cur.saveData();
            }
            return formIsValid;
        },
        
        showCurrentPage : function(){
            var cur = this.__getCur();
            this.placeholder.add(cur.getComposite());
            this.counterLabel.setValue("Шаг " + (1 + this.__step) +
                " из " + this.__getLength() + ".");
        },
        
        __getLength : function(){
            return this.__length;
        },
        
        d: function(){console.log(arguments);},
        
        buildForm : function(){
            var layout = new qx.ui.layout.Grid(1, 1);
            var cnt = new qx.ui.container.Composite(layout);
            layout.setColumnFlex(0, 1);
            layout.setColumnAlign(0, "left", "top");
            var work_layout = new qx.ui.layout.VBox(1);
            this.placeholder = new qx.ui.container.Composite(work_layout);
            // -------------------------------------------------------------
            var vertical_offset = -1;
            cnt.add(this.counterLabel,  {row:++vertical_offset, column:0});
            cnt.add(this.placeholder,   {row:++vertical_offset, column:0});
            this.addbuttonRow(cnt,      ++vertical_offset);
            // -------------------------------------------------------------
            this.controller.placeForm(cnt);
            this.cnt = cnt;
            return {controller : cnt, offset: vertical_offset};
        },
        
        addbuttonRow: function(cnt, vertical_offset) {
            var br = new qx.ui.container.Composite(new qx.ui.layout.Dock);
            br.setMarginTop(5);

            var buttonRow = new qx.ui.container.Composite();
            br.add(buttonRow, {edge:"east"});

            br.add(this.helpButton, {edge:"west"});
//            buttonRow.setMarginTop(5);
            var hbox = new qx.ui.layout.HBox(5);
            hbox.setAlignX("right");
            buttonRow.setLayout(hbox);
            /**
                Такой странный порядок следования обусловлен,
                что некоторые кнопки пропадают.
            **/
            buttonRow.add(this.prevButton);
            buttonRow.add(this.nextButton);
            buttonRow.add(this.cancelButton);
            buttonRow.add(this.sendButton);
            //cnt.add(buttonRow, {row:vertical_offset , column:0, colSpan:3});
            cnt.add(br, {row:vertical_offset , column:0, colSpan:3});
        },
        
        addListeners: function() {
            var _this = this;
        },
        
        _onNextClick: function() {
            if(this.validateCurrent()){
                this.__delCur();
                this.__gotoNext();
                this.showCurrentPage();
            }
         },
         
        _onPrevClick: function() {
//            if(this.validateCurrent()){
                this.__delCur();
                this.__gotoPrev();
                this.showCurrentPage();
//            }
         },
         
        _onCancelClick : function(e) {
            this.controller.onCancelClick();
        },

        _onHelpClick : function(e) {
            var C = this.__getCur();
            if(C.helpUrl)
                window.open(C.helpUrl, "_blank", "",false);
        },
        
        _onSendClick: function() {
            if(this.validateCurrent()){
                if(this.uReq){
                    this.submit(this.uReq);
                }
            }
        },
        
        /**
         * Проверяет коректность ВСЕХ данных.
        **/
        validateForm : function() {
            var flag = true;
            for(var i = 0; i!=  this.__list.length ; ++i){
                flag = flag && this.__list[i].validateForm();
            }
            return flag;
        },

        onFormClose : function() {
            var win = zqr.util.utils.infoWindow("Заявка принята.<br/>Инструкции по оплате будут высланы на ваш email.");

            this.browser.add(win, {
                left : win.l*1, 
                top  : win.t*1
            });
        }
    }
});

