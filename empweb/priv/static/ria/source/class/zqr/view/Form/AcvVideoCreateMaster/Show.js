/* ************************************************************************
    https://gist.github.com/1639960
    
#asset(qx/icon/Tango/16/actions/document-save.png)
#asset(qx/icon/Tango/16/apps/utilities-help.png)
************************************************************************ */

qx.Class.define("zqr.view.Form.AcvVideoCreateMaster.Show",
{
    extend : zqr.view.Form.AcvVideoCreateMaster.BasePage,
    
    construct : function(uReq, Row, Options) { 
        this.base(arguments, uReq, Row, Options); 
        this.isNew = false; 
        console.log(this.isNew); 
    },

    members : {
 
        helpUrl : "/docs/offer",
        
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
            url: "/get-acv-video/show",
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
            Wish:       null,
            Shown:       null,
            Preroll:    null,
            Midroll:    null,
            Postroll:   null,
            Pauseroll:  null,
            Rerun_hours: null,
            Rerun_minutes: null
        },
        
        flashBar    : null, /* поле флеш */
        flashPlayer : null, /* объект плеера */
        boxPlace: null,
        boxRe: null,
        
        buildForm : function(){  
            var RFM = zqr.view.Form.AbstractForm.REQUIRED_FIELD_MARKER;


            
            this.pageName = new qx.ui.basic.Label()
                .set({
                    value: "Показ видео",  font: "bold",
                    alignX: "left", rich : true
                });
                
            this.boxPlace = this.makeBoxPlace();
            this.boxRerun = this.makeBoxRerun();
            this.inp.Wish = new qx.ui.form.Spinner(1000, 1000, 1152921504606846976)
                .set({
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Желаемое вами количество показов вашей рекламы")
                });
            this.inp.Wish.setHeight(10);
            var nf = new qx.util.format.NumberFormat();
            nf.setMaximumFractionDigits(0);
            this.inp.Wish.setNumberFormat(nf);

            this.inp.Shown = new qx.ui.form.Spinner(0, 0, 1152921504606846976)
                .set({
                    enabled: false
                });

            return this.refresh();
        },
                
        refresh: function(){
            var vertical_offset = -1;
            var layout = new qx.ui.layout.Grid(3, 10);

            layout.setColumnFlex(1, 1, 1);
            layout.setColumnAlign(0, "right", "top");
            
            this.composite  = new qx.ui.container.Composite(layout).set({
                width:700
            }) ;

            this.composite.add(this.pageName, {row:++vertical_offset, column:0});
            this.composite.add(new qx.ui.basic.Label().set({value: "Количество показов ",  rich : true}),
                    {row:++vertical_offset, column:0});
            this.composite.add(this.inp.Wish,   {row:vertical_offset, column:1});

            if(this.isNew){
                this.composite.add(new qx.ui.basic.Label().set({value: "Фактическое количество",  rich : true}),
                        {row:++vertical_offset, column:0});
                this.composite.add(this.inp.Wish,   {row:vertical_offset, column:1});
            }

            this.composite.add(this.boxPlace,
                {row:++vertical_offset, column:0,colSpan:2});

            this.composite.add(this.boxRerun,
                {row:++vertical_offset, column:0,colSpan:2});

            this.flashBar = new qx.ui.container.Composite(new qx.ui.layout.HBox()).set({width:448, height:336});
            this.composite.add(this.flashBar, {row:0, column:2,rowSpan:10});

            this.flashPlayer = new qx.ui.embed.Flash("resource/zqr/flash/tvzavrplayer2.swf").set({
                width: 448,
                height: 336,
                variables : {
                    autoplay:0,
                    src:"/" + this.uReq.getParameter('ref', true)
                }
            });
            this.flashBar.add(this.flashPlayer);

            return this.composite;            
        },

        makeBoxPlace : function() { 
            this.inp.Preroll = new qx.ui.form.CheckBox("Preroll — показ ролика перед фильмом")
                .set({
                    value: true,
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Показ ролика перед началом трансляции фильма")
                });
            this.inp.Midroll = new qx.ui.form.CheckBox("Midroll — показ ролика во время фильма")
                .set({
                    value: true,
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Показ ролика во время трансляции фильма")
                });
            this.inp.Postroll = new qx.ui.form.CheckBox("Postroll — показ ролика в конце фильма")
                .set({
                    value: true,
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Показ ролика в конце трансляции фильма")
                });
            this.inp.Pauseroll = new qx.ui.form.CheckBox("Pauseroll — показ ролика на паузе")
                .set({
                    value: false,
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Показ ролика на паузе")
                });
            
            var boxPlace = new qx.ui.groupbox.GroupBox("Размещение ролика")
                .set({
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Размещение ролика внутри фильма")
                });
            boxPlace.setLayout(new qx.ui.layout.VBox(2));
            boxPlace.add(this.inp.Preroll);
            boxPlace.add(this.inp.Midroll);
            boxPlace.add(this.inp.Postroll);
            boxPlace.add(this.inp.Pauseroll);
            return boxPlace;
        },
        
        makeBoxRerun : function() { 
            this.inp.Rerun_hours = new qx.ui.form.Spinner(0, 0, 24)
                .set({
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Часы")
                });
            this.inp.Rerun_minutes = new qx.ui.form.Spinner(0, 10, 60)
                .set({
                    toolTip: new
                        qx.ui.tooltip.ToolTip("Минуты")
                });
            var vertical_offset = 0;
            var boxRerun  = new
                qx.ui.groupbox.CheckGroupBox("Задержка повторного показа определенному зрителю").set({
                    toolTip: new qx.ui.tooltip.ToolTip("Активируйте, если хотите установить временной интервал между повторами ролика одному и тому же пользователю")
                    //,blockToolTip: true
                });
            //var boxRerun = new qx.ui.groupbox.GroupBox("Повтор ролика");
            var layout = new qx.ui.layout.Grid(1, 5)
            layout.setColumnFlex(1, 1);
            layout.setColumnFlex(2, 1);
            layout.setColumnFlex(4, 1);
            boxRerun.setLayout(layout);
            boxRerun.setValue(false);
            boxRerun.add(new qx.ui.basic.Label().set({value: "Часы",  rich : true}), {row:vertical_offset, column:0});
            boxRerun.add(this.inp.Rerun_hours, {row:vertical_offset, column:1});
            boxRerun.add(new qx.ui.basic.Label().set({value: "Минуты",  rich : true}), {row:vertical_offset, column:3});
            boxRerun .add(this.inp.Rerun_minutes, {row:vertical_offset, column:4});
            return boxRerun ;
        },
        
        /**
            Создает область загрузки картинки.
        **/
        _buildPicFormCnt: function() { 
            var pic_layout = new qx.ui.layout.Grid(12, 6);
            var picFormCnt = new qx.ui.container.Composite(pic_layout).set({
                allowGrowX: true
              });
            if(!this.inp.Url)
                return picFormCnt;
            
            pic_layout.setColumnFlex(0, 1);
            pic_layout.setColumnAlign(0, "right", "middle");
            picFormCnt.add(this.inp.Url,  {row:0, column:0});
            this.picForm.setParameter('rm','upload');
            this.picForm.setLayout(new qx.ui.layout.Basic);
            picFormCnt.add(this.picForm, {row:0, column:1});
            this.picForm.add(this.picButton , {left:0,top:0});
            return picFormCnt;
        },
        
        _onLoadFormDataCompl : function(response) { 
            var result = response.getContent();
            if (false == zqr.util.errors.process(this, result))
                return false;
            this.fillForm(result);
            return true;
        },
        
        /**
            @overload
                Функция блокировки\разблокировки элементов ввода,
                которые не относятся
                к this.inp, и там их нельзя обработать.
        **/
        onChangeEnabled: function(enabled) { 
            this.boxRerun.setEnabled(enabled);  
        },
        
        /**
            Заполняет форму полученными данными.
        **/
        fillForm : function(data) { 
            this.inp.Wish.setValue(parseInt(data.value.wish));
             
            this.inp.Preroll.setValue(zqr.util.utils.parseBoolean(data.value.preroll));
            this.inp.Midroll.setValue(zqr.util.utils.parseBoolean(data.value.midroll));
            this.inp.Postroll.setValue(zqr.util.utils.parseBoolean(data.value.postroll));
            //this.inp.Pauseroll.setValue(zqr.util.utils.parseBoolean(data.value.pauseroll));
            
            this.boxRerun.setValue((("null" != data.value.rerun_hours) && ("null" != data.value.rerun_minutes)))
            this.inp.Rerun_hours.setValue(parseInt(data.value.rerun_hours));
            this.inp.Rerun_minutes.setValue(parseInt(data.value.rerun_minutes)); 
        },
        
        /**
            Проверяет коректность данных
        **/
        validateForm : function() {
            var flag = true;
            
            flag &= zqr.view.Form.AbstractForm.allowedChars(this.inp.Wish, "0-9");
            return flag;
        },
        
        /**
            Применив некоторые преобразования <<загружает>> данные на сервер
        **/
        saveData : function(e) {
            var formIsValid = this.validateForm();
            if(formIsValid){
                var res = {}
                
                if(this.boxRerun.getValue()){
                    res.rerun_hours     = this.inp.Rerun_hours.getValue();
                    res.rerun_minutes   = this.inp.Rerun_minutes.getValue();
                }else{
                    res.rerun_hours     = "null";
                    res.rerun_minutes   = "null";
                }
                
                for(var fieldName in this.inp){
                    if(("Rerun_hours" == fieldName) ||
                        ("Rerun_minutes" == fieldName))
                            continue;
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

