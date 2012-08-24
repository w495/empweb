

/**
    Моделе-зависимые статические функции
**/

qx.Class.define("zqr.Config",
{
    type : "static",

    statics : {

        WINDOW_WIDTH       : window.innerWidth,
        WINDOW_HEIGHT      : window.innerHeight,

        WINDOW_WIDTH_HALF       : Math.floor(window.innerWidth/2),
        WINDOW_HEIGHT_HALF      : Math.floor(window.innerHeight/2),
        
        DOC_FORM_HEIGHT     : 250,
        DOC_FORM_WIDTH      : Math.floor(window.innerWidth * 0.7) ,

        TEST_FORM_HEIGHT     : 50,
        TEST_FORM_WIDTH      : Math.floor(window.innerWidth * 0.7) ,
        
        MASTER_FORM_WIDTH       : 500, //Math.floor(window.innerWidth * 0.7) ,
        MASTER_FORM_WIDTH_M     : 300, //Math.floor(window.innerWidth * 0.7) ,
        
        SELLISTTREE_HEIGHT            : 350 ,
        
        SORTEDSELLISTTREE_TIMEOUT    : 200 ,

        
        DOC_NAME_MAX_LEN    : 500,
        DOC_CONT_MAX_LEN    : 2147483647

    }
});
