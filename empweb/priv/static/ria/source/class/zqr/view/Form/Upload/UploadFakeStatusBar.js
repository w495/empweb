/* ************************************************************************

   qooxdoo - the new era of web development

   http://qooxdoo.org

   Copyright:
     2007 Visionet GmbH, http://www.visionet.de

   License:
     LGPL: http://www.gnu.org/licenses/lgpl.html
     EPL: http://www.eclipse.org/org/documents/epl-v10.php
     See the LICENSE file in the project's top-level directory for details.

   Authors:
     * Dietrich Streifert (level420)
   
   Contributors:
     * Petr Kobalicek (e666e)
     * Tobi Oetiker (oetiker)

************************************************************************ */

/**
 * An upload widget implementation capable of holding multiple upload buttons
 * and upload fields.
 * 
 * Each upload form creates an iframe which is used as a target for form submit.
 * 
 *
 */
qx.Class.define("zqr.view.Form.Upload.UploadFakeStatusBar",
{
    type : "static",

    statics : {
        
        on: function(){
            console.log("zqr.view.Form.Upload.UploadFakeStatusBar.on");
            var resRootItem = document.getElementById("upload_progress_bar");
            resRootItem.style.display = "block"
            return resRootItem;
        },
        
        off: function(){
            console.log("zqr.view.Form.Upload.UploadFakeStatusBar.off");
            var resRootItem = document.getElementById("upload_progress_bar");
            resRootItem.style.display = "none"
            return resRootItem;
        }
    }
});
