

qx.Class.define("zqr.util.pipeline",
{
    extend: qx.core.Object,
    /**
    * Constructor: Pipeline
    * Creates a new pipeline
    *
    * Parameters:
    * name {string} the name of te pipeline
    * size {integer) the diameter of the pipe in inches
    *
    */
    construct : function(obj, map, proj, layer, biz) {
        this.biz = biz;
        this.proj = proj;
        this.map = map;
        this.obj = obj;
        this.layerA = layer;
        this.shapePoints = [];
        this.run = false;
        this.curPosition = null;
        var coord = qx.util.Json.parse(obj.last_coordinate);
        if(coord)
            this.mkCurPosition(coord.coordinates[0]*1, coord.coordinates[1]*1);
    },
    statics : {
    },

    members : {
        updateObj : function(nObj) {
            this.obj = nObj;
            this.drawBus(this.run);
        },

        getIconDir : function(hasEvents) {
            var ret = "yellow";
            if(this.obj.alarm != "")
                ret = "red";

            if(this.obj.has_connect != "true")
                ret += "-1";
            else if(hasEvents == false && this.biz.animation != false)
                ret += "-2";
            return ret;
        },

        mkCurPosition : function(x, y)  {
            var pnt = new OpenLayers.Geometry.Point(x, y)
            pnt.transform(this.proj, this.map.getProjectionObject());
            this.curPosition = new OpenLayers.Feature.Vector(pnt);
            var icon = "/resource/zqr/img/bus/yellow/060.png";
            this.curPosition.attributes = {
                gosNumber: this.obj.gos_number, 
                icon : icon,
                obj_id : this.obj.id
            };
            this.layerA.addFeatures([this.curPosition]);
            this.drawBus(false);
        },
        drop : function() {
            if(this.curPosition)
                this.layerA.removeFeatures([this.curPosition]);
        },
        addShapePoint : function(x, y) {
            this.shapePoints.push( new MyPoint(x, y));
        },

        /**
         * Method: mapFeature
         * Create a vector feature from Pipeline geometry
         *
         * Parameters:
         * style (Object} an optional style object for the pipeline when mapped
         *
         * Returns:
         * A map feature ready to be display
         */
        mapFeature : function( style ) {
            if (!style)
                style = {strokeColor: "black", strokeWidth: 1};

            var ls = new OpenLayers.Geometry.LineString( this.shapePoints );
            var lt = ls.clone();
            lt.transform(this.proj, this.map.getProjectionObject());
            return new OpenLayers.Feature.Vector( lt, null, style);
        },

        /**
         * Method: segmentGeoLength
         *
         * Parameters:
         * segment {iinteger} the segment offset desired
         *
         * Returns:
         * {float} the great circle length of the segment
         *
         */
        segmentGeoLength : function( segment ) {
            var pt = new Array(2);

            if (segment > this.shapePoints.length-1)
                return 0;

            pt[0] = this.shapePoints[ segment ];
            pt[1] = this.shapePoints[ segment+1 ];

            return pt[0].geoDistanceTo (pt[1]);
        },

        /**
         * Method:  geoLength
         *
         * Returns:
         * {float} the great circle length of the pipeline
         *
         */
        geoLength : function() {
            var distance = 0;

            for (var i=0; i< this.shapePoints.length-1; i++) {
                distance += this.segmentGeoLength (i);
            }
            return distance;
        },

        /**
         * Method: segmentPoints
         * Create a set of evenly-spaced points along the polyline
         *
         * Parameters:
         * segLength {float} distance between points
         *
         */
        segmentPoints : function( segLength ) {
            var p = []; // Points
            var lsp = [];
            var bearings = [];

            var i = 0;
            var currentLength = 0;
            p[0] = this.shapePoints[0];
            var P = p[0].clone();
            P.transform(this.proj, this.map.getProjectionObject());
            lsp.push(P);
            var bearing;
            while( i < this.shapePoints.length-1 ) {
                p[1] = this.shapePoints[i+1];
                var legDistance = p[0].geoDistanceTo(p[1]);
                bearing = p[0].geoBearingTo(p[1]);

                                        
                if( (segLength-currentLength) <= legDistance ) {
                    p[3] = p[0].geoWaypoint( segLength-currentLength, bearing );
                    P = p[3].clone();
                    P.transform(this.proj, this.map.getProjectionObject());
                    //lsp.push( p[3].clone() );
                    lsp.push(P);
                    bearings.push(bearing);
                    p[0] = p[3];
                    currentLength = 0;
                }
                else {
                    currentLength += legDistance;
                    p[0]=this.shapePoints[++i];
                }
            }
            P = p[0].clone();
            P.transform(this.proj, this.map.getProjectionObject());
            //lsp.push(p[0]);
            lsp.push(P);
            bearings.push(bearing);
            return {lsp: lsp, bearings:bearings};
        },

        /**
         * Method: animate1
         * Animate the polyline with "marching ants"
         *
         * Parameters:
         * runnerLength {float} the distance between points
         * tick_rate {integer} milliseconds between firing animation function
         * duration {integer} milliseconds total duration
         *
         */
        startAnimation : function() {
            if(this.shapePoints.length <= 1)
                return;
            if(this.biz.animation == false) {
                var P = this.shapePoints[this.shapePoints.length-1].clone();
                P.transform(this.proj, this.map.getProjectionObject());
                this.points = [P];
                this.stopAnimation();
            }
            else {
                this.index = 1;
                var Length = this.geoLength();
                runnerLength = Length / 90;
                if(!runnerLength) return;
                //this.points 
                var r = this.segmentPoints( runnerLength );
                this.points = r.lsp;
                this.bearings = r.bearings;
                this.run=true;
            }
        },

        stopAnimation : function() {
            if(this.curPosition && this.points) {
                this.curPosition.geometry.x = this.points[this.points.length-1].x;
                this.curPosition.geometry.y = this.points[this.points.length-1].y;
                this.layerA.drawFeature(this.curPosition);
            }
            this.shapePoints = [];
            this.run = false;
        },

        drawBus : function(animFlag) {
            var dir = 60;
            if(!this.curPosition)
                return;
            if(this.curPosition.dir)
                dir = this.curPosition.dir;
            if(("" + dir).length  == 1)
                dir = "00" + dir;
            else if(("" + dir).length  == 2)
                dir = "0" + dir;

            var x = this.curPosition.geometry.x;
            var y = this.curPosition.geometry.y;

            var directory = this.getIconDir(animFlag);
            var icon = "/resource/zqr/img/bus/" + directory + "/" + dir + ".png";
            this.layerA.removeFeatures([this.curPosition]);
            this.curPosition.destroy();

            var pnt = new OpenLayers.Geometry.Point(x, y)
            this.curPosition = new OpenLayers.Feature.Vector(pnt);
            this.curPosition.attributes = {
                gosNumber: this.obj.gos_number, 
                icon : icon,
                obj_id : this.obj.id
            };
            this.layerA.addFeatures([this.curPosition]);
            //this.layerA.drawFeature(this.curPosition);

            //this.layerA.addFeatures([this.curPosition]); 
            //this.layerA.drawFeature(this.curPosition);
            //this.layerA.redraw();
        },

        _onATimer : function() {
            if(!this.run)
                return false;

            if(!this.curPosition) {
                this.mkCurPosition(this.points[ this.index].x, this.points[ this.index].y);
            }
            else {
                var bearing = 360 - this.bearings[this.index-1];
                var dir = Math.round(bearing/3) * 2;

                this.curPosition.geometry.x = this.points[ this.index].x;
                this.curPosition.geometry.y = this.points[ this.index].y;
                this.curPosition.dir = dir;
                this.drawBus(this.index + 1 < this.points.length);
            }
            this.index++;
            if(this.index >= this.points.length)
                this.run = false;
            return true;
        }
    }
});
