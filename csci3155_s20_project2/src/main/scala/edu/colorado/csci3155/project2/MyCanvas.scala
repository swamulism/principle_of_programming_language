package edu.colorado.csci3155.project2

/* A class to maintain a canvas object.
*  A canvas is a collection of circles and polygons.
*/

import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{Graphics2D}

/* A figure is a sealed trait. It can be a Polygon or a "MyCircle"*/
sealed trait Figure {
    def getBoundingBox: (Double, Double, Double, Double)
    def translate(shiftX: Double, shiftY: Double): Figure
    def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double): Unit
}

/*
 Class Polygon
   A polygon is defined by a list of its vertices
 */

case class Polygon(val cList: List[(Double, Double)]) extends Figure {
    //TODO: Define the bounding box of the polygon
    //      A bounding box is a 4-tuple (xMin, xMax, yMin, yMax)
    override def getBoundingBox: (Double, Double, Double, Double ) = {
        val xMin = cList.map(elt => elt._1).min
        val xMax = cList.map(elt => elt._1).max
        val yMin = cList.map(elt => elt._2).min
        val yMax = cList.map(elt => elt._2).max
        (xMin, xMax, yMin, yMax)
    }
    //TODO: Create a new polygon by shifting each vertex in cList by (x,y)
    //    Do not change the order in which the vertices appear
    override def translate(shiftX: Double, shiftY: Double): Polygon = {
        Polygon(cList.map((x) => (x._1 + shiftX, x._2 + shiftY)))
    }

    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val xPoints: Array[Int] = new Array[Int](cList.length)
        val yPoints: Array[Int] = new Array[Int](cList.length)
        for (i <- 0 until cList.length){
            xPoints(i) = ((cList(i)._1 + shiftX )* scaleX).toInt
            yPoints(i) = ((cList(i)._2 + shiftY) * scaleY).toInt
        }
        g.drawPolygon(xPoints, yPoints, cList.length)
    }
}

/*
  Class MyCircle
  Define a circle with a given center c and radius r
 */
case class MyCircle(val c: (Double, Double), val r: Double) extends Figure {
    //TODO: Define the bounding box for the circle
    //    A bounding box is a 4-tuple (xMin, xMax, yMin, yMax)
    override def getBoundingBox: (Double, Double, Double, Double) = {
        val xMin = c._1 - r
        val xMax = c._1 + r
        val yMin = c._2 - r
        val yMax = c._2 + r
        (xMin, xMax, yMin, yMax)
    }

    //TODO: Create a new circle by shifting the center
    override def translate(shiftX: Double, shiftY: Double): MyCircle = {
        MyCircle((c._1 + shiftX, c._2 + shiftY), r)
    }

    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val centerX = ((c._1 + shiftX) * scaleX) .toInt
        val centerY = ((c._2 + shiftY) * scaleY) .toInt
        val radX = (r * scaleX).toInt
        val radY = (r * math.abs(scaleY)).toInt
        //g.draw(new Ellipse2D.Double(centerX, centerY, radX, radY))
        g.drawOval(centerX-radX, centerY-radY, 2*radX, 2*radY)
    }
}

/*
  Class : MyCanvas
  Define a canvas through a list of figure objects. Figure objects can be circles or polygons.
 */
class MyCanvas (val listOfObjects: List[Figure]) {
    // TODO: Write a function to get the boundingbox for the entire canvas.
    //     Hint: use existing getBoundingBox functions defined in each figure.
    //     A bounding box is defined as (xMin, xMax, yMin, yMax)
    def getBoundingBox: (Double, Double, Double, Double) = {
        val boxes = listOfObjects.map(x => x.getBoundingBox)
        val xMin = boxes.map(elt => elt._1).min
        val xMax = boxes.map(elt => elt._2).max
        val yMin = boxes.map(elt => elt._3).min
        val yMax = boxes.map(elt => elt._4).max
        (xMin, xMax, yMin, yMax)
    }

    //TODO: Write a function to translate each figure in the canvas by shiftX, shiftY
    def translate(shiftX: Double, shiftY: Double): MyCanvas = {
        new MyCanvas(listOfObjects.map(x => x.translate(shiftX, shiftY)))
    }

    //TODO: Write a function that will return a new MyCanvas object that places
    // all the objects in myc2 to the right of the objects in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeRight(myc2: MyCanvas):MyCanvas = {
        val (xMin1, xMax1, yMin1, yMax1) = getBoundingBox
        val (xMin2, xMax2, yMin2, yMax2) = myc2.getBoundingBox
        val xShift = xMax1 - xMin1
        val yShift = (yMax1 - yMin1) / 2 - (yMax2 - yMin2) / 2
        overlap(myc2.translate(xShift, yShift))
    }

    //TODO: Write a function that will return a new MyCanvas object that places
    // all the figures in myc2 on top of the figures in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeTop(myc2: MyCanvas): MyCanvas = {
        val (xMin1, xMax1, yMin1, yMax1) = getBoundingBox
        val (xMin2, xMax2, yMin2, yMax2) = myc2.getBoundingBox
        val xShift = (xMax1 - xMin1) / 2 - (xMax2 - xMin2) / 2
        val yShift = yMax1 - yMin1
        overlap(myc2.translate(xShift, yShift))
    }

    //TODO: Write a function that will rotate each figure in the canvas using
    // the angle `ang` defined in radians.
    // Suggestion: first write rotation functions for polygon and circle.
    // rotating a polygon is simply rotating each vertex.
    // rotating a circle is simply rotating the center with radius unchanged.
    def rotate(angRad: Double): MyCanvas = ???

    //Function: overlap
    // This function takes a list of objects from this canvas
    // and contatenates with a list of objects from canvas c2.
    // The result is a new MyCanvas object with the concatenated
    // list of objects.
    def overlap(c2: MyCanvas): MyCanvas = {
        new MyCanvas(listOfObjects ++ c2.listOfObjects)
    }


    // Function to draw the canvas. Do not edit.
    def render(g: Graphics2D, xMax: Double, yMax: Double) = {
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox
        val shiftx = -lx1
        val shifty = -uy1
        val scaleX = xMax/(ux1 - lx1  + 1.0)
        val scaleY = yMax/(uy1 - ly1 + 1.0)
        listOfObjects.foreach(f => f.render(g,scaleX, -scaleY, shiftx, shifty))
    }

    // DO NOT EDIT THE CODE BELOW
    override def toString: String = {
        listOfObjects.foldLeft[String] ("") { case (acc, fig) => acc ++ fig.toString }
    }
    // DO NOT EDIT
    def getListOfObjects: List[Figure] = listOfObjects

    // DO NOT EDIT
    def numPolygons: Int =
        listOfObjects.count {
            case Polygon(_) => true
            case _ => false }

    //DO NOT EDIT
    def numCircles: Int = {
        listOfObjects.count {
            case MyCircle(_,_) => true
            case _ => false }
    }
    //DO NOT EDIT
    def numVerticesTotal: Int = {
        listOfObjects.foldLeft[Int](0) ((acc, f) =>
            f match {
                case Polygon(lst1) => acc + lst1.length
                case _ => acc
            }
        )
    }
}
