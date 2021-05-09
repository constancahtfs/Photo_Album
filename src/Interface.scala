import java.io.File

import FxApp.importImages
import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.image.{Image, ImageView}
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
//import javax.swing.text.html.ImageView

class ImageInfo(var file:File, var description:String, var width: Double, var height: Double){

  def getFile(): File ={
    file
  }

  def getDescription(): String ={
    description
  }

  def setDescription(text: String)= {
    description = text
  }

  def getWidth()= {
    width
  }

  def getHeight()= {
    height
  }

  def setSize(w: Double, h: Double)= {
    width = w
    height = h
  }
}


class Interface extends Application {

  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Photo Album")
    val fxmlLoader =
      new FXMLLoader(getClass.getResource("controller.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)
    primaryStage.setResizable (false);
    primaryStage.setScene(scene)
    primaryStage.show()


    importImages()


    var im1 : ImageView = scene.lookup("#img1").asInstanceOf[ImageView]
    var im2 : ImageView = scene.lookup("#img2").asInstanceOf[ImageView]
    var im3 : ImageView = scene.lookup("#img3").asInstanceOf[ImageView]

    new Controller().setSlideShow(im1,im2,im3)



  }
}
object FxApp {

  var images = List() : List[ImageInfo]
  var pointer = 1
  var prev = 0
  var next = 2
  val imagesPath = getClass.getClassLoader.getResource("").getPath + "Images/"

  var grid1 = 0
  var grid2 = 1
  var grid3 = 2
  var grid4 = 3
  var grid5 = 4
  var grid6 = 5

  def importImages() = {

    images = List()

    var folder = new File(imagesPath)
    var listOfFiles = folder.listFiles().toList



    for (file <- listOfFiles) {
      var image = new Image("file:///" + file.getAbsolutePath)
      images = images ::: List(new ImageInfo(file,"", image.getWidth(), image.getHeight()))
    }
    println(FxApp.images)

  }



  def main(args: Array[String]): Unit = {

    Application.launch(classOf[Interface], args: _*)

  }

}
