import java.io.File

import FxApp.importImages
import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.image.ImageView
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
//import javax.swing.text.html.ImageView

class ImageInfo(var file:File, var description:String){

  def getFile(): File ={
    file
  }

  def getDescription(): String ={
    description
  }

  def setDescription(text: String)= {
    description = text
  }
}


class Interface extends Application {

  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Photo Album")
    val fxmlLoader =
      new FXMLLoader(getClass.getResource("controller.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)

    primaryStage.setScene(scene)
    primaryStage.show()


    importImages()


    var im1 : ImageView = scene.lookup("#img1").asInstanceOf[ImageView]
    var im2 : ImageView = scene.lookup("#img2").asInstanceOf[ImageView]
    var im3 : ImageView = scene.lookup("#img3").asInstanceOf[ImageView]

    new Controller().setSavedImages(im1,im2,im3)



  }
}
object FxApp {

  var images = List() : List[ImageInfo]
  var pointer = 1
  var prev = 0
  var next = 2

  def importImages() = {
    val folderPath = getClass.getClassLoader.getResource("").getPath + "Images/"
    var folder = new File(folderPath)
    var listOfFiles = folder.listFiles().toList

    for (file <- listOfFiles)
      images = images ::: List(new ImageInfo(file,""))


  }



  def main(args: Array[String]): Unit = {

    Application.launch(classOf[Interface], args: _*)

  }

}
