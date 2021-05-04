import javafx.application.Application
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.image.ImageView
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

class Interface extends Application {
  @FXML
  private var img: ImageView = _

  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Photo Album")
    val fxmlLoader =
      new FXMLLoader(getClass.getResource("controller.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)


    primaryStage.setScene(scene)
    primaryStage.show()


  }
}
object FxApp {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[Interface], args: _*)
  }

}
