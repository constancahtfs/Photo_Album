import java.io.File

import BitMap.toListOfLists
import FxApp.{grid1, grid2, grid3, grid4, grid5, grid6, images, imagesPath, importImages, next, pointer, prev}
import Manipulation.{Coords, makeBitMap, makeQTree}
import QTree._
import javafx.fxml.FXML
import javafx.scene.control.Alert.AlertType
import javafx.scene.control.{Alert, Button, Label, TextArea, TextField}
import javafx.scene.image.{Image, ImageView}
import javafx.stage.FileChooser
import javax.imageio.ImageIO

import scala.util.{Failure, Success, Try}

class Controller {

  @FXML
  private var importImgs: Button = _
  @FXML
  private var logo: ImageView = _
  @FXML
  private var img1: ImageView = _
  @FXML
  private var img2: ImageView = _
  @FXML
  private var img3: ImageView = _
  @FXML
  private var imgDescription: TextArea = _
  @FXML
  private var imgSizeLabel: Label = _
  @FXML
  private var heightSizeInput: TextField = _
  @FXML
  private var widthSizeInput: TextField = _
  @FXML
  private var gridImg1: ImageView = _
  @FXML
  private var gridImg2: ImageView = _
  @FXML
  private var gridImg3: ImageView = _
  @FXML
  private var gridImg4: ImageView = _
  @FXML
  private var gridImg5: ImageView = _
  @FXML
  private var gridImg6: ImageView = _


  /**********************************************************************
   *
   *                 Utils
   *
   * *********************************************************************/

  /*
  *   Checks if both paths have the same file name
  * */
  def same(path1: String, path2: String): Boolean = path1.split('\\').last.equals(path2.split('\\').last)

  /*
  *   Checks if filename already exists in list of images
  * */
  def alreadyExists(images: List[ImageInfo], path: String) : Boolean = (images foldRight false) ( (i1,i2) => same(i1.getFile().getAbsolutePath,path) || i2)

  /*
  *    Remove image from list and directory
  * */
  def remove(file: ImageInfo, list: List[ImageInfo]) = list diff List(file)

  def rightSwitch(xs: List[ImageInfo], p: File): List[ImageInfo] = {
    xs match {
      case Nil => List()
      case x :: Nil => List(x)
      case x :: y :: ys =>
        if (x.getFile().getAbsolutePath equals p.getAbsolutePath)
          y :: x :: rightSwitch(ys,p)
        else
          x :: rightSwitch(y :: ys,p)
    }
  }

  def leftSwitch(xs: List[ImageInfo], p: File): List[ImageInfo] = {
    xs match {
      case Nil => List()
      case x :: Nil => List(x)
      case x :: y :: ys =>
        if (x.getFile().getAbsolutePath equals p.getAbsolutePath)
          y :: x :: leftSwitch(ys,p)
        else
          x :: leftSwitch(y :: ys,p)
    }
  }

  /*
  *   Set the given imageview with the image of the given index
  * */
  def setImg(img: ImageView, index: Int): Unit ={

    if(index < FxApp.images.size && index > -1) {
      img.setImage(new Image("file:///" + FxApp.images(index).getFile().getAbsolutePath))
    } else { // if there is no image at the given index
      img.setImage(null)
    }
  }

  /*
  *   Set image input description
  * */
  def setDescription(text: String): Unit ={
    imgDescription.setText("")
    imgDescription.setText(text)
  }

  /*
  *   Update slide show
  * */
  def updateSlideShow(): Unit ={

    setSlideShow(img1, img2, img3)

    // set size label on top of image, if image exists
    imgSizeLabel.setText("")
    if(pointer < FxApp.images.size && pointer > -1) {
      setDescription(FxApp.images(pointer).getDescription())
      imgSizeLabel.setText(FxApp.images(pointer).width + "x" + FxApp.images(pointer).height)
    }
  }

  /*
  *   Set slideshow 3 images
  * */
  def setSlideShow(im1: ImageView, im2: ImageView, im3: ImageView): Unit ={

    // if there is only one image, make it the pointer image
    if(FxApp.images.size == 1){
      prev = -1
      pointer = 0
      next = 1
    }

    setImg(im1, prev)
    setImg(im2, pointer)
    setImg(im3, next)
  }

  /*
  *   Convert list of lists to list of arrays
  * */
  def toListOfArrays(l: List[List[Int]]): List[Array[Int]] = {
    l match{
      case List() => List()
      case (h::t) => h.toArray :: toListOfArrays(t)
    }
  }

  /*
  *   Convert current image to tree (pointer)
  * */
  def pointerToTree(): QTree[Coords] ={
    val imageColors = ImageUtil.readColorImage(FxApp.images(FxApp.pointer).getFile().getAbsolutePath)
    // Convert to list of lists
    val converted = toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    tree
  }

  /*
  *   Display tree as current image (pointer)
  * */
  def treeToPointer(tree: QTree[Coords]): Unit ={
    val bitMap = makeBitMap(tree)
    ImageUtil.writeImage(toListOfArrays(bitMap).toArray, FxApp.images(FxApp.pointer).getFile().getAbsolutePath, "png")
    updateSlideShow()
  }

  /*
  *   Check if string is a numeric value
  * */
  def isInt(text:String):Boolean = Try {
    text.toInt
  } match {
    case Success(x) => true
    case Failure(ex) => false
  }

  /*
  *   Check if image is smaller than the given sizes
  * */
  def isSmallerThan(image: ImageInfo, width: Double, height: Double): Boolean ={
    if(image.width < width && image.height < height) true
    else false
  }

  /*
  *   Update grid view with next/prev 6 images
  * */
  def updateGrid(): Unit ={
    setImg(gridImg1,grid1)
    setImg(gridImg2,grid2)
    setImg(gridImg3,grid3)
    setImg(gridImg4,grid4)
    setImg(gridImg5,grid5)
    setImg(gridImg6,grid6)
  }


  /**********************************************************************
   *
   *                 Buttons Click Events
   *
   * *********************************************************************/

  /************************************************************
   *                 GRID
   * **********************************************************/

  def onGridTabClicked(): Unit = {
    updateGrid()
  }

  /************************************************************
   *                 GRID ARROWS
   * **********************************************************/

  def onPrevGridClicked(): Unit ={
    if(grid6 - 6 < images.size && grid6 - 6 > -1){
      grid1 = grid1 - 6
      grid2 = grid2 - 6
      grid3 = grid3 - 6
      grid4 = grid4 - 6
      grid5 = grid5 - 6
      grid6 = grid6 - 6
      updateGrid()
    }
  }

  def onNextGridClicked(): Unit ={
    if(grid1 + 6 < images.size && grid1 + 6 > -1) {
      grid1 = grid1 + 6
      grid2 = grid2 + 6
      grid3 = grid3 + 6
      grid4 = grid4 + 6
      grid5 = grid5 + 6
      grid6 = grid6 + 6
      updateGrid()
    }
  }

  /************************************************************
   *                 SWITCH IMAGES POSITION
   * **********************************************************/

  def onRightSwitchClicked() = {
    if ((pointer >= 0 && pointer < FxApp.images.size) && (next > 0 && next <= FxApp.images.size)) {
      val file= new File(FxApp.images(pointer).getFile().getAbsolutePath());
      FxApp.images = rightSwitch(FxApp.images, file)
      updateSlideShow()
    }
  }

  def onLeftSwitchClicked() = {
    if ((pointer > 0 && pointer <= FxApp.images.size) && (prev >= 0 && prev < FxApp.images.size)) {
      val file= new File(FxApp.images(prev).getFile().getAbsolutePath());
      FxApp.images = leftSwitch(FxApp.images, file)
      updateSlideShow()
    }
  }

  /************************************************************
   *                 DELETE IMAGE
   * **********************************************************/

  def onDeleteClicked() = {

    if(FxApp.images.size != 0) {
      val image = FxApp.images(pointer)
      FxApp.images = remove(image, FxApp.images)
      image.getFile().delete()

      if (pointer >= FxApp.images.size) {
        pointer = pointer - 1
        next = next - 1
        prev = prev - 1
      }

      updateSlideShow()
    }
  }

  /************************************************************
   *                 SLIDESHOW ARROWS
   * **********************************************************/

  def onPrevClicked() = {
    if(FxApp.prev < FxApp.images.size && FxApp.prev > -1){
      FxApp.pointer = FxApp.pointer - 1;
      FxApp.prev = FxApp.prev - 1
      FxApp.next = FxApp.next - 1
      updateSlideShow()
    }
  }

  def onNextClicked() = {
    if(FxApp.next < FxApp.images.size && FxApp.next > -1) {
      FxApp.pointer = FxApp.pointer + 1;
      FxApp.prev = FxApp.prev + 1
      FxApp.next = FxApp.next + 1
      updateSlideShow()
    }
  }

  /************************************************************
   *                 SET IMAGE DESCRIPTION
   * **********************************************************/

  def onUpdateDescriptionClicked() = {
    val text = imgDescription.getText()
    FxApp.images(pointer).setDescription(text)
  }

  /************************************************************
   *                 IMAGE EFFECTS
   * **********************************************************/

  def onRotateImgRightClicked() = {
    val rotatedImg = rotate90DegreesRight(pointerToTree())
    treeToPointer(rotatedImg)
  }

  def onRotateImgLeftClicked() = {
    val rotatedImg = rotate90DegreesLeft(pointerToTree())
    treeToPointer(rotatedImg)
  }

  def onInvertImgHClicked() = {
    val invertedImg = mirrorH(pointerToTree())
    treeToPointer(invertedImg)
  }

  def onInvertImgVClicked() = {
    val invertedImg = mirrorV(pointerToTree())
    treeToPointer(invertedImg)
  }

  def onScaleUpImgClicked() = {
    val scaledImg = scale(2,pointerToTree())
    val bitMap = makeBitMap(scaledImg)
    ImageUtil.writeImage(toListOfArrays(bitMap).toArray, FxApp.images(FxApp.pointer).getFile().getAbsolutePath, "png")

    var image = new Image("file:///" + FxApp.images(FxApp.pointer).getFile().getAbsolutePath)

    FxApp.images(FxApp.pointer).height = image.getHeight()
    FxApp.images(FxApp.pointer).width = image.getWidth()

    imgSizeLabel.setText(FxApp.images(pointer).width + "x" + FxApp.images(pointer).height)

    updateSlideShow()
  }

  def onNoiseImgClicked() = {
    val noiseTree = mapColourEffect(noise,pointerToTree())
    treeToPointer(noiseTree)
  }

  def onNoisePureImgClicked() = {
    val noiseTree = mapNoisePure(pointerToTree(),10)
    treeToPointer(noiseTree._1)
  }

  def onSepiaImgClicked() = {
    val sepiaTree = mapColourEffect(sepia,pointerToTree())
    treeToPointer(sepiaTree)
  }

  def onConstrastImgClicked() = {
    val contrastTree = mapColourEffect(contrast,pointerToTree())
    treeToPointer(contrastTree)
  }

  /************************************************************
   *                 FILTER IMAGES
   * **********************************************************/

  def onSizeSmallerThanClicked() = {

    importImages() // Reset the list to its original images

    if(widthSizeInput.getText() != "" && heightSizeInput.getText() != ""){ // if user filled inputs

      if(isInt(widthSizeInput.getText()) && isInt(heightSizeInput.getText())){ // if input texts are numeric values

        // Go to the beggining of the slideshow
        pointer = 1
        prev = 0
        next = 2

        // Filter images smaller than the given input size
        FxApp.images = FxApp.images.filter(isSmallerThan(_,widthSizeInput.getText().toDouble,heightSizeInput.getText().toDouble))

        // Show updated list on the photo album
        updateSlideShow()

      }
      else{

        var a = new Alert(AlertType.WARNING);
        a.setContentText("Insira um tamanho numérico válido")
        a.show()

      }

    }
    else{

      var a = new Alert(AlertType.WARNING);
      a.setContentText("Insira o tamanho máximo pretendido")
      a.show()

    }

  }

  /************************************************************
   *                 IMPORT IMAGES
   * **********************************************************/

  def onImportClicked(): Unit = {

    // Open file chooser to choose the image
    var chooser = new FileChooser();
    chooser.setTitle("Importar Imagem");
    var file = chooser.showOpenDialog(importImgs.getScene().getWindow());

    if(file != null) { // if Cancel button was not clicked (file choosed)

      var path = file.getAbsolutePath()
      var extension = path.split('.').last // file extension

      if (extension == "png" || extension == "jpg" || extension == "gif") { // must be an image

        if (!alreadyExists(FxApp.images, path)) { // if that file name does not exist in the photo album

          var in = ImageIO.read(file)                       // the original image
          var fileName = path.split('\\').last              // image name and extension
          var outputfile = new File(imagesPath + fileName)  // where the image is going to
          ImageIO.write(in, "png", outputfile)  // "write" file to the output directory (src/Images)

          // Add new image to the list of images
          var image = new Image("file:///" + file.getAbsolutePath)
          FxApp.images = FxApp.images ::: List(new ImageInfo(outputfile, "", image.getWidth(), image.getHeight()))


          // Show updated list on the photo album
          updateSlideShow()

        }
        else { // file name already exists in the photo album

          var a = new Alert(AlertType.WARNING);
          a.setContentText("Já existe um ficheiro com esse nome")
          a.show()

        }
      }
      else { // it is not an image

        var a = new Alert(AlertType.WARNING)
        a.setContentText("O ficheiro tem de ser uma imagem")
        a.show()

      }
    }

  }

}
