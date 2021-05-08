import java.io.File

import FxApp.{next, pointer, prev}
import Manipulation.{makeBitMap, makeQTree}
import QTree._
import javafx.fxml.FXML
import javafx.scene.control.Alert.AlertType
import javafx.scene.control.{Alert, Button, TextArea}
import javafx.scene.image.{Image, ImageView}
import javafx.stage.FileChooser
import javax.imageio.ImageIO

class Controller {

  @FXML
  private var importImgs: Button = _
  @FXML
  private var img1: ImageView = _
  @FXML
  private var img2: ImageView = _
  @FXML
  private var img3: ImageView = _
  @FXML
  private var imgDescription: TextArea = _
  @FXML
  private var prevButton: Button = _
  @FXML
  private var nextButton: Button = _
  @FXML
  private var deleteButton: Button = _
  @FXML
  private var rotateImgRightButton: Button = _
  @FXML
  private var rotateImgLeftRightButton: Button = _
  @FXML
  private var invertImgHButton: Button = _
  @FXML
  private var invertImgVButton: Button = _
  @FXML
  private var scaleUpImgButton: Button = _
  @FXML
  private var scaleDownImgButton: Button = _
  @FXML
  private var sepiaImgButton: Button = _
  @FXML
  private var noiseImgButton: Button = _
  @FXML
  private var contrastImgButton: Button = _


  def same(path1: String, path2: String): Boolean = path1.split('\\').last.equals(path2.split('\\').last)

  def alreadyExists(images: List[ImageInfo], path: String) : Boolean = (images foldRight false) ( (i1,i2) => same(i1.getFile().getAbsolutePath,path) || i2)


  def remove(file: ImageInfo, list: List[ImageInfo]) = list diff List(file)

  //------------------------------------
  def onRightSwitchClicked() = {
    if ((pointer >= 0 && pointer < FxApp.images.size) && (next > 0 && next <= FxApp.images.size)) {
      val file= new File(FxApp.images(pointer).getFile().getAbsolutePath());
      FxApp.images = rightSwitch(FxApp.images, file)
      updateSlideShow()
    }
  }

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

  def onLeftSwitchClicked() = {
    if ((pointer > 0 && pointer <= FxApp.images.size) && (prev >= 0 && prev < FxApp.images.size)) {
      val file= new File(FxApp.images(prev).getFile().getAbsolutePath());
      FxApp.images = leftSwitch(FxApp.images, file)
      updateSlideShow()
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


//------------------------------------
  def onDeleteClicked() = {
    val image = FxApp.images(pointer)
    FxApp.images = remove(image,FxApp.images)
    updateSlideShow()
  }

  def onPrevClicked() = {
    if(FxApp.prev - 1 < FxApp.images.size && FxApp.prev - 1 > -1){
      FxApp.pointer = FxApp.pointer - 1;
      FxApp.prev = FxApp.prev - 1
      FxApp.next = FxApp.next - 1
      updateSlideShow()
    }
  }

  def onNextClicked() = {
    if(FxApp.next - 1 < FxApp.images.size && FxApp.next - 1 > -1) {
      FxApp.pointer = FxApp.pointer + 1;
      FxApp.prev = FxApp.prev + 1
      FxApp.next = FxApp.next + 1
      updateSlideShow()
    }
  }

  def setImg(img: ImageView, index: Int): Unit ={

    if(index < FxApp.images.size && index > -1) {
      img.setImage(new Image("file:///" + FxApp.images(index).getFile().getAbsolutePath))
    } else {
      img.setImage(null)
    }
  }

  def setDescription(text: String): Unit ={
    imgDescription.setText("")
    imgDescription.setText(text)
  }


  def updateSlideShow(): Unit ={

    setImg(img1, prev)
    setImg(img2, pointer)
    setImg(img3, next)

    println(FxApp.images(pointer).getDescription())
    setDescription(FxApp.images(pointer).getDescription())

  }

  def setSavedImages(im1: ImageView, im2: ImageView, im3: ImageView): Unit ={

    setImg(im1, prev)
    setImg(im2, pointer)
    setImg(im3, next)

  }

  def toListOfArrays(l: List[List[Int]]): List[Array[Int]] = {
    l match{
      case List() => List()
      case (h::t) => h.toArray :: toListOfArrays(t)
    }
  }


  def onUpdateDescriptionClicked() = {
    // Get color for each pixel

    val text = imgDescription.getText()
    FxApp.images(pointer).setDescription(text)
  }

  def onRotateImgRightClicked() = {
    // Get color for each pixel

    val imageColors = ImageUtil.readColorImage(FxApp.images(FxApp.pointer).getFile().getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    val rotatedImg = rotate90DegreesRight(tree)
    val bitMap2 = makeBitMap(rotatedImg)
    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, FxApp.images(FxApp.pointer).getFile().getAbsolutePath, "png")
    updateSlideShow()
  }

  def onRotateImgLeftClicked() = {
    // Get color for each pixel

    val imageColors = ImageUtil.readColorImage(FxApp.images(FxApp.pointer).getFile().getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    val rotatedImg = rotate90DegreesLeft(tree)
    val bitMap2 = makeBitMap(rotatedImg)
    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, FxApp.images(FxApp.pointer).getFile().getAbsolutePath, "png")
    updateSlideShow()
  }

  def onInvertImgHClicked() = {

    val imageColors = ImageUtil.readColorImage(FxApp.images(FxApp.pointer).getFile().getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    val invertedImg = mirrorH(tree)
    val bitMap2 = makeBitMap(invertedImg)
    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, FxApp.images(FxApp.pointer).getFile().getAbsolutePath, "png")
    updateSlideShow()
  }

  def onInvertImgVClicked() = {

    val imageColors = ImageUtil.readColorImage(FxApp.images(FxApp.pointer).getFile().getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    val invertedImg = mirrorV(tree)
    val bitMap2 = makeBitMap(invertedImg)
    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, FxApp.images(FxApp.pointer).getFile().getAbsolutePath, "png")
    updateSlideShow()
  }

  def onScaleUpImgClicked() = {

    val imageColors = ImageUtil.readColorImage(FxApp.images(FxApp.pointer).getFile().getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    val scaledImg = scale(2,tree)
    val bitMap2 = makeBitMap(scaledImg)
    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, FxApp.images(FxApp.pointer).getFile().getAbsolutePath, "png")
    updateSlideShow()
  }

  def onScaleDownImgClicked() = {

    val imageColors = ImageUtil.readColorImage(FxApp.images(FxApp.pointer).getFile().getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);

    println("--- um")
    val treeScaleDown = scale(0.5,tree)
    println("--- dois")
    val bitMap2 = makeBitMap(treeScaleDown)
    println("--- tres")

    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, FxApp.images(FxApp.pointer).getFile().getAbsolutePath, "png")
    println("--- quatro")
    updateSlideShow()
  }

  def onNoiseImgClicked() = {

    val imageColors = ImageUtil.readColorImage(FxApp.images(FxApp.pointer).getFile().getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    val noiseTree = mapColourEffect(noise,tree)
    //val noiseTree = mapNoisePure(tree, 10)
    val bitMap2 = makeBitMap(noiseTree)
    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, FxApp.images(FxApp.pointer).getFile().getAbsolutePath, "png")
    updateSlideShow()


  }

  def onSepiaImgClicked() = {

    val imageColors = ImageUtil.readColorImage(FxApp.images(FxApp.pointer).getFile().getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    val noiseTree = mapColourEffect(sepia,tree)
    val bitMap2 = makeBitMap(noiseTree)
    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, FxApp.images(FxApp.pointer).getFile().getAbsolutePath, "png")
    updateSlideShow()


  }

  def onConstrastImgClicked() = {

    val imageColors = ImageUtil.readColorImage(FxApp.images(FxApp.pointer).getFile().getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    val noiseTree = mapColourEffect(contrast,tree)
    val bitMap2 = makeBitMap(noiseTree)
    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, FxApp.images(FxApp.pointer).getFile().getAbsolutePath, "png")
    updateSlideShow()


  }





  def onImportClicked(): Unit = {
    //FxApp.importImages()

    var chooser = new FileChooser();
    chooser.setTitle("Importar Imagem");
    var file = chooser.showOpenDialog(importImgs.getScene().getWindow());
    var path = file.getAbsolutePath()



    if(!alreadyExists(FxApp.images, path)){
      // Save in directory
      var in = ImageIO.read(file)
      var fileName = path.split('\\').last


      val folderPath = getClass.getClassLoader.getResource("").getPath + "Images/"
      //var outputfile = new File("C:/Users/const/IdeaProjects/Photo_Album/src/Images/" + fileName)
      var outputfile = new File(folderPath + fileName)
      ImageIO.write(in, "png", outputfile)

      FxApp.images = FxApp.images ::: List(new ImageInfo(outputfile,""))
    }
    else {
      var a = new Alert(AlertType.WARNING);
      a.setContentText("Já existe um ficheiro com esse nome")
      a.show();
    }

    updateSlideShow()


  }

}
