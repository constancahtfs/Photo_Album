import java.io.File

import Manipulation.{makeBitMap, makeQTree}
import QTree.{contrast, mapColourEffect, mirrorH, mirrorV, noise, rotate90DegreesLeft, rotate90DegreesRight, scale, sepia}
import javafx.fxml.FXML
import javafx.scene.control.Alert.AlertType
import javafx.scene.control.{Alert, Button}
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
  private var scaleImgButton: Button = _
  @FXML
  private var sepiaImgButton: Button = _
  @FXML
  private var noiseImgButton: Button = _
  @FXML
  private var contrastImgButton: Button = _

  private var images = List() : List[File]
  private var pointer = -1
  private var prev = -1
  private var next = -1

  def importImages() = {
    val folderPath = getClass.getClassLoader.getResource("").getPath + "Images/"
    //var folder = new File("C:/Users/const/IdeaProjects/Photo_Album/src/Images/");
    var folder = new File(folderPath);

    var listOfFiles = folder.listFiles().toList

    images = listOfFiles

  }

  def same(path1: String, path2: String): Boolean = path1.split('\\').last.equals(path2.split('\\').last)

  def alreadyExists(images: List[File], path: String) : Boolean = (images foldRight false) ( (i1,i2) => same(i1.getAbsolutePath,path) || i2)


  def remove(file: File, list: List[File]) = list diff List(file)

  def onDeleteClicked() = {
    var file= new File(images(pointer).getAbsolutePath());
    images = remove(file,images)
    file.delete()
    updateSlideShow()
  }

  def onPrevClicked() = {
    if(pointer != 0){
      pointer = pointer - 1;
      prev = prev - 1
      next = next - 1
      updateSlideShow()
    }
  }

  def onNextClicked() = {
    if(pointer != images.size - 1) {
      pointer = pointer + 1;
      prev = prev + 1
      next = next + 1
      updateSlideShow()
    }
  }



  def toListOfArrays(l: List[List[Int]]): List[Array[Int]] = {
    l match{
      case List() => List()
      case (h::t) => h.toArray :: toListOfArrays(t)
    }
  }


  def onRotateImgRightClicked() = {
    // Get color for each pixel

    val imageColors = ImageUtil.readColorImage(images(pointer).getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    val rotatedImg = rotate90DegreesRight(tree)
    val bitMap2 = makeBitMap(rotatedImg)
    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, images(pointer).getAbsolutePath, "png")
    updateSlideShow()
  }

  def onRotateImgLeftClicked() = {
    // Get color for each pixel

    val imageColors = ImageUtil.readColorImage(images(pointer).getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    val rotatedImg = rotate90DegreesLeft(tree)
    val bitMap2 = makeBitMap(rotatedImg)
    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, images(pointer).getAbsolutePath, "png")
    updateSlideShow()
  }

  def onInvertImgHClicked() = {

    val imageColors = ImageUtil.readColorImage(images(pointer).getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    val invertedImg = mirrorH(tree)
    val bitMap2 = makeBitMap(invertedImg)
    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, images(pointer).getAbsolutePath, "png")
    updateSlideShow()
  }

  def onInvertImgVClicked() = {

    val imageColors = ImageUtil.readColorImage(images(pointer).getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    val invertedImg = mirrorV(tree)
    val bitMap2 = makeBitMap(invertedImg)
    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, images(pointer).getAbsolutePath, "png")
    updateSlideShow()
  }

  def onScaleImgClicked() = {

    val imageColors = ImageUtil.readColorImage(images(pointer).getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    val scaledImg = scale(2,tree)
    val bitMap2 = makeBitMap(scaledImg)
    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, images(pointer).getAbsolutePath, "png")
    updateSlideShow()
  }

  def onNoiseImgClicked() = {

    val imageColors = ImageUtil.readColorImage(images(pointer).getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    val noiseTree = mapColourEffect(noise,tree)
    val bitMap2 = makeBitMap(noiseTree)
    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, images(pointer).getAbsolutePath, "png")
    updateSlideShow()


  }

  def onSepiaImgClicked() = {

    val imageColors = ImageUtil.readColorImage(images(pointer).getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    val noiseTree = mapColourEffect(sepia,tree)
    val bitMap2 = makeBitMap(noiseTree)
    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, images(pointer).getAbsolutePath, "png")
    updateSlideShow()


  }

  def onConstrastImgClicked() = {

    val imageColors = ImageUtil.readColorImage(images(pointer).getAbsolutePath)
    // Convert to list of lists
    val converted = Utils.toListOfLists(imageColors.toList)
    // Create bitmap out of the list of lists
    val bitMap = BitMap(converted)
    // Create a tree out of the bitmap
    val tree = makeQTree(bitMap);
    val noiseTree = mapColourEffect(contrast,tree)
    val bitMap2 = makeBitMap(noiseTree)
    ImageUtil.writeImage(toListOfArrays(bitMap2).toArray, images(pointer).getAbsolutePath, "png")
    updateSlideShow()


  }


  def updateSlideShow(): Unit ={
    if(prev >= 0 && prev < images.size)
      img1.setImage(new Image("file:///" + images(prev).getAbsolutePath))
    else
      img1.setImage(null);

    if(pointer >= 0 && pointer < images.size)
      img2.setImage(new Image("file:///" + images(pointer).getAbsolutePath))
    else
      img2.setImage(null);

    if(next >= 0 && next < images.size)
      img3.setImage(new Image("file:///" + images(next).getAbsolutePath))
    else
      img3.setImage(null);

  }


  def onImportClicked(): Unit = {
    importImages()

    var chooser = new FileChooser();
    chooser.setTitle("Importar Imagem");
    var file = chooser.showOpenDialog(importImgs.getScene().getWindow());
    var path = file.getAbsolutePath()



    if(!alreadyExists(images, path)){
      // Save in directory
      var in = ImageIO.read(file)
      var fileName = path.split('\\').last


      val folderPath = getClass.getClassLoader.getResource("").getPath + "Images/"
      //var outputfile = new File("C:/Users/const/IdeaProjects/Photo_Album/src/Images/" + fileName)
      var outputfile = new File(folderPath + fileName)
      ImageIO.write(in, "png", outputfile)


      images = images ::: List(outputfile)
    }
    else {
      var a = new Alert(AlertType.WARNING);
      a.setContentText("Já existe um ficheiro com esse nome")
      a.show();
    }


    if(images.size == 1) {
      pointer = 0
      next = -1
      prev = -1

    } else
      if(images.size == 2) {
        pointer = 1
        prev = 0
        next = -1
      }
      else {
        pointer = images.size - 2
        prev = images.size - 3
        next = images.size - 1

      }


    updateSlideShow()


  }

}
