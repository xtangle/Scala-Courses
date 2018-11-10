package scalashop

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

@RunWith(classOf[JUnitRunner])
class BlurSuite extends FunSuite {

  trait BlurTesting {
    val image1 = new Img(3, 3)
    image1(0, 0) = 0; image1(1, 0) = 1; image1(2, 0) = 2
    image1(0, 1) = 3; image1(1, 1) = 4; image1(2, 1) = 5
    image1(0, 2) = 6; image1(1, 2) = 7; image1(2, 2) = 8

    val image2 = new Img(4, 3)
    image2(0, 0) = 0; image2(1, 0) = 1; image2(2, 0) = 2; image2(3, 0) = 9
    image2(0, 1) = 3; image2(1, 1) = 4; image2(2, 1) = 5; image2(3, 1) = 10
    image2(0, 2) = 6; image2(1, 2) = 7; image2(2, 2) = 8; image2(3, 2) = 11

    def check(dst: Img)(x: Int, y: Int, expected: Int): Unit =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    def checkImage1(dst: Img): Unit = {
      def checkImg = check(dst)_
      checkImg(0, 0, 2)
      checkImg(1, 0, 2)
      checkImg(2, 0, 3)
      checkImg(0, 1, 3)
      checkImg(1, 1, 4)
      checkImg(2, 1, 4)
      checkImg(0, 2, 5)
      checkImg(1, 2, 5)
      checkImg(2, 2, 6)
    }

    def checkImage2(dst: Img): Unit = {
      def checkImg = check(dst)_
      checkImg(0, 0, 4)
      checkImg(1, 0, 5)
      checkImg(2, 0, 5)
      checkImg(3, 0, 6)
      checkImg(0, 1, 4)
      checkImg(1, 1, 5)
      checkImg(2, 1, 5)
      checkImg(3, 1, 6)
      checkImg(0, 2, 4)
      checkImg(1, 2, 5)
      checkImg(2, 2, 5)
      checkImg(3, 2, 6)
    }
  }

  test("boxBlurKernel should correctly handle radius 0") {
    val src = new Img(5, 5)

    for (x <- 0 until 5; y <- 0 until 5)
      src(x, y) = rgba(x, y, x + y, math.abs(x - y))

    for (x <- 0 until 5; y <- 0 until 5)
      assert(boxBlurKernel(src, x, y, 0) === rgba(x, y, x + y, math.abs(x - y)),
        "boxBlurKernel(_,_,0) should be identity.")
  }

  test("boxBlurKernel should return the correct value on an interior pixel " +
    "of a 3x4 image with radius 1") {
    val src = new Img(3, 4)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
    src(0, 3) = 50; src(1, 3) = 11; src(2, 3) = 16

    assert(boxBlurKernel(src, 1, 2, 1) === 12,
      s"(boxBlurKernel(1, 2, 1) should be 12, " +
        s"but it's ${boxBlurKernel(src, 1, 2, 1)})")
  }

  test("boxBlurKernel should return the correct value on an exterior pixel " +
    "of a 3x4 image with radius 1") {
    val src = new Img(3, 4)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = rgba(1, 2, 3, 4); src(2, 2) = rgba(5, 6, 7, 8)
    src(0, 3) = 50; src(1, 3) = rgba(9, 10, 11, 12); src(2, 3) = rgba(13, 14, 15, 16)

    val actual = boxBlurKernel(src, 2, 3, 1)
    assert(actual === rgba(7, 8, 9, 10),
      s"(boxBlurKernel(2, 3, 1) should be rgba(7, 8, 9, 10), " +
        s"but it's rgba(${red(actual)}, ${green(actual)}, ${blue(actual)}, ${alpha(actual)})")
  }

  test("HorizontalBoxBlur.blur with radius 1 should correctly blur the entire 3x3 image") {
    new BlurTesting {
      val src: Img = image1
      val dst: Img = new Img(src.width, src.height)
      HorizontalBoxBlur.blur(src, dst, 0, 3, 1)
      checkImage1(dst)
    }
  }

  test("HorizontalBoxBlur.parBlur with numTasks 2 and radius 1 should correctly " +
    "blur the entire 3x3 image") {
    new BlurTesting {
      val src: Img = image1
      val dst: Img = new Img(src.width, src.height)
      HorizontalBoxBlur.parBlur(src, dst, 2, 1)
      checkImage1(dst)
    }
  }

  test("HorizontalBoxBlur.parBlur with numTasks 32 and radius 1 should correctly " +
    "blur the entire 3x3 image") {
    new BlurTesting {
      val src: Img = image1
      val dst: Img = new Img(src.width, src.height)
      HorizontalBoxBlur.parBlur(src, dst, 32, 1)
      checkImage1(dst)
    }
  }

  test("VerticalBoxBlur.blur with radius 2 should correctly blur the entire " +
    "4x3 image") {
    new BlurTesting {
      val src: Img = image2
      val dst: Img = new Img(src.width, src.height)
      VerticalBoxBlur.blur(src, dst, 0, 4, 2)
      checkImage2(dst)
    }
  }

  test("VerticalBoxBlur.parBlur with numTasks 2 and radius 2 should correctly " +
    "blur the entire 4x3 image") {
    new BlurTesting {
      val src: Img = image2
      val dst: Img = new Img(src.width, src.height)
      VerticalBoxBlur.parBlur(src, dst, 2, 2)
      checkImage2(dst)
    }
  }

  test("VerticalBoxBlur.parBlur with numTasks 32 and radius 2 should correctly " +
    "blur the entire 4x3 image") {
    new BlurTesting {
      val src: Img = image2
      val dst: Img = new Img(src.width, src.height)
      VerticalBoxBlur.parBlur(src, dst, 32, 2)
      checkImage2(dst)
    }
  }

}
